#lang racket

(require (for-syntax syntax/parse))


;; ============================================================================
;; XORM DSL
;;
;; Macros are the only abstraction; the machine underneath stays small.
;;
;; State
;;   R0, R1   two 8-bit registers, the only state a program names directly
;;   carry    a flag, written by ADD and SHR, readable via `carry->r1`
;;   temp     a one-byte scratch slot used by `swap`, not otherwise addressable
;;
;; Instructions
;;   ⊕                    R0 ← R0 ⊕ R1
;;   AND, OR              R0 ← R0 ∧ R1, R0 ← R0 ∨ R1
;;   ADD                  R0 ← R0 + R1 + carry, carry ← overflow
;;   SHR                  carry ← bit 0 of R0, then R0 ← R0 >> 1
;;   (← v)                R1 ← v, where v is a byte or a register name
;;   (set-carry c)        carry ← c, for c in {0, 1}
;;   carry->r1            R1 ← carry
;;   store-r1             temp ← R1
;;   load-r0-from-temp    R0 ← temp
;;
;; Everything else in this file is a macro that expands to those.  Programs are
;; collected in `xorm-program` in emission order.
;; ============================================================================


;; the XORM program in emission order (first emitted, first executed)
(define xorm-program '())

;; Export DSL constructs
(provide
  xorm-program emit run-xorm
  xor ← set-r0 seq swap clear-r0 clear-r1 inc-r0 dec-r0
  copy-to-r1 not-r0 and-r0-r1 or-r0-r1 add-r0-r1
  shift-left-r0 shift-right-r0 << >>
  set-carry clear-carry store-carry-in-r1
  reset-program!)

;; reset the recorded program
(define (reset-program!)
  (set! xorm-program '()))

;; ---------------------------------------------------------------------------
;; The instruction set
;;
;; One definition of what a XORM instruction is, consulted by both `emit` and
;; `run-xorm`.  Previously the two disagreed: `emit` checked only `←` forms and
;; let anything else through, so `(emit 'BOGUS)` was accepted and only blew up
;; later at run time; and `run-xorm` silently masked an out-of-range constant
;; that `emit` would have rejected outright.  A program was therefore neither
;; guaranteed runnable because it was built, nor guaranteed valid because it
;; ran.
;; ---------------------------------------------------------------------------

;; Instructions taking no operand.
(define nullary-instructions
  '(⊕ AND OR ADD SHR carry->r1 store-r1 load-r0-from-temp))

(define (byte? v) (and (exact-integer? v) (<= 0 v 255)))
(define (register? v) (or (eq? v 'R0) (eq? v 'R1)))

(define (valid-instruction? inst)
  (cond
    [(symbol? inst) (and (memq inst nullary-instructions) #t)]
    [(and (list? inst) (= (length inst) 2))
     (define op (first inst))
     (define val (second inst))
     (cond
       [(eq? op '←) (or (register? val) (byte? val))]
       [(eq? op 'set-carry) (or (equal? val 0) (equal? val 1))]
       [else #f])]
    [else #f]))

;; Raise a descriptive error for anything `valid-instruction?` rejects.  `who`
;; names the caller so the message points at `emit` or `run-xorm` as
;; appropriate.
(define (check-instruction who inst)
  (unless (valid-instruction? inst)
    (cond
      [(and (list? inst) (= (length inst) 2) (eq? (first inst) '←))
       (define val (second inst))
       (if (exact-integer? val)
           (error who (format "← constant ~a out of range 0..255" val))
           (error who
                  (format "← expected a register reference or integer constant, got ~a"
                          val)))]
      [(and (list? inst) (= (length inst) 2) (eq? (first inst) 'set-carry))
       (error who (format "set-carry expected 0 or 1, got ~a" (second inst)))]
      [else
       (error who (format "unknown instruction: ~v" inst))]))
  inst)

;; Append an instruction to the program.
;;
;; The program is kept in emission order: the first instruction emitted is the
;; first executed, and that is the order every consumer -- `run-xorm`,
;; `decompile-xorm`, the tests -- reads it in.  Appending is O(n) per
;; instruction rather than O(1), but XORM programs are tens of instructions
;; long, and one unambiguous ordering is worth far more here than the
;; asymptotics.
(define (emit inst)
  (check-instruction 'emit inst)
  (set! xorm-program (append xorm-program (list inst))))

;; Run a XORM program given in emission order -- the order `emit` stores it in,
;; and the same order `decompile-xorm` accepts.
;;
;; The machine starts zeroed unless an initial state is supplied.  The keyword
;; arguments exist so a caller can drive a program from every point in the
;; state space rather than only from the origin; tests/exhaustive-tests.rkt
;; uses them to check each macro against a reference model over all 8-bit
;; inputs.  Initial values are masked and normalised exactly like computed
;; ones, so no program can be started from a state it could not reach.
(define (run-xorm prog #:r0 [r0-init 0] #:r1 [r1-init 0] #:carry [carry-init 0])

  (define (mask-byte v)
    (bitwise-and v #xFF))

  (define R0 (mask-byte r0-init))
  (define R1 (mask-byte r1-init))
  (define temp 0)
  (define carry (if (equal? carry-init 0) 0 1))

  ;; Reject the whole program before executing any of it, using the same
  ;; definition `emit` enforces.  A hand-written program now fails the same way
  ;; a macro-built one would, instead of being quietly masked into range.
  (for-each (lambda (inst) (check-instruction 'run-xorm inst)) prog)

  (for-each (lambda (inst)
              (cond
                [(eq? inst '⊕)
                 (set! R0 (mask-byte (bitwise-xor R0 R1)))]
                [(eq? inst 'AND)
                 (set! R0 (mask-byte (bitwise-and R0 R1)))]
                [(eq? inst 'OR)
                 (set! R0 (mask-byte (bitwise-ior R0 R1)))]
                [(eq? inst 'ADD)
                 (define sum (+ R0 R1 carry))
                 (set! R0 (mask-byte sum))
                 (set! carry (if (> sum 255) 1 0))]
                [(eq? inst 'SHR)
                 ;; Logical right shift.  The bit shifted out lands in the
                 ;; carry, mirroring the way ADD deposits its overflow there.
                 (set! carry (bitwise-and R0 1))
                 (set! R0 (arithmetic-shift R0 -1))]
                [(eq? inst 'carry->r1)
                 (set! R1 carry)]
                [(eq? inst 'store-r1)
                 (set! temp (mask-byte R1))]
                [(eq? inst 'load-r0-from-temp)
                 (set! R0 (mask-byte temp))]
                [(and (list? inst)
                      (equal? (first inst) 'set-carry))
                 (define c (second inst))
                 (set! carry (if (equal? c 0) 0 1))]
                [(and (list? inst)
                      (equal? (first inst) '←))
                 (define val (second inst))
                 (cond
                   [(eq? val 'R0) (set! R1 R0)]
                   [(eq? val 'R1) (void)]
                   [else (set! R1 val)])]
                [else
                 ;; Unreachable: the program was checked above.
                 (error 'run-xorm
                        (format "unknown instruction: ~v" inst))]))
            prog)
  (list R0 R1))

;; ⊕: The only runtime instruction: R0 ← R0 ⊕ R1
(define-syntax xor
  (syntax-rules ()
    [(_)
      (begin
        (emit
          '⊕))]))

;; ←: sets R1 to a constant or the value of a register
;;
;; A literal constant is range-checked at compile time, so `(← 300)` is a
;; syntax error reported at the offending expression rather than an exception
;; raised when the enclosing module runs.  Non-literal arguments are still
;; checked by `emit`.
(define-syntax (← stx)
  (syntax-parse stx
    [(_ c:exact-integer)
     #:when (not (<= 0 (syntax-e #'c) 255))
     (raise-syntax-error
      '← (format "constant ~a is out of range for an 8-bit register (0..255)"
                 (syntax-e #'c))
      stx #'c)]
    [(_ c)
     #'(emit (list '← c))]))

;; set-r0: sets R0 to a constant or to the contents of a register
;;
;; The constant form clears R0 by xoring it with itself and then applies the
;; constant, which leaves R1 holding that same constant.
;;
;; The register forms were documented but never worked: `(set-r0 'R0)`
;; expanded to the constant sequence with the register symbol substituted, so
;; it cleared R0 and then xored it with itself again, always yielding 0.  They
;; are now handled separately and, unlike the constant form, leave R1 alone:
;;
;;   (set-r0 'R0)  is a no-op -- R0 already holds R0, so nothing is emitted
;;   (set-r0 'R1)  moves R1 into R0 through the temp slot
(define-syntax (set-r0 stx)
  (syntax-parse stx
    #:literals (quote)
    [(_ (quote reg))
     #:when (eq? (syntax-e #'reg) 'R0)
     #'(void)]
    [(_ (quote reg))
     #:when (eq? (syntax-e #'reg) 'R1)
     #'(begin
         (emit 'store-r1)
         (emit 'load-r0-from-temp))]
    [(_ (quote reg))
     (raise-syntax-error 'set-r0
                         (format "unknown register ~a (expected 'R0 or 'R1)"
                                 (syntax-e #'reg))
                         stx #'reg)]
    [(_ c:exact-integer)
     #:when (not (<= 0 (syntax-e #'c) 255))
     (raise-syntax-error
      'set-r0 (format "constant ~a is out of range for an 8-bit register (0..255)"
                      (syntax-e #'c))
      stx #'c)]
    [(_ c)
     #'(begin (← 'R0) (xor) (← c) (xor))]))

;; seq: run a sequence of operations
;;
;; This was called `do`, which shadowed `racket`'s own iteration form for
;; anyone who required this module -- `(do ((i 0 (add1 i))) ...)` became an
;; unbound identifier.  `seq` is free in `racket` and reads at least as well
;; here.
(define-syntax seq
  (syntax-rules ()
    [(_ op ...)
      (begin
        op ...)]))

;; swap: Swap values between R0 and R1
(define-syntax swap
  (syntax-rules ()
    [(_)
     (begin
       (emit 'store-r1)
       (copy-to-r1)
       (xor)
       (emit 'load-r0-from-temp))]))

;; clear-r0: Set R0 to 0 (also overwrites R1 with 0 via set-r0)
(define-syntax clear-r0
  (syntax-rules ()
    [(_)
     (begin
       (set-r0 0))]))  ; Set R0 (and consequently R1) to 0

;; clear-r1: Set R1 to 0
(define-syntax clear-r1
  (syntax-rules ()
    [(_)
     (begin
       (← 0))]))  ; Set R1 to 0

;; inc-r0: R0 ← R0 + 1, with 8-bit wrap-around
;;
;; Adding 1 is just `add-r0-r1` against the constant 1.  Carry is set when the
;; increment wraps 255 → 0.  Clobbers R1 (leaves it holding 1).
(define-syntax inc-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 1)
       (add-r0-r1))]))

;; dec-r0: R0 ← R0 - 1, with 8-bit wrap-around
;;
;; Subtracting 1 modulo 256 is the same as adding 255, so this is `add-r0-r1`
;; against the constant 255.  Carry follows the usual "carry = NOT borrow"
;; convention: it is set whenever the subtraction does *not* borrow, i.e. for
;; every starting value except 0.  Clobbers R1 (leaves it holding 255).
(define-syntax dec-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 255)
       (add-r0-r1))]))

;; copy-to-r1: Copy value from R0 to R1
(define-syntax copy-to-r1
  (syntax-rules ()
    [(_)
     (begin
       (← 0)      ; Set R1 to 0
       (xor)      ; R0 = R0 ⊕ 0 = R0
       (← 'R0))]))  ; Set R1 to R0

;; not-r0: Bitwise NOT of R0
(define-syntax not-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 255)    ; Set R1 to 255 (all 1s)
       (xor))]))  ; R0 = R0 ⊕ 255 (flips all bits)

;; and-r0-r1: Bitwise AND with result in R0
(define-syntax and-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       (emit 'AND))]))

;; or-r0-r1: Bitwise OR with result in R0
(define-syntax or-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       (emit 'OR))]))

;; set-carry: Set the carry flag (0 or 1)
(define-syntax (set-carry stx)
  (syntax-parse stx
    [(_ c:exact-integer)
     #:when (not (memv (syntax-e #'c) '(0 1)))
     (raise-syntax-error
      'set-carry (format "carry must be 0 or 1, got ~a" (syntax-e #'c))
      stx #'c)]
    [(_ c)
     #'(emit (list 'set-carry c))]))

;; clear-carry: Convenience wrapper for `(set-carry 0)`
(define-syntax clear-carry
  (syntax-rules ()
    [(_)
     (begin
       (set-carry 0))]))

;; store-carry-in-r1: Move the current carry into R1
(define-syntax store-carry-in-r1
  (syntax-rules ()
    [(_)
     (begin
       (emit 'carry->r1))]))

;; add-r0-r1: 8-bit addition with wrap-around
(define-syntax add-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       (clear-carry)
       (emit 'ADD))]))

;; <<: compile-time helper that doubles a numeric literal, masked to 8 bits.
;;
;; This does not touch R1 or emit anything -- it is arithmetic on a constant,
;; for writing things like `(← (<< 3))`.  A non-numeric argument is passed
;; through unchanged.  The register-level shift is `shift-left-r0`.
(define-syntax (<< stx)
  (syntax-parse stx
    [(_ val)
     (define v (syntax-e #'val))
     (if (number? v)
         (datum->syntax stx (bitwise-and (arithmetic-shift v 1) 255))
         #'val)]))

;; shift-left-r0: R0 ← R0 << 1, with 8-bit wrap-around
;;
;; A left shift by one is a doubling, and doubling is `x + x`.  Copying R0 into
;; R1 and adding therefore gives a real shift with no new primitive, and the
;; bit shifted off the top lands in the carry for free as ADD's overflow.
;;
;; Note this clobbers R1, which ends up holding the *pre-shift* value of R0.
;; `shift-right-r0` leaves R1 alone; the asymmetry is the price of deriving
;; this one from ADD rather than adding a second shift primitive.
(define-syntax shift-left-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 'R0)
       (add-r0-r1))]))

;; shift-right-r0: R0 ← R0 >> 1 (logical), bit 0 shifted into the carry
;;
;; Unlike the left shift, this one genuinely needs a new primitive.  Halving is
;; not expressible from XOR, ADD and constant loads: the machine has no
;; conditionals to test a bit with and no operation that moves information
;; toward the low end of the register.  `SHR` is that primitive.
;;
;; Leaves R1 untouched.
(define-syntax shift-right-r0
  (syntax-rules ()
    [(_)
     (begin
       (emit 'SHR))]))

;; >>: compile-time helper that halves a numeric literal.
;;
;; The constant-folding counterpart of `<<`; see the note there.  The
;; register-level shift is `shift-right-r0`.
(define-syntax (>> stx)
  (syntax-parse stx
    [(_ val)
     (define v (syntax-e #'val))
     (if (number? v)
         (datum->syntax stx (arithmetic-shift v -1))
         #'val)]))


;; Example usage when running this file directly.
(module+ main
  (reset-program!)
  (seq (set-r0 5))
  (seq (inc-r0))
  (displayln (list 'inc-result (run-xorm xorm-program)))      ; 5 + 1 = 6

  (reset-program!)
  (seq (set-r0 3))
  (seq (← 1))
  (seq (add-r0-r1))
  (displayln (list 'add-result (run-xorm xorm-program)))      ; 3 + 1 = 4

  (reset-program!)
  (seq (set-r0 5))
  (seq (shift-left-r0))
  (displayln (list 'shl-result (run-xorm xorm-program)))      ; 5 << 1 = 10

  (reset-program!)
  (seq (set-r0 5))
  (seq (shift-right-r0))
  (displayln (list 'shr-result (run-xorm xorm-program))))     ; 5 >> 1 = 2
