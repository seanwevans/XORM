#lang racket

(require rnrs/arithmetic/bitwise-6)
(require (for-syntax syntax/parse))


;; ============================================================================
;; XORM DSL
;;
;; XORM is a language with two 8‑bit registers (R0 and R1) and a single
;; runtime instruction: XOR, which computes:
;;    R0 ← R0 ⊕ R1
;;
;; This minimal DSL compiles high-level macros into a sequence of runtime
;; instructions, collected in the global variable `xorm-program`.
;; ============================================================================


;; the XORM program in emission order (first emitted, first executed)
(define xorm-program '())


;; register constants used by macros
(define R0 'R0)
(define R1 'R1)

;; Export DSL constructs
(provide
  xorm-program emit run-xorm
  xor ← set-r0 do swap clear-r0 clear-r1 inc-r0 dec-r0
  copy-to-r1 not-r0 and-r0-r1 or-r0-r1 add-r0-r1
  shift-left-r0 shift-right-r0 << >>
  set-carry clear-carry store-carry-in-r1
  reset-program!)

;; reset the recorded program
(define (reset-program!)
  (set! xorm-program '()))

;; instruction validation (see `emit` below)
(define (validate-inst inst)
  (when (and (list? inst)
             (equal? (first inst) '←))
    (define val (second inst))
    (cond
      [(or (eq? val 'R0) (eq? val 'R1))
       (void)]
      [(number? val)
       (unless (exact-integer? val)
         (error 'emit
                (format "← expected an integer constant, got ~a" val)))
       (unless (<= 0 val 255)
         (error 'emit
                (format "← constant ~a out of range 0..255" val)))]
      [else
       (error 'emit
              (format "← expected a register reference or integer constant, got ~a"
                      val))])))

;; Append an instruction to the program.
;;
;; The program is kept in emission order: the first instruction emitted is the
;; first executed, and that is the order every consumer -- `run-xorm`,
;; `decompile-xorm`, the tests -- reads it in.  Appending is O(n) per
;; instruction rather than O(1), but XORM programs are tens of instructions
;; long, and one unambiguous ordering is worth far more here than the
;; asymptotics.
(define (emit inst)
  (validate-inst inst)
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
                   [(eq? val 'R0) (set! R1 (mask-byte R0))]
                   [(eq? val 'R1) (set! R1 (mask-byte R1))]
                   [else (set! R1 (mask-byte val))])]
                [else
                 (error 'run-xorm
                        (format "Unknown instruction in run-xorm: ~v" inst))]))
            prog)
  (list R0 R1))

;; ⊕: The only runtime instruction: R0 ← R0 ⊕ R1
(define-syntax xor
  (syntax-rules ()
    [(_)
      (begin
        (emit
          '⊕))]))

;; ←: sets R1 to a constant
(define-syntax ←
  (syntax-rules ()
    [(_ c)
      (begin
        (emit
          (list '← c)))]))

;; set-r0: sets R0 to a constant (clobbers R1)
(define-syntax set-r0
  (syntax-rules ()
    [(_ c)
      (begin
        (← 'R0)   ; Copy current R0 into R1
        (xor)     ; Clear R0 by xoring it with itself
        (← c)     ; Load the requested constant
        (xor))])) ; Apply it to R0; R1 remains c

;; run a list of operations
(define-syntax do
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
(define-syntax set-carry
  (syntax-rules ()
    [(_ c)
     (begin
       (emit (list 'set-carry c)))]))

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

;; Shift R1 left by 1 bit
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

;; Shift R1 right by 1 bit
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
  (do (set-r0 5))
  (do (inc-r0))
  (displayln (list 'inc-result (run-xorm xorm-program)))      ; 5 + 1 = 6

  (reset-program!)
  (do (set-r0 3))
  (do (← 1))
  (do (add-r0-r1))
  (displayln (list 'add-result (run-xorm xorm-program)))      ; 3 + 1 = 4

  (reset-program!)
  (do (set-r0 5))
  (do (shift-left-r0))
  (displayln (list 'shl-result (run-xorm xorm-program)))      ; 5 << 1 = 10

  (reset-program!)
  (do (set-r0 5))
  (do (shift-right-r0))
  (displayln (list 'shr-result (run-xorm xorm-program))))     ; 5 >> 1 = 2
