#lang racket

(require rnrs/arithmetic/bitwise-6)
(require (for-syntax syntax/parse))

(provide (all-defined-out))


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


;; the XORM program (stored in reverse emission order)
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

;; append an instruction to the program
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

(define (emit inst)
  (validate-inst inst)
  (set! xorm-program (append xorm-program (list inst))))

;; run a XORM program
(define (run-xorm prog)

  (define (mask-byte v)
    (bitwise-and v #xFF))

  (define R0 0)
  (define R1 0)
  (define temp 0)
  (for-each (lambda (inst)
              (cond
                [(eq? inst '⊕)
                 (set! R0 (mask-byte (bitwise-xor R0 R1)))]
                [(eq? inst 'store-r1)
                 (set! temp (mask-byte R1))]
                [(eq? inst 'load-r0-from-temp)
                 (set! R0 (mask-byte temp))]
                [(and (list? inst)
                      (equal? (first inst) '←))
                 (define val (second inst))
                 (cond
                   [(eq? val 'R0) (set! R1 (mask-byte R0))]
                   [(eq? val 'R1) (set! R1 (mask-byte R1))]
                   [else (set! R1 (mask-byte val))])]
                [else (error "???" inst)]))
            (reverse prog))
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

;; set-r0: sets R0 to a constant
(define-syntax set-r0
  (syntax-rules ()
    [(_ c)
      (begin
        (← 'R0)   ; Copy current R0 into R1
        (xor)     ; Clear R0 by xoring it with itself
        (← c)     ; Load the requested constant
        (xor))])) ; Apply it to R0

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

;; clear-r0: Set R0 to 0
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

;; inc-r0: Attempt to increment R0 by 1
;;
;; With only XOR and constant loads available this macro simply toggles the
;; lowest bit of R0.  It does **not** implement a correct increment operation
;; for arbitrary values.
(define-syntax inc-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 1)      ; load constant 1
       (xor))]))  ; toggles bit 0 of R0

;; dec-r0: Attempt to decrement R0 by 1
;;
;; This sequence is the inverse of `inc-r0` and is likewise incorrect for
;; general subtraction.  Only XOR operations and constant loads are used.
(define-syntax dec-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 255)    ; flip all bits
       (xor)
       (inc-r0)   ; try to add one to the inverted value
       (← 255)
       (xor))]))  ; flip back

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

;; shift-left-r0: Dummy left shift
;;
;; True shifting cannot be achieved with XOR alone.  This macro merely
;; scrambles the registers using XOR and constants.
(define-syntax shift-left-r0
  (syntax-rules ()
    [(_)
     (begin
       (copy-to-r1)
       (← (<< R1))
       (← 'R0)
       (xor)
       (xor)
       (← 0)
       (xor))]))

;; shift-right-r0: Dummy right shift
;;
;; Like the left shift, this macro does not actually shift bits.  It is kept
;; for symmetry and uses only XOR and constant loads.
(define-syntax shift-right-r0
  (syntax-rules ()
    [(_)
     (begin
       (copy-to-r1)
       (← (>> R1))
       (← 'R0)
       (xor)
       (xor)
       (← 0)
       (xor))]))

;; Shift R1 right by 1 bit
(define-syntax (>> stx)
  (syntax-parse stx
    [(_ val)
     (define v (syntax-e #'val))
     (if (number? v)
         (datum->syntax stx (arithmetic-shift v -1))
         #'val)]))


;; Example usage when running this file directly
;; The examples below illustrate the behaviour of the placeholder macros.  The
;; resulting values do not correspond to real arithmetic or bitwise logic.
(module+ main
  (reset-program!)
  (do (set-r0 5))
  (do (inc-r0))
  (displayln (list 'inc-result (run-xorm xorm-program)))

  (reset-program!)
  (do (set-r0 3))
  (do (← 1))
  (do (add-r0-r1))
  (displayln (list 'add-result (run-xorm xorm-program))))
