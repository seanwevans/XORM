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


;; the XORM program
(define xorm-program '())

;; register constants used by macros
(define R0 'R0)
(define R1 'R1)

;; append an instruction to the program
(define (emit inst)
  (set! xorm-program (append xorm-program (list inst))))

;; run a XORM program
(define (run-xorm prog)

  (define R0 0)
  (define R1 0)
  (for-each (lambda (inst)
              (cond
                [(eq? inst '⊕)
                 (set! R0 (bitwise-xor R0 R1))]
                [(and (list? inst)
                      (equal? (first inst) '←))
                 (let ([v (second inst)])
                   (cond
                     [(eq? v 'R0) (set! R1 R0)]
                     [(eq? v 'R1) (set! R1 R1)]
                     [else (set! R1 v)]))]
                [else (error "???" inst)]))
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

;; set-r0: sets R0 to a constant
(define-syntax set-r0
  (syntax-rules ()
    [(_ c)
      (begin
        (← c)
        (xor))]))

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
       (xor)        ; R0 = R0 ⊕ R1
       (← 0)        ; R1 = 0
       (← R0)       ; Set R1 to current R0
       (xor)        ; R0 = R0 ⊕ R0 = 0
       (← R1)       ; Restore original R1 to R1
       (xor))]))    ; R0 = 0 ⊕ R1 = original R1

;; clear-r0: Set R0 to 0
(define-syntax clear-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 0)      ; Set R1 to 0
       (set-r0 0))]))  ; Set R0 to 0

;; clear-r1: Set R1 to 0
(define-syntax clear-r1
  (syntax-rules ()
    [(_)
     (begin
       (← 0))]))  ; Set R1 to 0

;; inc-r0: Increment R0 by 1
(define-syntax inc-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 1)      ; Set R1 to 1
       (xor))]))  ; R0 = R0 ⊕ 1 (Flips least significant bit, effectively adding 1 if it's 0)

;; dec-r0: Decrement R0 by 1
(define-syntax dec-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 255)    ; Set R1 to 255 (all 1s)
       (xor)      ; R0 = R0 ⊕ 255 (flips all bits)
       (inc-r0)   ; Add 1 to flipped R0
       (← 255)    ; Set R1 to 255 again
       (xor))]))  ; Flip all bits back, giving us R0-1

;; copy-to-r1: Copy value from R0 to R1
(define-syntax copy-to-r1
  (syntax-rules ()
    [(_)
     (begin
       (← 0)      ; Set R1 to 0
       (xor)      ; R0 = R0 ⊕ 0 = R0
       (← R0))]))  ; Set R1 to R0

;; not-r0: Bitwise NOT of R0
(define-syntax not-r0
  (syntax-rules ()
    [(_)
     (begin
       (← 255)    ; Set R1 to 255 (all 1s)
       (xor))]))  ; R0 = R0 ⊕ 255 (flips all bits)

;; and-r0-r1: Bitwise AND of R0 and R1, result in R0
;; Using De Morgan's law: A & B = ¬(¬A | ¬B) = ¬(¬A ⊕ B ⊕ ¬B)
(define-syntax and-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       ;; Save R1
       (copy-to-r1)  ; R1 = R0
       ;; Prepare for AND operation
       (not-r0)      ; R0 = ~R0
       (xor)         ; R0 = ~R0 ⊕ R1
       (not-r0))]))  ; R0 = ~(~R0 ⊕ R1) = R0 & R1

;; or-r0-r1: Bitwise OR of R0 and R1, result in R0
;; Using the identity: A | B = A ⊕ B ⊕ (A & B)
(define-syntax or-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       ;; Save original R0 and R1
       (copy-to-r1)  ; Store R0 in R1
       (← R1)        ; Get original R1
       ;; Compute A ⊕ B
       (xor)         ; R0 = R0 ⊕ R1
       ;; Save A ⊕ B
       (← R0)        ; Save A ⊕ B in R1
       ;; Compute A & B
       (← R0)        ; R1 = R0 (original)
       (and-r0-r1)   ; R0 = R0 & R1
       ;; Final step: (A ⊕ B) ⊕ (A & B)
       (← R0)        ; Set R1 to A & B
       (← R0)        ; Set R1 to A ⊕ B
       (xor))]))     ; R0 = (A ⊕ B) ⊕ (A & B) = A | B

;; add-r0-r1: Add R1 to R0, result in R0 (simple 8-bit addition)
(define-syntax add-r0-r1
  (syntax-rules ()
    [(_)
     (begin
       (and-r0-r1)    ; R0 = R0 & R1 (get common 1 bits)
       (← R0)         ; Store A & B
       (← R1)         ; Get original R1
       (xor)          ; R0 = R0 ⊕ R1
       (← R0)         ; Store A ⊕ B
       (← R1)         ; Restore A & B
       (← (<< R1))      ; Shift left (multiply by 2)
       (xor))]))      ; R0 = (A ⊕ B) ⊕ ((A & B) << 1)

;; Shift R1 left by 1 bit
(define-syntax (<< stx)
  (syntax-parse stx
    [(_ val)
     (define v (syntax-e #'val))
     (if (number? v)
         (datum->syntax stx (bitwise-and (arithmetic-shift v 1) 255))
         #'val)]))

;; shift-left-r0: Shift R0 left by 1 bit, result in R0
(define-syntax shift-left-r0
  (syntax-rules ()
    [(_)
     (begin
       (copy-to-r1)    ; R1 = R0
       (← (<< R1))       ; R1 = R0 << 1
       (set-r0 0)      ; Clear R0
       (← R1)          ; Set R1 to shifted value
       (xor))]))       ; R0 = 0 ⊕ R1 = R1

;; shift-right-r0: Shift R0 right by 1 bit, result in R0
(define-syntax shift-right-r0
  (syntax-rules ()
    [(_)
     (begin
       (copy-to-r1)    ; R1 = R0
       (← (>> R1))       ; R1 = R0 >> 1
       (set-r0 0)      ; Clear R0
       (← R1)          ; Set R1 to shifted value
       (xor))]))       ; R0 = 0 ⊕ R1 = R1

;; Shift R1 right by 1 bit
(define-syntax (>> stx)
  (syntax-parse stx
    [(_ val)
     (define v (syntax-e #'val))
     (if (number? v)
         (datum->syntax stx (arithmetic-shift v -1))
         #'val)]))

;; Testing
(do (set-r0 42))     ; Set R0 to 42
(do (← 13))          ; Set R1 to 13
(do (add-r0-r1))     ; R0 = 42 + 13 = 55
(do (inc-r0))        ; R0 = 55 + 1 = 56
(do (dec-r0))        ; R0 = 56 - 1 = 55
(do (← 127))         ; Set R1 to 127
(do (and-r0-r1))     ; R0 = 55 & 127 = 55
(do (← 72))          ; Set R1 to 72
(do (or-r0-r1))      ; R0 = 55 | 72 = 127
(do (shift-left-r0)) ; R0 = 127 << 1 = 254
(do (shift-right-r0)); R0 = 254 >> 1 = 127
(do (swap))          ; Swap R0 and R1, R0 = 72, R1 = 127

(list "(0 0) ↦" xorm-program '↦ (run-xorm xorm-program))