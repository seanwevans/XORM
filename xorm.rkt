#lang racket

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
                 (set! R1 (second inst))]
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


;; Example Usage
(do (set-r0 42))  ; Set R0 to 42
(do (← 69))       ; Set R1 to 69
(do (xor))        ; R0 ← 42 ⊕ 69 = 111
(do (← 127))      ; Set R1 to 127
(do (xor))        ; R0 ← 111 ⊕ 127 = 16
(list "(0 0) ↦" xorm-program '↦ (run-xorm xorm-program))
