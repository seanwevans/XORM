#lang racket

(require racket/list)

;; ============================================================================
;; 🐂 MR. OX 🐂 - The XORM decompiler
;;
;; MR. OX analyzes sequences of XORM primitive operations (XOR and register
;; settings) and attempts to recognize patterns that correspond to higher-level
;; macros defined in the XORM language.
;; ============================================================================

(provide decompile-xorm)

(define (match-instruction pattern-inst real-inst)
  (cond
    [(equal? pattern-inst real-inst) #t]
    
    [(and (eq? pattern-inst 'NUMBER)
          (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) '←)
          (number? (second real-inst)))
     #t]
    
    [(and (eq? pattern-inst 'REG)
          (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) '←)
          (or (eq? (second real-inst) 'R0)
              (eq? (second real-inst) 'R1)))
     #t]
    
    [(and (list? pattern-inst)
          (= (length pattern-inst) 2)
          (eq? (first pattern-inst) '←)
          (number? (second pattern-inst))
          (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) '←)
          (equal? (second pattern-inst) (second real-inst)))
     #t]

    [(and (list? pattern-inst)
          (= (length pattern-inst) 2)
          (eq? (first pattern-inst) 'set-carry)
          (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) 'set-carry))
     (let ([pattern-val (second pattern-inst)]
           [real-val (second real-inst)])
       (cond
         [(eq? pattern-val 'NUMBER)
          (number? real-val)]
         [else
          (equal? pattern-val real-val)]))]
    
    [(and (list? pattern-inst)
          (= (length pattern-inst) 2)
          (eq? (first pattern-inst) '←)
          (or (eq? (second pattern-inst) 'R0)
              (eq? (second pattern-inst) 'R1))
          (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) '←)
          (eq? (second pattern-inst) (second real-inst)))
     #t]
    
    [else #f]))

(define (match-sequence pattern-seq prog)
  (let ([pattern-len (length pattern-seq)])
    (and (>= (length prog) pattern-len)
         (let loop ([i 0])
           (if (= i pattern-len)
               pattern-len  ; Successfully matched all instructions
               (if (match-instruction (list-ref pattern-seq i) (list-ref prog i))
                   (loop (add1 i))
                   #f))))))

(define macro-patterns
  `(
    (inc-r0 . ((← 1) ⊕))

    (set-r0 . ((← R0) ⊕ NUMBER ⊕))

    (swap . (store-r1 (← 0) ⊕ (← R0) ⊕ load-r0-from-temp))

    (clear-r0 . ((← R0) ⊕ (← 0) ⊕))

    (clear-r1 . ((← 0)))

    (not-r0 . ((← 255) ⊕))

    (dec-r0 . ((← 255) ⊕ (← 1) ⊕ (← 255) ⊕))

    (copy-to-r1 . ((← 0) ⊕ (← R0)))

    (and-r0-r1 . (AND))

    (or-r0-r1 . (OR))

    (set-carry . ((set-carry NUMBER)))

    (clear-carry . ((set-carry 0)))

    (store-carry-in-r1 . (carry->r1))

    (add-r0-r1 . ((set-carry 0) ADD))

    (shift-left-r0-placeholder . ((← 0) ⊕ (← R0) (← R1) (← R0) ⊕ ⊕ (← 0) ⊕))

    (shift-right-r0-placeholder . ((← 0) ⊕ (← R0) (← R1) (← R0) ⊕ ⊕ (← 0) ⊕))
  ))

(define (decompile-xorm program)
  (let loop ([prog program]
             [result '()])
    (if (null? prog)
        (reverse result)  ; Done processing
        (let ([matched-pattern (find-best-match prog)])
          (if matched-pattern
              (let ([macro-name (car matched-pattern)]
                    [consumed (cdr matched-pattern)])
                (loop (list-tail prog consumed)
                      (cons macro-name result)))
              ;; No match, keep as primitive instruction
              (loop (cdr prog)
                    (cons (car prog) result)))))))

(define (find-best-match prog)
  (define matches
    (filter-map 
     (lambda (pattern-entry)
       (let* ([macro-name (car pattern-entry)]
              [pattern-seq (cdr pattern-entry)]
              [match-result (match-sequence pattern-seq prog)])
         (and match-result 
              (cons macro-name match-result))))
     macro-patterns))
  
  ;; Find the pattern that consumes the most instructions
  (if (null? matches)
      #f
      (argmax cdr matches)))

(define (pretty-print-decompiled prog)
  (for ([item prog])
    (cond
      [(symbol? item)
       (displayln (format "(~a)" item))]
      [(list? item)
       (displayln item)]
      [else (displayln item)])))

(define example-program
  '((← 42)      ; Set R1 to 42
    ⊕           ; XOR into R0
    (← 13)      ; Set R1 to 13  
    ⊕           ; XOR into R0
    (← 1)       ; Set R1 to 1
    ⊕           ; XOR into R0 (inc-r0)
    (← 255)     ; Set R1 to 255
    ⊕           ; XOR into R0
    (← 1)       ; Set R1 to 1
    ⊕           ; XOR into R0
    (← 255)     ; Set R1 to 255
    ⊕))         ; XOR into R0 (dec-r0)

(module+ main
  (displayln "Original XORM program:")
  (for-each displayln example-program)

  (displayln "\nDecompiled high-level macros:")
  (pretty-print-decompiled (decompile-xorm example-program)))
