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

;; Match one pattern element against one instruction.
;;
;; Returns a list of captured operands -- usually empty -- on success, or #f on
;; failure.  The captures are what make the output re-compilable: recovering
;; `set-r0` without its 42 tells you the shape of the program but not the
;; program.
;;
;; Three pattern elements are wildcards and capture what they match:
;;
;;   NUMBER            any constant load, `(← n)`
;;   REG               any register load, `(← R0)` or `(← R1)`
;;   (set-carry NUMBER) any carry assignment
;;
;; Everything else is a literal and must match exactly.
(define (match-instruction pattern-inst real-inst)
  (define (load-of? pred i)
    (and (list? i) (= (length i) 2) (eq? (first i) '←) (pred (second i))))
  (cond
    [(eq? pattern-inst 'NUMBER)
     (and (load-of? number? real-inst) (list (second real-inst)))]
    [(eq? pattern-inst 'REG)
     (and (load-of? (lambda (v) (memq v '(R0 R1))) real-inst)
          (list (second real-inst)))]
    [(and (list? pattern-inst)
          (= (length pattern-inst) 2)
          (eq? (first pattern-inst) 'set-carry)
          (eq? (second pattern-inst) 'NUMBER))
     (and (list? real-inst)
          (= (length real-inst) 2)
          (eq? (first real-inst) 'set-carry)
          (list (second real-inst)))]
    [(equal? pattern-inst real-inst) '()]
    [else #f]))

;; Match a whole pattern against the head of `prog`.
;; Returns (cons instructions-consumed captured-operands), or #f.
(define (match-sequence pattern-seq prog)
  (define pattern-len (length pattern-seq))
  (and (>= (length prog) pattern-len)
       (let loop ([pats pattern-seq]
                  [insts prog]
                  [captures '()])
         (if (null? pats)
             (cons pattern-len (reverse captures))
             (let ([m (match-instruction (car pats) (car insts))])
               (and m
                    (loop (cdr pats)
                          (cdr insts)
                          (append (reverse m) captures))))))))

(define macro-patterns
  `(
    (xor . (⊕))

    (inc-r0 . ((← 1) (set-carry 0) ADD))

    (set-r0 . ((← R0) ⊕ NUMBER ⊕))

    (swap . (store-r1 (← 0) ⊕ (← R0) ⊕ load-r0-from-temp))

    (clear-r0 . ((← R0) ⊕ (← 0) ⊕))

    (clear-r1 . ((← 0)))

    (not-r0 . ((← 255) ⊕))

    (dec-r0 . ((← 255) (set-carry 0) ADD))

    (copy-to-r1 . ((← 0) ⊕ (← R0)))

    (and-r0-r1 . (AND))

    (or-r0-r1 . (OR))

    (set-carry . ((set-carry NUMBER)))

    (clear-carry . ((set-carry 0)))

    (store-carry-in-r1 . (carry->r1))

    (add-r0-r1 . ((set-carry 0) ADD))

    (shift-left-r0 . ((← R0) (set-carry 0) ADD))

    (shift-right-r0 . (SHR))
  ))

(define (decompile-xorm program)
  (let loop ([prog program]
             [result '()])
    (if (null? prog)
        (reverse result)
        (let ([m (find-best-match prog)])
          (if m
              (let ([name (first m)]
                    [consumed (second m)]
                    [captures (third m)])
                (loop (list-tail prog consumed)
                      ;; A macro with operands comes back as a form, `(set-r0
                      ;; 42)`, so the result can be fed straight back to the
                      ;; compiler.  One without stays a bare name.
                      (cons (if (null? captures) name (cons name captures))
                            result)))
              ;; No macro matched, so keep the primitive instruction as-is.
              (loop (cdr prog)
                    (cons (car prog) result)))))))

;; Pick the best macro to explain the head of `prog`.
;;
;; The longest match wins.  On a tie the *most specific* pattern wins -- the
;; one that had to capture fewer operands to fit.  Without that rule `clear-r0`
;; and `(set-r0 0)` are the same four instructions and the winner is whichever
;; one `argmax` happened to see first; the same goes for `clear-carry` against
;; `(set-carry 0)`.
(define (find-best-match prog)
  (define matches
    (filter-map
     (lambda (pattern-entry)
       (let* ([macro-name (car pattern-entry)]
              [pattern-seq (cdr pattern-entry)]
              [match-result (match-sequence pattern-seq prog)])
         (and match-result
              (list macro-name (car match-result) (cdr match-result)))))
     macro-patterns))

  (define (better? a b)
    (cond
      [(> (second a) (second b)) #t]
      [(< (second a) (second b)) #f]
      [else (< (length (third a)) (length (third b)))]))

  (and (pair? matches)
       (for/fold ([best (first matches)]) ([m (rest matches)])
         (if (better? m best) m best))))

(define (pretty-print-decompiled prog)
  (for ([item prog])
    (cond
      [(symbol? item)
       (displayln (format "(~a)" item))]
      [(list? item)
       (displayln item)]
      [else (displayln item)])))

(define example-program
  '((← R0) ⊕ (← 42) ⊕          ; set-r0 42
    (← 1) (set-carry 0) ADD     ; inc-r0        (42 -> 43)
    (← R0) (set-carry 0) ADD    ; shift-left-r0 (43 -> 86)
    SHR                         ; shift-right-r0 (86 -> 43)
    (← 255) (set-carry 0) ADD)) ; dec-r0        (43 -> 42)

(module+ main
  (displayln "Original XORM program:")
  (for-each displayln example-program)

  (displayln "\nDecompiled high-level macros:")
  (pretty-print-decompiled (decompile-xorm example-program)))
