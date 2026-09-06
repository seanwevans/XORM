#lang racket
(require rackunit
         syntax/macro-testing
         "../xorm.rkt")

;; Runtime test for inc-r0
(test-case "inc-r0 runtime"
  (reset-program!)
  (seq (inc-r0))
  (check-equal? (run-xorm xorm-program)
                '(1 1)))

(test-case "swap runtime"
  (reset-program!)
  (seq (set-r0 16)
      (← 32)
      (swap))
  (check-equal? (run-xorm xorm-program)
                '(32 16)))

;; Runtime test for dec-r0 starting from 5
(test-case "dec-r0 runtime"
  (reset-program!)
  (seq (set-r0 5)
      (dec-r0))
  (check-equal? (run-xorm xorm-program)
                '(4 255)))

;; Regression: set-r0 overwrites non-zero R0
(test-case "set-r0 overwrites existing value"
  (reset-program!)
  (seq (set-r0 5)
      (set-r0 42))
  (check-equal? (run-xorm xorm-program)
                '(42 42)))

;; Runtime test for add-r0-r1: 5 + 3 = 8
(test-case "add-r0-r1 runtime"
  (reset-program!)
  (seq (set-r0 5)
      (← 3)
      (add-r0-r1))
  (check-equal? (run-xorm xorm-program)
                '(8 3)))

;; Addition should wrap around on overflow
(test-case "add-r0-r1 wrap-around"
  (reset-program!)
  (seq (set-r0 255)
      (← 2)
      (add-r0-r1))
  (check-equal? (run-xorm xorm-program)
                '(1 2)))

;; Carry can be surfaced explicitly
(test-case "add-r0-r1 carry exposure"
  (reset-program!)
  (seq (set-r0 200)
      (← 100)
      (add-r0-r1)
      (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program)
                '(44 1)))

;; inc-r0 and dec-r0 are real 8-bit arithmetic, not bit-0 toggles.  The old
;; XOR-based versions expanded to `xor 1` and so ran backwards on odd inputs:
;; (set-r0 5) (inc-r0) produced 4, and (set-r0 4) (dec-r0) produced 5.
(test-case "inc-r0 increments rather than toggling bit 0"
  (reset-program!)
  (seq (set-r0 5) (inc-r0))
  (check-equal? (first (run-xorm xorm-program)) 6))

(test-case "dec-r0 decrements rather than toggling bit 0"
  (reset-program!)
  (seq (set-r0 4) (dec-r0))
  (check-equal? (first (run-xorm xorm-program)) 3))

(test-case "inc-r0 wraps 255 -> 0 and sets carry"
  (reset-program!)
  (seq (set-r0 255) (inc-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(0 1)))

(test-case "dec-r0 wraps 0 -> 255 and clears carry (borrow)"
  (reset-program!)
  (seq (set-r0 0) (dec-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(255 0)))

;; Runtime test for shift-left-r0: a real doubling, R1 left holding the
;; pre-shift value of R0.
(test-case "shift-left-r0 runtime"
  (reset-program!)
  (seq (set-r0 5)
      (shift-left-r0))
  (check-equal? (run-xorm xorm-program)
                '(10 5)))

(test-case "shift-left-r0 overflow goes to the carry"
  (reset-program!)
  (seq (set-r0 200) (shift-left-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(144 1)))

;; Runtime test for shift-right-r0: a real halving that leaves R1 alone.
(test-case "shift-right-r0 runtime"
  (reset-program!)
  (seq (set-r0 5)
      (shift-right-r0))
  (check-equal? (run-xorm xorm-program)
                '(2 5)))

(test-case "shift-right-r0 shifts bit 0 into the carry"
  (reset-program!)
  (seq (set-r0 5) (shift-right-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(2 1)))

;; Hand-written programs are in emission order, the same order `emit` stores.
;;
;; `run-xorm` enforces the same instruction set `emit` does.  It used to mask
;; an out-of-range constant into range instead, which meant a program the macro
;; layer refused to build would run happily if you handed it over directly.
(define (out-of-range? exn)
  (and (exn:fail? exn) (regexp-match? #rx"out of range" (exn-message exn))))

(test-case "runtime rejects a constant above 255"
  (check-exn out-of-range? (lambda () (run-xorm (list (list '← 300) '⊕)))))

(test-case "runtime rejects a negative constant"
  (check-exn out-of-range? (lambda () (run-xorm (list (list '← -1) '⊕)))))

(test-case "runtime rejects an unknown instruction"
  (check-exn (lambda (exn)
               (and (exn:fail? exn)
                    (regexp-match? #rx"unknown instruction" (exn-message exn))))
             (lambda () (run-xorm '(BOGUS)))))

(test-case "runtime rejects a bad carry value"
  (check-exn (lambda (exn)
               (and (exn:fail? exn)
                    (regexp-match? #rx"set-carry expected 0 or 1" (exn-message exn))))
             (lambda () (run-xorm '((set-carry 7))))))

;; The same definition applies at emission time.
(test-case "emit rejects an unknown instruction"
  (reset-program!)
  (check-exn (lambda (exn)
               (and (exn:fail? exn)
                    (regexp-match? #rx"unknown instruction" (exn-message exn))))
             (lambda () (emit 'BOGUS)))
  (check-equal? xorm-program '()))

;; A program is rejected before any of it runs, not part-way through.
(test-case "a bad instruction stops the program before it starts"
  (check-exn out-of-range?
             (lambda () (run-xorm (list (list '← 5) '⊕ (list '← 999))))))

;; A literal out-of-range constant is now rejected at *compile* time, with the
;; error pointing at the offending literal rather than surfacing when the
;; enclosing module happens to run.  `convert-compile-time-error` lets a test
;; observe an expansion-time error at run time.
(test-case "macro rejects >255 constant at compile time"
  (check-exn
   (lambda (exn)
     (and (exn:fail:syntax? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda () (convert-compile-time-error (seq (← 300))))))

(test-case "macro rejects negative constant at compile time"
  (check-exn
   (lambda (exn)
     (and (exn:fail:syntax? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda () (convert-compile-time-error (seq (← -1))))))

(test-case "set-r0 rejects an out-of-range constant at compile time"
  (check-exn
   (lambda (exn)
     (and (exn:fail:syntax? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda () (convert-compile-time-error (seq (set-r0 999))))))

(test-case "set-carry rejects anything but 0 or 1 at compile time"
  (check-exn
   (lambda (exn)
     (and (exn:fail:syntax? exn)
          (regexp-match? #rx"carry must be 0 or 1" (exn-message exn))))
   (lambda () (convert-compile-time-error (seq (set-carry 7))))))

(test-case "set-r0 rejects an unknown register at compile time"
  (check-exn
   (lambda (exn)
     (and (exn:fail:syntax? exn)
          (regexp-match? #rx"unknown register" (exn-message exn))))
   (lambda () (convert-compile-time-error (seq (set-r0 'R7))))))

;; A non-literal argument cannot be checked during expansion, so `emit` still
;; enforces the same range when the program is built.
(test-case "emit rejects an out-of-range value computed at run time"
  (reset-program!)
  (check-exn
   (lambda (exn)
     (and (exn:fail? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda () (let ([n 300]) (seq (← n))))))

;; The register forms of set-r0 were documented but always produced 0.
(test-case "set-r0 with a register moves the register into R0"
  (reset-program!)
  (seq (set-r0 200) (← 42) (set-r0 'R1))
  (check-equal? (run-xorm xorm-program) '(42 42)))

(test-case "set-r0 'R0 is a no-op and emits nothing"
  (reset-program!)
  (seq (set-r0 77))
  (define before xorm-program)
  (seq (set-r0 'R0))
  (check-equal? xorm-program before)
  (check-equal? (run-xorm xorm-program) '(77 77)))

(provide (all-defined-out))
