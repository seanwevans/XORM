#lang racket
(require rackunit
         "../xorm.rkt")

;; Runtime test for inc-r0
(test-case "inc-r0 runtime"
  (reset-program!)
  (do (inc-r0))
  (check-equal? (run-xorm xorm-program)
                '(1 1)))

(test-case "swap runtime"
  (reset-program!)
  (do (set-r0 16)
      (← 32)
      (swap))
  (check-equal? (run-xorm xorm-program)
                '(32 16)))

;; Runtime test for dec-r0 starting from 5
(test-case "dec-r0 runtime"
  (reset-program!)
  (do (set-r0 5)
      (dec-r0))
  (check-equal? (run-xorm xorm-program)
                '(4 255)))

;; Regression: set-r0 overwrites non-zero R0
(test-case "set-r0 overwrites existing value"
  (reset-program!)
  (do (set-r0 5)
      (set-r0 42))
  (check-equal? (run-xorm xorm-program)
                '(42 42)))

;; Runtime test for add-r0-r1: 5 + 3 = 8
(test-case "add-r0-r1 runtime"
  (reset-program!)
  (do (set-r0 5)
      (← 3)
      (add-r0-r1))
  (check-equal? (run-xorm xorm-program)
                '(8 3)))

;; Addition should wrap around on overflow
(test-case "add-r0-r1 wrap-around"
  (reset-program!)
  (do (set-r0 255)
      (← 2)
      (add-r0-r1))
  (check-equal? (run-xorm xorm-program)
                '(1 2)))

;; Carry can be surfaced explicitly
(test-case "add-r0-r1 carry exposure"
  (reset-program!)
  (do (set-r0 200)
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
  (do (set-r0 5) (inc-r0))
  (check-equal? (first (run-xorm xorm-program)) 6))

(test-case "dec-r0 decrements rather than toggling bit 0"
  (reset-program!)
  (do (set-r0 4) (dec-r0))
  (check-equal? (first (run-xorm xorm-program)) 3))

(test-case "inc-r0 wraps 255 -> 0 and sets carry"
  (reset-program!)
  (do (set-r0 255) (inc-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(0 1)))

(test-case "dec-r0 wraps 0 -> 255 and clears carry (borrow)"
  (reset-program!)
  (do (set-r0 0) (dec-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(255 0)))

;; Runtime test for shift-left-r0: a real doubling, R1 left holding the
;; pre-shift value of R0.
(test-case "shift-left-r0 runtime"
  (reset-program!)
  (do (set-r0 5)
      (shift-left-r0))
  (check-equal? (run-xorm xorm-program)
                '(10 5)))

(test-case "shift-left-r0 overflow goes to the carry"
  (reset-program!)
  (do (set-r0 200) (shift-left-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(144 1)))

;; Runtime test for shift-right-r0: a real halving that leaves R1 alone.
(test-case "shift-right-r0 runtime"
  (reset-program!)
  (do (set-r0 5)
      (shift-right-r0))
  (check-equal? (run-xorm xorm-program)
                '(2 5)))

(test-case "shift-right-r0 shifts bit 0 into the carry"
  (reset-program!)
  (do (set-r0 5) (shift-right-r0) (store-carry-in-r1))
  (check-equal? (run-xorm xorm-program) '(2 1)))

;; Hand-written programs are in emission order, the same order `emit` stores.
(test-case "runtime masks wide constant loads"
  (check-equal? (run-xorm (list (list '← 300) '⊕))
                '(44 44)))

(test-case "runtime masks negative constant loads"
  (check-equal? (run-xorm (list (list '← -1) '⊕))
                '(255 255)))

(test-case "macro rejects >255 constant"
  (reset-program!)
  (check-exn
   (lambda (exn)
     (and (exn:fail? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda ()
     (do (← 300)))))

(test-case "macro rejects negative constant"
  (reset-program!)
  (check-exn
   (lambda (exn)
     (and (exn:fail? exn)
          (regexp-match? #rx"out of range" (exn-message exn))))
   (lambda ()
     (do (← -1)))))

(provide (all-defined-out))
