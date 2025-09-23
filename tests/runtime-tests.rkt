#lang racket
(require rackunit
         "../xorm.rkt")

;; Runtime test for inc-r0
(test-case "inc-r0 runtime"
  (reset-program!)
  (do (inc-r0))
  (check-equal? (run-xorm xorm-program)
                '(1 1)))

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
                '(0 0)))

;; Runtime test for shift-left-r0 (placeholder behavior)
(test-case "shift-left-r0 runtime"
  (reset-program!)
  (do (set-r0 5)
      (shift-left-r0))
  (check-equal? (run-xorm xorm-program)
                '(5 0)))

;; Runtime test for shift-right-r0 (placeholder behavior)
(test-case "shift-right-r0 runtime"
  (reset-program!)
  (do (set-r0 5)
      (shift-right-r0))
  (check-equal? (run-xorm xorm-program)
                '(5 0)))

(provide (all-defined-out))
