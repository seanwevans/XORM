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
