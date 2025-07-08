#lang racket
(require rackunit
         "../xorm.rkt"
         "../mrox.rkt")

(test-case "decompile simple program"
  (reset-program!)
  (do (set-r0 3))
  (do (swap))
  (do (inc-r0))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '(set-r0 swap inc-r0)))

(test-case "decompile add-r0-r1"
  (reset-program!)
  (do (set-r0 5))
  (do (← 3))
  (do (add-r0-r1))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '(set-r0 (← 3) add-r0-r1)))

(provide (all-defined-out))
