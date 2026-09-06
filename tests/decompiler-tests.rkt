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

;; Regression: `run-xorm` and `decompile-xorm` must consume the same list.
;;
;; They used to disagree -- `run-xorm` reversed its argument, `decompile-xorm`
;; did not -- so handing a program to the wrong one produced a silently wrong
;; answer rather than an error.  This test pins the shared convention down.
(test-case "run-xorm and decompile-xorm agree on instruction order"
  (reset-program!)
  (do (set-r0 10) (not-r0) (clear-r1) (add-r0-r1) (swap) (dec-r0))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '(set-r0 not-r0 clear-r1 add-r0-r1 swap dec-r0))
  (check-equal? (run-xorm prog) '(1 255))
  ;; Reversing the program must change what it computes; if this ever stops
  ;; holding, the two orderings have quietly become interchangeable again.
  (check-not-equal? (run-xorm (reverse prog)) (run-xorm prog)))

(provide (all-defined-out))
