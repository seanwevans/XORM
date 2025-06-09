#lang racket
(require rackunit
         "../xorm.rkt"
         "runtime-tests.rkt")

;; Test swap macro instruction sequence
(test-case "swap expands correctly"
  (reset-program!)
  (do (swap))
  (check-equal? xorm-program
                '(⊕ (← 0) (← R0) ⊕ (← R1) ⊕))
  )

;; Test clear-r0 macro
(test-case "clear-r0 expands correctly"
  (reset-program!)
  (do (clear-r0))
  (check-equal? xorm-program
                '((← 0) (← 0) ⊕))
  )

;; Test inc-r0 macro
(test-case "inc-r0 expands correctly"
  (reset-program!)
  (do (inc-r0))
  (check-equal? xorm-program
                '((← 1) ⊕))
  )

;; Test dec-r0 macro
(test-case "dec-r0 expands correctly"
  (reset-program!)
  (do (dec-r0))
  (check-equal? xorm-program
                '((← 255) ⊕ (← 1) ⊕ (← 255) ⊕))
  )

;; Provide tests for raco test
(provide (all-defined-out)
         (all-from-out "runtime-tests.rkt"))

