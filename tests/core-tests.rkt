#lang racket
(require rackunit
         "../xorm.rkt"
         "runtime-tests.rkt")

;; Test swap macro instruction sequence
(test-case "swap expands correctly"
  (reset-program!)
  (do (swap))
  (check-equal? xorm-program
                '(store-r1 (← 0) ⊕ (← R0) ⊕ load-r0-from-temp))
  )

;; Test clear-r0 macro
(test-case "clear-r0 expands correctly"
  (reset-program!)
  (do (clear-r0))
  (check-equal? xorm-program
                '((← R0) ⊕ (← 0) ⊕))
  )

;; Test inc-r0 macro
(test-case "inc-r0 expands correctly"
  (reset-program!)
  (do (inc-r0))
  (check-equal? xorm-program
                '((← 1) (set-carry 0) ADD))
  )

;; Test dec-r0 macro
(test-case "dec-r0 expands correctly"
  (reset-program!)
  (do (dec-r0))
  (check-equal? xorm-program
                '((← 255) (set-carry 0) ADD))
  )

;; Test shift-left-r0 macro (a doubling, derived from ADD)
(test-case "shift-left-r0 expands correctly"
  (reset-program!)
  (do (shift-left-r0))
  (check-equal? xorm-program
                '((← R0) (set-carry 0) ADD))
  )

;; Test shift-right-r0 macro (the one genuinely new primitive)
(test-case "shift-right-r0 expands correctly"
  (reset-program!)
  (do (shift-right-r0))
  (check-equal? xorm-program
                '(SHR))
  )

;; Provide tests for raco test
(provide (all-defined-out)
         (all-from-out "runtime-tests.rkt"))

