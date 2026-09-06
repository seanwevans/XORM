#lang racket
(require rackunit
         "../xorm.rkt"
         "../mrox.rkt")

(test-case "decompile simple program"
  (reset-program!)
  (seq (set-r0 3))
  (seq (swap))
  (seq (inc-r0))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '((set-r0 3) swap inc-r0)))

(test-case "decompile add-r0-r1"
  (reset-program!)
  (seq (set-r0 5))
  (seq (← 3))
  (seq (add-r0-r1))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '((set-r0 5) (← 3) add-r0-r1)))

;; Regression: `run-xorm` and `decompile-xorm` must consume the same list.
;;
;; They used to disagree -- `run-xorm` reversed its argument, `decompile-xorm`
;; did not -- so handing a program to the wrong one produced a silently wrong
;; answer rather than an error.  This test pins the shared convention down.
(test-case "run-xorm and decompile-xorm agree on instruction order"
  (reset-program!)
  (seq (set-r0 10) (not-r0) (clear-r1) (add-r0-r1) (swap) (dec-r0))
  (define prog xorm-program)
  (check-equal? (decompile-xorm prog)
                '((set-r0 10) not-r0 clear-r1 add-r0-r1 swap dec-r0))
  (check-equal? (run-xorm prog) '(255 255))
  ;; Reversing the program must change what it computes; if this ever stops
  ;; holding, the two orderings have quietly become interchangeable again.
  (check-not-equal? (run-xorm (reverse prog)) (run-xorm prog)))

;; ---------------------------------------------------------------------------
;; Round trip
;;
;; The decompiler used to drop operands: `(set-r0 42)` came back as a bare
;; `set-r0`, so its output described the shape of a program without being one.
;; These tests hold it to the stronger property -- decompiling and recompiling
;; must reproduce the original instruction list exactly.
;; ---------------------------------------------------------------------------

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

;; Evaluate decompiler output, turning bare macro names back into calls.
(define (recompile decompiled)
  (reset-program!)
  (for ([form (in-list decompiled)])
    (eval (if (symbol? form) (list form) form) ns))
  xorm-program)

(define-syntax-rule (check-round-trip op ...)
  (let ()
    (reset-program!)
    (seq op ...)
    (define prog xorm-program)
    (check-equal? (recompile (decompile-xorm prog)) prog)))

(test-case "round trip: constants are preserved"
  (check-round-trip (set-r0 42))
  (check-round-trip (set-r0 0))
  (check-round-trip (set-r0 255))
  (check-round-trip (← 3))
  (check-round-trip (set-carry 1)))

(test-case "round trip: arithmetic and shifts"
  (check-round-trip (set-r0 42) (inc-r0) (shift-left-r0) (shift-right-r0) (dec-r0)))

(test-case "round trip: register movement"
  (check-round-trip (set-r0 9) (copy-to-r1) (swap) (clear-r1) (clear-r0)))

(test-case "round trip: a longer mixed program"
  (check-round-trip (set-r0 42) (inc-r0) (not-r0) (and-r0-r1) (or-r0-r1)
                    (swap) (add-r0-r1) (store-carry-in-r1) (shift-right-r0)
                    (dec-r0) (xor)))

;; The tie-break that keeps `clear-r0` from being reported as `(set-r0 0)` and
;; `clear-carry` from being reported as `(set-carry 0)`: identical instructions,
;; so the more specific name has to win deterministically.
(test-case "ambiguous patterns resolve to the more specific macro"
  (reset-program!)
  (seq (clear-r0))
  (check-equal? (decompile-xorm xorm-program) '(clear-r0))
  (reset-program!)
  (seq (clear-carry))
  (check-equal? (decompile-xorm xorm-program) '(clear-carry)))

(provide (all-defined-out))
