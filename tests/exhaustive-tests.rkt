#lang racket
(require rackunit
         "../xorm.rkt")

;; ============================================================================
;; Exhaustive semantics tests
;;
;; XORM's entire architectural state is two 8-bit registers and a carry flag.
;; That is 2^17 reachable configurations, which is small enough to enumerate
;; outright -- so these tests do not sample, they *prove*, by checking every
;; macro against an independent reference model at every input it can be given.
;;
;; This is worth having because the failure mode it catches is the one that
;; actually happened: `inc-r0` and `dec-r0` shipped as `xor 1`, which is correct
;; for exactly the inputs the hand-written tests used and wrong everywhere else.
;; A single well-chosen example cannot tell those apart. 65536 of them can.
;; ============================================================================

(define (mask v) (bitwise-and v #xFF))

;; Compile a macro body once, then run it from every (R0, R1) pair and compare
;; against `ref`, a function from the starting state to the expected
;; '(R0 R1) result.
(define (check-all-states prog ref)
  (define mismatches
    (for*/list ([r0 (in-range 256)]
                [r1 (in-range 256)]
                #:unless (equal? (run-xorm prog #:r0 r0 #:r1 r1) (ref r0 r1)))
      (list 'from (list r0 r1)
            'got (run-xorm prog #:r0 r0 #:r1 r1)
            'want (ref r0 r1))))
  (check-equal? (if (null? mismatches)
                    '()
                    (take mismatches (min 3 (length mismatches))))
                '()
                (format "~a of 65536 starting states disagree with the reference model"
                        (length mismatches))))

;; Same, but checks the carry flag: appends `store-carry-in-r1` so the flag
;; becomes observable in R1, and compares against `carry-ref`.
(define (check-all-carries prog carry-ref)
  (define observed (append prog (list 'carry->r1)))
  (define mismatches
    (for*/list ([r0 (in-range 256)]
                [r1 (in-range 256)]
                #:unless (equal? (second (run-xorm observed #:r0 r0 #:r1 r1))
                                 (carry-ref r0 r1)))
      (list 'from (list r0 r1)
            'got (second (run-xorm observed #:r0 r0 #:r1 r1))
            'want (carry-ref r0 r1))))
  (check-equal? (if (null? mismatches)
                    '()
                    (take mismatches (min 3 (length mismatches))))
                '()
                (format "~a of 65536 starting states disagree on the carry flag"
                        (length mismatches))))

(define-syntax-rule (compiled op ...)
  (let () (reset-program!) (do op ...) xorm-program))

;; ---------------------------------------------------------------------------
;; Bitwise primitives
;; ---------------------------------------------------------------------------

(test-case "xor over all 65536 states"
  (check-all-states (compiled (xor))
                    (lambda (r0 r1) (list (bitwise-xor r0 r1) r1))))

(test-case "and-r0-r1 over all 65536 states"
  (check-all-states (compiled (and-r0-r1))
                    (lambda (r0 r1) (list (bitwise-and r0 r1) r1))))

(test-case "or-r0-r1 over all 65536 states"
  (check-all-states (compiled (or-r0-r1))
                    (lambda (r0 r1) (list (bitwise-ior r0 r1) r1))))

(test-case "not-r0 over all 65536 states"
  (check-all-states (compiled (not-r0))
                    (lambda (r0 r1) (list (bitwise-xor r0 255) 255))))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(test-case "add-r0-r1 over all 65536 states"
  (check-all-states (compiled (add-r0-r1))
                    (lambda (r0 r1) (list (mask (+ r0 r1)) r1))))

(test-case "add-r0-r1 carry over all 65536 states"
  (check-all-carries (compiled (add-r0-r1))
                     (lambda (r0 r1) (if (> (+ r0 r1) 255) 1 0))))

;; `add-r0-r1` clears the carry first, so the incoming flag must not affect the
;; result.  A raw ADD does consume it -- that is what makes multi-byte addition
;; possible -- so check both flag values explicitly.
(test-case "raw ADD consumes the incoming carry, add-r0-r1 does not"
  (for* ([r0 (in-range 0 256 17)]
         [r1 (in-range 0 256 17)]
         [c (in-list '(0 1))])
    (check-equal? (run-xorm '(ADD) #:r0 r0 #:r1 r1 #:carry c)
                  (list (mask (+ r0 r1 c)) r1))
    (check-equal? (run-xorm (compiled (add-r0-r1)) #:r0 r0 #:r1 r1 #:carry c)
                  (list (mask (+ r0 r1)) r1))))

(test-case "inc-r0 over all 65536 states"
  (check-all-states (compiled (inc-r0))
                    (lambda (r0 r1) (list (mask (add1 r0)) 1))))

(test-case "dec-r0 over all 65536 states"
  (check-all-states (compiled (dec-r0))
                    (lambda (r0 r1) (list (mask (sub1 r0)) 255))))

;; inc and dec must be inverses at every point, including across both wraps.
;; The XOR-based versions they replaced were *identical* to each other, so this
;; would have failed at every odd input.
(test-case "inc-r0 and dec-r0 invert each other everywhere"
  (define prog (compiled (inc-r0) (dec-r0)))
  (for ([r0 (in-range 256)])
    (check-equal? (first (run-xorm prog #:r0 r0)) r0)))

;; ---------------------------------------------------------------------------
;; Shifts
;; ---------------------------------------------------------------------------

(test-case "shift-left-r0 over all 65536 states"
  (check-all-states (compiled (shift-left-r0))
                    ;; leaves R1 holding the pre-shift value of R0
                    (lambda (r0 r1) (list (mask (arithmetic-shift r0 1)) r0))))

(test-case "shift-left-r0 carry over all 65536 states"
  (check-all-carries (compiled (shift-left-r0))
                     (lambda (r0 r1) (if (> r0 127) 1 0))))

(test-case "shift-right-r0 over all 65536 states"
  (check-all-states (compiled (shift-right-r0))
                    ;; leaves R1 alone
                    (lambda (r0 r1) (list (arithmetic-shift r0 -1) r1))))

(test-case "shift-right-r0 carry over all 65536 states"
  (check-all-carries (compiled (shift-right-r0))
                     (lambda (r0 r1) (bitwise-and r0 1))))

;; ---------------------------------------------------------------------------
;; Register movement
;; ---------------------------------------------------------------------------

(test-case "swap over all 65536 states"
  (check-all-states (compiled (swap))
                    (lambda (r0 r1) (list r1 r0))))

(test-case "copy-to-r1 over all 65536 states"
  (check-all-states (compiled (copy-to-r1))
                    (lambda (r0 r1) (list r0 r0))))

(test-case "clear-r0 over all 65536 states"
  (check-all-states (compiled (clear-r0))
                    (lambda (r0 r1) (list 0 0))))

(test-case "clear-r1 over all 65536 states"
  (check-all-states (compiled (clear-r1))
                    (lambda (r0 r1) (list r0 0))))

;; `set-r0` takes a compile-time constant, so sweep the constant too: every
;; (starting state, constant) triple would be 2^24, which is more than this
;; suite should cost, so sweep all 256 constants against a spread of starting
;; states instead.
(define-syntax-rule (set-r0-case c)
  (let ([prog (compiled (set-r0 c))])
    (for* ([r0 (in-range 0 256 37)]
           [r1 (in-range 0 256 37)])
      (check-equal? (run-xorm prog #:r0 r0 #:r1 r1) (list c c)))))

(test-case "set-r0 lands on its constant from any starting state"
  (set-r0-case 0)   (set-r0-case 1)   (set-r0-case 2)   (set-r0-case 42)
  (set-r0-case 85)  (set-r0-case 127) (set-r0-case 128) (set-r0-case 170)
  (set-r0-case 200) (set-r0-case 254) (set-r0-case 255))

(provide (all-defined-out))
