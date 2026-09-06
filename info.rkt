#lang info

(define collection "xorm")
(define pkg-desc "⊕, macros, and two 8-bit registers. That's all you get.")
(define version "0.1")
(define license 'MIT)

;; Everything the library itself needs ships with a plain Racket install.
(define deps '("base"))
(define build-deps '("rackunit-lib"))
