#lang info
(define collection "identity-check")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/identity-check.scrbl" ())))
(define pkg-desc "Verify Chinese ID number")
(define version "0.0")
(define pkg-authors '(hin))
(define license '(Apache-2.0 OR MIT))
