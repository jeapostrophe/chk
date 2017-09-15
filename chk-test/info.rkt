#lang info
(define collection 'multi)
(define deps '("chk-lib" ("testing-util-lib" #:version "1.1")))
(define build-deps '("base"))
(define update-implies '("chk-lib"))
(define pkg-desc "Tests for \"chk\".")
