#lang info
(define collection 'multi)
(define deps '("base" "chk-lib" ("testing-util-lib" #:version "1.1")))
(define update-implies '("chk-lib"))
(define pkg-desc "Tests for \"chk\".")
