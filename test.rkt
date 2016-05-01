#lang racket/base
(module+ test
  (require chk)
  (chk #:! #:x (/ 1 0) "division")
  (chk
   1 1
   ;; Fail 1
   1 0
   #:! 1 0
   ;; Fail 2
   #:! 1 1

   #:! #:! #:! 1 0
   #:! #:! 1 1

   #:! (/ 1 0) +inf.0
   ;; Fail 3
   (/ 1 0) +inf.0

   (/ 1 0) (/ 1 0)
   ;; Fail 4
   #:! (/ 1 0) (/ 1 0)

   (error 'fail "a") (error 'fail "a")
   #:! (error 'fail "a") (error 'fail "b")

   #:! #:t (/ 1 0)
   ;; Fail 5
   #:t (values 0 1)
   ;; Fail 6
   #:t (values #f 1)
   #:! #:t (values #f 1)

   1 1
   2 2
   ;; Fail 7
   #:x (/ 1 0) "divided"
   #:x (/ 1 0) "division"
   #:x (/ 1 0) #rx"di.ision"
   #:x (/ 1 0) exn:fail?
   #:! #:x (/ 1 1) "division"
   #:! #:x (/ 1 0) "diblision"
   (/ 1 0) (error '/ "division by zero")

   #:t (chk 1)
   #:t 1
   #:! #f
   #:! #:t #f
   1 1
   #:t 1
   #:! 2 3

   ;; It just works with values
   (values 1 2) (values 1 2)
   ;; Fail 8
   (values 1 2) (values 2 3)
   #:! (values 1 2) (values 2 3)
   ;; Fail 9
   (values 1 2) 3
   #:! (values 1 2) 3
   #:! 3 (values 1 2)
   (quotient/remainder 10 3) (values 3 1)

   ;; You can make things more explicit, if you want, by adding parens
   ;; and #:=
   #:= 1 1
   [#:x (/ 1 0) "division"]
   [#:! #f]
   [#:t 1]
   [#:= 1 1]

   ;; You can change what the printed source AND location is
   ;; Fail 10
   #:= [#:stx one 1] 2

   ;; You can also change what JUST source location is
   ;; Fail 11
   #:= [#:src #'here 1] 2
   ;; Fail 12
   #:= [#:src '(there 99 1 22 1) 1] 2

   ;; You can put in arbitrary code and definitions:
   #:do (define x 1)
   #:= x 1
   #:do (printf "Hey, listen!\n")

   ;; chk* is a form like "let ()" ensures the block exits on any
   ;; failure. (This is a bit of a weird use-case, normally you'd use
   ;; this to implement your own custom testing form, so you wouldn't
   ;; immediately put a big chk form)
   #:do
   (chk* (chk #:= 1 1
              ;; Fail 13
              #:= 2 3
              ;; NOT printed
              #:do (printf "Hey, listen!")
              ;; NOT Fail 14
              #:= 5 6))

   ;; with-chk allows you to add information to all failures contained
   ;; in its body.
   #:do
   (with-chk (['module "testing"]
              ['author "jay"])
     ;; Fail 14
     (chk 1 2)
     (with-chk (['amazing? #t])
       ;; Fail 15
       (chk 2 3)))

   ;; #:eq allows you to specify a comparison
   #:eq eq? 'foo 'foo
   #:! #:eq eq? 'foo 'bar
   ;; Fail 16
   #:eq eq? "foo" (list->string (string->list "foo"))
   ;; Fail 17
   #:eq string=? "foo" 'bar
   ;; Fail 18
   #:eq (/ 1 0) 0 0

   ;; #:? allows you to specify a predicate
   #:? even? 0
   ;; Fail 19
   #:? even? 1
   ;; Fail 20
   #:? even? "two"
   ;; Fail 21
   #:? (/ 1 0) 0))
