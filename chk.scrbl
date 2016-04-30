#lang scribble/manual
@(require (for-label chk
                     racket/base)
          scribble/example
          racket/sandbox)

@(define e (make-base-eval '(require chk)))

@title{chk: a minimal tester}
@author{@author+email["Jay McCarthy" "jay@racket-lang.org"]}

@defmodule[chk]

This package defines a minimal testing library with a simple
implementation, sensible defaults, useful flexibilty, and beautiful
failure reporting. It is independent of all other Racket testing
libraries, but it does cooperate with @exec{raco test} and @exec{raco
cover} for displaying summary information.

@defform[(chk . t)]{

Evaluates the tests described in @racket[t] as described below:

@racketgrammar*[[t (test ...)]
                [test
                 strict-test
                 (strict-test)
                 (ex ex)
                 (ex)]
                [strict-test
                 (#:t ex)
                 (#:? ex ex)
                 (#:= ex ex)
                 (#:eq ex ex ex)
                 (#:! test)
                 (#:x ex ex)
                 (#:do e)]
                [ex
                 (#:stx e e)
                 (#:src e e)
                 e]]}

The syntax of @racket[_t] determines the test executed.

@section{Single Value Tests}

The simplest form is a @racket[#:t _e] test, where @racketmodname[chk]
ensures that @racket[_e] evaluates to exactly one value that is not
@racket[#f].

@examples[#:eval e
          (chk #:t 5)
          (chk #:t #f)
          (chk #:t (values 1 2))
          (chk #:t (/ 1 0))]

This may be abbreviated by removing the @racket[#:t].

@examples[#:eval e
          (chk 5)
          (chk #f)]

A @racket[#:t _e] test is a special case of a @racket[#:? _p _e] test,
where @racketmodname[chk] ensures that @racket[_e] evaluates to
exactly one value and that the predicate @racket[_p] does not return
@racket[#f] on it. This form is robust to errors in @racket[_p], which
must evaluate to exactly one value that is a procedure of one
argument.

@examples[#:eval e
          (chk #:? even? 0)
          (chk #:? even? 1)
          (chk #:? even? (values 1 2))
          (chk #:? even? "two")
          (chk #:? #f 0)
          (chk #:? (values 3 4) 0)
          (chk #:? list-ref 0)
          (chk #:? (error 'boo) 0)]

@section{Expected Value Tests}

An @racket[#:= _actual _expected] test compares an actual evaluation
with an expected evaluation. If @racket[_expected] raises an
exception, then @racket[_actual] must also, with the same
@racket[exn-message]. Otherwise, the two expression must return the
same number of values, which must be @racket[equal?].

@examples[#:eval e
          (chk #:= 1 1)
          (chk #:= 1 "two")

          (chk #:= (/ 1 0) (error '/ "division by zero"))
          (chk #:= (/ 1 0) +inf.0)
          (chk #:= (/ 1 0) (error '/ "divided by zero"))
          
          (chk #:= (values 1 2) (values 1 2))
          (chk #:= (values 1 2) 1)
          (chk #:= 1 (values 1 2))
          (chk #:= (values 1 2) (values "one" "two"))]

An @racket[#:= _actual _expected] test may be abbreviated as
@racket[_actual _expected].

@examples[#:eval e
          (chk 1 1)
          (chk 1 "two")]

An @racket[#:= _actual _expected] test is a special case of a
@racket[#:eq _equal _actual _expected] test where @racket[_equal] is
@racket[equal?]. @racket[_equal] must evaluate to exactly one value,
which is a procedure of two arguments. If @racket[_equal] does not
evaluate as expected, then the test is considered a failure.

@examples[#:eval e
          (chk #:eq = 1 1)
          (chk #:eq eq? (cons 1 2) (cons 1 2))
          (chk #:eq (/ 1 0) 1 2)
          (chk #:eq #f 1 1)
          (chk #:eq add1 1 1)
          (chk #:eq + "one" "two")]

@section{Expected Exception Tests}

An @racket[#:x _actual _exn-pred] test is form when you expect
@racket[_actual] to evaluate to an exception, but cannot (or do not
want to) predict the exact error message. @racket[_exn-pred] must
evaluate to exactly one value. If that value is a string, then it must
be appear in the message of the exception that @racket[_actual]
raises. If that value is a regular expression, then it must match the
message of exception that @racket[_actual] raises. If the value is not
a string or regular expression, it must be a procedure of one argument
that does not return @racket[#f] on the exception that
@racket[_actual] raises.

@examples[#:eval e
          (chk #:x (/ 1 0) "division")
          (chk #:x (/ 1 0) "divided")
          
          (chk #:x (/ 1 0) #rx"di.ision")
          (chk #:x (/ 1 0) #rx"(one|two)")

          (chk #:x (/ 1 0) exn:fail?)
          (chk #:x (/ 1 0) exn:fail:syntax?)

          (chk #:x 0 exn:fail?)
          (chk #:x (/ 1 0) 0)
          (chk #:x (/ 1 0) cons)
          (chk #:x (/ 1 0) add1)]

@section{Meta Forms}

There are two meta-forms provided by @racket[chk].

A @racket[#:! _t] test, where @racket[_t] is another test, represents
the expectation that test @racket[_t] will fail.

@examples[#:eval e
          (chk #:! #:t #f)
          (chk #:! #:t #t)
          
          (chk #:! #:= 1 0)
          (chk #:! #:= 1 1)
          
          (chk #:! #:x (/ 1 0) exn:fail:syntax?)
          (chk #:! #:x (/ 1 0) "division")]

A @racket[#:do _e] form is equivalent to @racket[_e]. (This does not
appear to be useful, unless you read the next section!)

@examples[#:eval e
          (chk #:do (printf "Hey, listen!"))]

@section{Combining Tests}

A @racket[chk] form can contain any number of sub-forms. This is why
@racket[#:do] can be useful. They may be wrapped in parentheses, or
not.

@examples[#:eval e
          (chk 1 1
               2 2
               #t)
          (chk #:= 1 1
               #:! #:t #f
               #:? even? 2)
          (chk [#:= 1 1]
               [#:! #:t #f]
               [#:? even? 2])
          (chk #:! [#:t #f]
               #:! [#:= 1 2])
          (chk #:= (+ 2 3) 5
               #:do (define x 1)
               #:= (+ 5 x) 6)]

@section{Test Sets}

@defform[(chk* body ...+)]{

Sometimes you want to ensure that if one test fails, then no other
tests are evaluated within a block. @racket[chk*] does this for all
tests within @racket[(let () body ...)].

@examples[#:eval e
          (chk*
           (define x "one")
           (chk #:? number? x
                #:do (printf "Hey, listen!\n")
                #:? even? x)
           (chk #:? odd? (add1 x)))]}

@section{Contextual Failure Reporting}

@defform[(with-chk ([key value] ...) body ...+)]{

The failure reporting format used by @racket[chk] is sensitive to the
dynamic context it is run within. The @racket[with-chk] form allows
you to add details to all tests within @racket[(let () body ...)].

@examples[#:eval e
          (with-chk (['author "jay"])
            (chk #:= 1 1
                 #:= 1 2)
            (with-chk (['suite "1337"])
              (chk #:? even? "two")))]

}

@defthing[chk-inform! symbol?]{

A special key for use with @racket[with-chk] that is not printed, but
instead must be matched with a value that is a procedure of one
argument. If there is an error, then the function will be called with
all of the failure details.

@examples[#:eval e
          (with-chk (['author "jay"]
                     [chk-inform! (λ (d) (printf "Woah:\n~v\n" d))])
            (chk #:= 1 1
                 #:= 1 2))]

}
