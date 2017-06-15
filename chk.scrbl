#lang scribble/manual
@(require (for-label chk
                     racket/base
                     syntax/srcloc)
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

@section{Filtering Tests with Command-line Arguments}

The key/value pairs specified in the current context by
@racket[with-chk] can also be used to group tests into catagories
which can be selected and run by supplying command-line arguments to
the module running the tests. Before each test is run,
@racketmodname[chk] will use the value of
@racket[current-command-line-arguments] to construct a list of names
and key/value pairs.

Key/value pairs are specified by arguments of the form @tt{key=value}.
If a non-zero amount of key/value pairs are specified, then only tests
which match the set of key/value pairs specified on the command line
will be executed.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "foo=bar")])
            (with-chk (['foo "bar"])
              (chk #:= 1 2))
            (chk #:= 2 3))]

The command-line argument @racket["foo=bar"] corresponds with the
racket pair @racket[('foo . "bar")]. In the above example, the former
test runs because it has been called inside of an expression where the
key @racket['foo] matches the value @racket["bar"], but the latter
test is skipped.

Keys read from the command line are always
treated as symbols and compared using @racket[eq?]. Values are
processed using @racket[read] and are always compared using
@racket[equal?]. Because of this, it is possible to filter against
values more complex than just strings or symbols.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "data='(list 1 2 3)")])
            (with-chk (['data (list 1 2 3)])
              (chk #:= 1 2)))]

If multiple key/value pairs are present on the command line, then
tests will only run if they match against @italic{all} specified key/value
pairs.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "foo=bar" "number=6")])
            (with-chk (['foo "bar"])
              (chk #:= 1 2)
              (with-chk (['number 6])
                (chk #:= 2 3))))]

In the above example, the former test does not run because it is
defined in a context in which only the key @racket['foo] has a value.
The latter test, however, runs because its context contains both the
keys @racket['foo] and @racket['number] (and their values match the
values specified in the arguments).

The keys @racket['file] and @racket['line] are special cases when
present in the command-line arguments. They are case insensitive and
are matched against the file name and line number of current test.
@racketmodname[chk] tracks the values of these keys internally, so it
is not possible to spoof the current file or line values by calling
@racket[(with-chk (['file "foo.rkt"] ['line 100]) ...)] or anything
similar.

Any command-line arguments which do not fit the @tt{name=value} pattern
are treated as regular expressions and used as a set of names to run.
In the execution context of the test, these names are compared against
the value corresponding with the @racket['name] key. Unlike with
key/value pairs, a test's name needs only to match against one of the
names specified on the command line in order to be run. This way,
multiple tests can be specified to be run. Tests with no
@racket['name] key specified will only be run in the case that no
names are present in the command-line arguments.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "test1" "test2")])
            (with-chk (['name "test1"])
              (chk #:= 1 2))
            (with-chk (['name "test2"])
              (chk #:= 2 3))
            (chk #:= 3 4))]

If both names and key/value pairs are present in the command-line
arguments, then a test must satisfy both predicates in order to be
run.

@examples[#:eval e
          (parameterize ([current-command-line-arguments (vector "test1" "foo=bar")])
            (with-chk (['name "test1"])
              (chk #:= 1 2))
            (with-chk (['foo "bar"])
              (chk #:= 2 3)))]

In the above example, the command-line arguments are specifying that
only tests which match the name @tt{test1} @italic{and} the key/value pair
@tt{foo=bar} should be run. Neither test satisfies both conditions, so
neither test is run.

@section{Controlling Source Location and Syntax Display}

Everywhere in the preceeding discussion where we referred to an
expression evaluating to something, that position may be replaced with
@racket[(#:stx _stx _e)] or @racket[(#:src _src _e)]. In both cases,
the expression evaluated is @racket[_e]. However, if there is an
error, then a different syntax (@racket[_stx], which is simply a
datum) or source location (@racket[_src] which is evaluate to
something that is a @racket[source-location?]) is reported.

@examples[#:eval e
          (chk #:= (#:stx one 1) (#:stx two 2))
          (chk #:= (#:src #'here 1) 2)
          (chk #:= (#:src '(there 99 1 22 1) "one") "two")]

This is intended to be used by macros that produce @racket[chk]s.
