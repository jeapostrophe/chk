#lang racket/base
(require racket/match
         racket/list
         racket/function
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     syntax/srcloc)
         syntax/quote
         syntax/srcloc
         rackunit/log
         racket/pretty
         racket/format
         racket/string
         racket/port)

;; Display code
(define chk-inform! (gensym 'chk-inform!))
(define (format-v v)
  (match v
    [(? syntax?)
     (cond
       [(source-location-known? v)
        (string-append (source-location->prefix v)
                       (format-v (syntax->datum v)))]
       [else
        (format-v (syntax-e v))])]
    [(? exn:fail?)
     (with-output-to-string
       (λ ()
         (parameterize ([current-error-port (current-output-port)])
           ((error-display-handler) (exn-message v) v))))]
    [(res:exn stx x)
     (format "~afrom\n~a" (format-v x) (format-v stx))]
    [(res:values stx x)
     (format "~a\nfrom\n~a"
             (format-v
              (match x
                [(list x) x]
                [_ (cons 'values x)]))
             (format-v stx))]
    [_
     (pretty-format #:mode 'write v)]))
(define (display-info d)
  (eprintf "FAILURE\n")
  (unless (null? d)
    (define key-symbols (map car d))
    (define key-strings (map symbol->string key-symbols))
    (define max-length (apply max (map string-length key-strings)))
    (for ([the-d (in-list d)])
      (match-define (cons k v) the-d)
      (cond
        [(eq? k chk-inform!)
         (v d)]
        [else
         (define k-header
           (format "~a : "
                   (~a #:min-width max-length
                       #:align 'left
                       k)))
         (define k-space-header
           (make-string (string-length k-header) #\space))
         (eprintf "~a" k-header)
         (for ([vl (in-list (string-split (format-v v) "\n"))]
               [i (in-naturals)])
           (eprintf "~a~a\n"
                    (if (zero? i) "" k-space-header)
                    vl))])))
  (eprintf "\n"))

;; Recording check details
(define with-chk-param (make-parameter null))

(define-simple-macro (with-chk ([k v] ...) e ...+)
  (parameterize ([with-chk-param
                   (cons (list (cons k v) ...)
                         (with-chk-param))])
    (let () e ...)))

(define (*chk-escape! v)
  (unless v
    (display-info
     (append*
      (reverse
       (with-chk-param)))))
  (test-log! v))

;; Context manipulation
(define current-chk-escape (make-parameter *chk-escape!))
(define (*chk-fail!) ((current-chk-escape) #f))
(define (*chk-succ!) ((current-chk-escape) #t))

(define (*chk-invert* t)
  (define (invert p)
    (let ([old (p)])
      (λ (x) (old (not x)))))
  (parameterize ([current-chk-escape (invert current-chk-escape)])
    (with-chk (['inverted #t])
      (t))))

(define (*chk* t)
  (let/ec esc
    (define (escape p)
      (let ([old (p)])
        (λ (x)
          (old x)
          (unless x
            (esc)))))
    (parameterize ([current-chk-escape (escape current-chk-escape)])
      (t))))

(define-syntax-rule (*chk-invert e)
  (*chk-invert* (λ () e)))

;; Evaluating expressions
(struct res (stx) #:prefab)
(struct res:exn res (x) #:prefab)
(struct res:values res (vs) #:prefab)

(define-syntax-rule (D e stx)
  (let ([stx-id stx])
    (with-handlers ([exn? (λ (x) (res:exn stx-id x))])
      (call-with-values (λ () e)
                        (λ vs (res:values stx-id vs))))))

(define D-identity (D identity #'identity))
(define D-equal? (D equal? #'equal?))

;; Core checks
(define (*check= p = a e)
  (with-chk (['part p]
             ['actual a]
             ['expected e])
    (unless (= a e)
      (*chk-fail!))))

(define (*check? p v)
  (with-chk (['predicate p])
    (unless v
      (*chk-fail!))))

(define (protect p f)
  (λ args
    (with-handlers ([exn:fail?
                     (λ (x)
                       (with-chk (['part (format "evaluating ~a" p)]
                                  ['exn x])
                         (*chk-fail!)))])
      (apply f args))))

(define (*check-one-value v)
  (*check? "not exception" (res:values? v))
  (define v-vs (res:values-vs v))
  (*check= "how many values" = (length v-vs) 1)
  (car v-vs))

(define (*check-one-value-matching-? p ? v)
  (*check? p (? (*check-one-value v))))

(define (valid-compare? x)
  (and (procedure? x)
       (procedure-arity-includes? x 2)))

(define (check eq a e)
  (chk*
   (with-chk (['kind "valid compare expected"]
              ['actual eq])
     (*check-one-value-matching-?
      "procedure of 2 args" valid-compare? eq))
   (match-define (res:values _ (list the-eq)) eq)
   (with-chk (['kind "compare"]
              ['compare the-eq]
              ['actual a]
              ['expected e])
     (match e
       [(? res:exn?)
        (chk*
         (*check? "exception?" (res:exn? a))
         (*check= "exn-message"
                  string=?
                  (exn-message (res:exn-x a))
                  (exn-message (res:exn-x e)))
         (*chk-succ!))]
       [(? res:values?)
        (chk*
         (*check? "values" (res:values? a))
         (define as (res:values-vs a))
         (define es (res:values-vs e))
         (*check= "how many values"
                  =
                  (length as)
                  (length es))
         (for ([a (in-list as)]
               [e (in-list es)]
               [i (in-naturals)])
           (*check=
            (format "value ~a" i)
            (protect "check" the-eq)
            a
            e))
         (*chk-succ!))]))))

(define (valid-pred? x)
  (and (procedure? x)
       (procedure-arity-includes? x 1)))

(define (check? ? a)
  (chk*
   (with-chk (['kind "valid predicate expected"]
              ['actual ?])
     (*check-one-value-matching-?
      "procedure of 1 arg" valid-pred? ?))
   (match-define (res:values _ (list the-?)) ?)
   (with-chk (['kind "predicate"]
              ['predicate the-?]
              ['actual a])
     (chk*
      (define v (*check-one-value a))
      (with-chk (['value v])
        (*check? "not false" ((protect "predicate" the-?) v))
        (*chk-succ!))))))

(define (*chk-regexp-match s r)
  (and (regexp-match r s) #t))

(define (valid-exn-expect? x)
  (or (string? x) (regexp? x)
      (and (procedure? x)
           (procedure-arity-includes? x 1))))

(define (check-exn a e)
  (chk*
   (with-chk (['kind "valid exception expected"]
              ['actual e])
     (match e
       [(? res:exn?)
        (void)]
       [(? res:values?)
        (*check-one-value-matching-?
         "exception expectation" valid-exn-expect? e)]))
   (with-chk (['kind "exception"])
     (match e
       [(? res:exn?)
        (check D-equal? a e)]
       [(res:values _ (list e))
        (with-chk (['actual a])
          (*check? "exception" (res:exn? a))
          (define x (res:exn-x a))
          (match e
            [(? string?)
             (*check= "exn-message" *chk-regexp-match
                      (exn-message x)
                      (regexp (regexp-quote e)))]
            [(? regexp?)
             (*check= "exn-message" *chk-regexp-match
                      (exn-message x)
                      e)]
            [(? procedure?)
             (*check? (object-name e) ((protect "exn predicate" e) x))])
          (*chk-succ!))]))))

;; Syntax
(begin-for-syntax
  (define-syntax-class test-expr-inner
    #:attributes (stx e)
    [pattern (#:stx s e)
             #:attr stx #'(quote-syntax/keep-srcloc s)]
    [pattern (#:src s e)
             #:attr stx
             #`(datum->syntax #f 'e s)]
    [pattern e:expr
             #:attr stx #'(quote-syntax/keep-srcloc e)])

  (define-syntax-class test-expr
    #:attributes (exp)
    [pattern t:test-expr-inner
             #:attr exp #'(D t.e t.stx)])

  (define-splicing-syntax-class strict-test
    #:commit
    #:attributes (unit)
    [pattern (~seq #:! t:test)
             #:attr unit #'(*chk-invert t.unit)]
    [pattern (~seq #:t a:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check? D-identity a.exp))]
    [pattern (~seq #:? ?:test-expr a:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check? ?.exp a.exp))]
    [pattern (~seq #:x a:test-expr b:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check-exn a.exp b.exp))]
    [pattern (~seq #:eq eq?:test-expr a:test-expr b:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check eq?.exp a.exp b.exp))]
    [pattern (~seq #:= a:test-expr b:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check D-equal? a.exp b.exp))]
    [pattern (~seq #:do e:expr)
             #:attr unit #'e])

  (define-splicing-syntax-class test
    #:commit
    #:attributes (unit)
    (pattern c:strict-test
             #:attr unit #'c.unit)
    (pattern (c:strict-test)
             #:attr unit #'c.unit)
    [pattern (~seq a:test-expr b:test-expr)
             #:with (c:strict-test) (syntax/loc #'a (#:= a b))
             #:attr unit #'c.unit]
    [pattern (~seq a:test-expr)
             #:with (c:strict-test) (syntax/loc #'a (#:t a))
             #:attr unit #'c.unit]))

(define-simple-macro (chk e:test ...)
  (let () e.unit ...))
(define-simple-macro (chk* e ...+)
  (*chk* (λ () e ...)))

(provide chk
         chk*
         with-chk
         chk-inform!)

(module+ test
  (chk #:! #:x (/ 1 0) "division"))

(module+ test
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
