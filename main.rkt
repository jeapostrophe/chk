#lang racket/base
(require racket/match
         racket/list
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse)
         syntax/quote
         syntax/srcloc
         rackunit/log)

(define-syntax-rule (D e stx)
  (let ([stx-id stx])
    (with-handlers ([exn? (λ (x) (res:exn stx-id x))])
      (call-with-values (λ () e)
                        (λ vs (res:values stx-id vs))))))

(define chk-key (make-continuation-mark-key 'chk))

(define-simple-macro (with-chk ([k v] ...) e ...+)
  (with-continuation-mark chk-key (list (cons k v) ...)
    (let () e ...)))

(require racket/pretty
         racket/format
         racket/string
         racket/port)
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
     (pretty-format v)]))
(define (display-info d)
  (eprintf "FAILURE\n")
  (unless (null? d)
    (define key-symbols (map car d))
    (define key-strings (map symbol->string key-symbols))
    (define max-length (apply max (map string-length key-strings)))
    (for ([d (in-list d)])
      (match-define (cons k v) d)
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
                 vl))))
  (eprintf "\n"))

(define (*chk-escape! v)
  (unless v
    (display-info
     (append*
      (reverse
       (continuation-mark-set->list
        (current-continuation-marks)
        chk-key)))))
  (test-log! v))

(define current-chk-escape (make-parameter *chk-escape!))
(define (*chk-fail!) ((current-chk-escape) #f))
(define (*chk-succ!) ((current-chk-escape) #t))

(define (*chk-invert* t)
  (define (invert p)
    (let ([old (p)])
      (λ (x) (old (not x)))))
  (parameterize ([current-chk-escape (invert current-chk-escape)])
    (t)))

(define (*chk* t)
  (let/ec esc
    (define (escape p)
      (let ([old (p)])
        (λ (x) (old x) (esc))))
    (parameterize ([current-chk-escape (escape current-chk-escape)])
      (t))))

(define-syntax-rule (*chk-invert e)
  (*chk-invert* (λ () e)))

(struct res (stx))
(struct res:exn res (x))
(struct res:values res (vs))

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

(define (check-equal? a e)
  (with-chk (['kind "equal?"]
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
          (*check= (format "value ~a" i)
                   equal?
                   a
                   e))
        (*chk-succ!))])))

(define (check-not-false a)
  (with-chk (['kind "not false"]
             ['actual a])
    (chk*
     (*check? "values" (res:values? a))
     (define as (res:values-vs a))
     (cond
       [(= 1 (length as))
        (*check? "not false" (first as))
        (*chk-succ!)]
       [else
        (*chk-succ!)]))))

(define (check-exn a e)
  (chk*
   (with-chk (['kind "valid exception expected"]
              ['actual e])
     (match e
       [(? res:exn?)
        (void)]
       [(? res:values?)
        (define es (res:values-vs e))
        (*check= "how many values" = (length es) 1)
        (define x (first es))
        (*check? "string, regexp, or procedure of 1 arg"
                 (or (string? x) (regexp? x)
                     (and (procedure? x)
                          (procedure-arity-includes? x 1))))]))
   (with-chk (['kind "exception"])
     (match e
       [(? res:exn?)
        (check-equal? a e)]
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
             (*check? (object-name e) (e x))]))]))))

(define (*chk-regexp-match s r)
  (and (regexp-match r s) #t))

(begin-for-syntax
  (define-syntax-class test-expr
    #:attributes (stx e)
    [pattern e:expr
             #:attr stx #'(quote-syntax/keep-srcloc e)]
    [pattern (#:stx s e)
             #:attr stx #'(quote-syntax/keep-srcloc s)]
    [pattern (#:src s e)
             #:attr stx
             #`(quote-syntax/keep-srcloc
                #,(datum->syntax #'e #'e #'s))])

  (define-splicing-syntax-class strict-test
    #:commit
    #:attributes (unit)
    [pattern (~seq #:! t:test)
             #:attr unit #'(*chk-invert t.unit)]
    [pattern (~seq #:t a:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check-not-false (D a.e a.stx)))]
    [pattern (~seq #:x a:test-expr b:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check-exn (D a.e a.stx) (D b.e b.stx)))]
    [pattern (~seq #:= a:test-expr b:test-expr)
             #:attr unit
             (syntax/loc #'a
               (check-equal? (D a.e a.stx) (D b.e b.stx)))])

  (define-splicing-syntax-class test
    #:commit
    #:attributes (unit)
    (pattern c:strict-test
             #:attr unit #'c.unit)
    (pattern (c:strict-test)
             #:attr unit #'c.unit)
    [pattern (~seq a:expr b:expr)
             #:with (c:strict-test) (syntax/loc #'a (#:= a b))
             #:attr unit #'c.unit]
    [pattern (~seq a:expr)
             #:with (c:strict-test) (syntax/loc #'a (#:t a))
             #:attr unit #'c.unit]))

(define-simple-macro (chk e:test ...)
  (begin e.unit ...))
(define-simple-macro (chk* e ...+)
  (*chk* (λ () e ...)))

(provide chk
         chk*
         with-chk)

(module+ test
  (chk
   1 1
   1 0
   #:! 1 0
   #:! 1 1

   #:! #:! #:! 1 0
   #:! #:! 1 1

   #:! (/ 1 0) +inf.0
   (/ 1 0) +inf.0

   (/ 1 0) (/ 1 0)
   #:! (/ 1 0) (/ 1 0)

   (error 'xxx "a") (error 'xxx "a")
   #:! (error 'xxx "a") (error 'xxx "b")

   #:! #:t (/ 1 0)
   #:t (values 0 1)
   #:t (values #f 1)
   #:! #:t (values #f 1)

   1 1
   2 2
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

   (values 1 2) (values 1 2)
   (values 1 2) (values 2 3)
   #:! (values 1 2) (values 2 3)
   (values 1 2) 3
   #:! (values 1 2) 3
   #:! 3 (values 1 2)
   (quotient/remainder 10 3) (values 3 1)

   #:= 1 1
   [#:x (/ 1 0) "division"]
   [#:! #f]
   [#:t 1]
   [#:= 1 1]

   ;; XXX test with-chk
   ;; XXX test chk*
   ;; XXX test stx
   ;; XXX test src

   ))
