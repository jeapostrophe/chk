#lang racket/base
(require racket/match
         racket/list
         racket/function
         syntax/parse/define
         (for-syntax racket/base
                     syntax/location
                     syntax/parse
                     syntax/srcloc)
         syntax/quote
         syntax/srcloc
         rackunit/log
         racket/pretty
         racket/format
         racket/string
         racket/port)

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

(define with-chk-param (make-parameter null))

(define-simple-macro (with-chk ([k v] ...) e ...+)
  (parameterize ([with-chk-param
                   (cons (list (cons k v) ...)
                         (with-chk-param))])
    (let () e ...)))

(define (flatten-with-chk-param)
  (append* (reverse (with-chk-param))))

(define (*chk-escape! v)
  (unless v
    (display-info (flatten-with-chk-param)))
  (test-log! v))

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
     (*check-one-value-matching-? "procedure of 2 args" valid-compare? eq))
   (match-define (res:values _ (list the-eq)) eq)
   (with-chk (['kind "compare"]
              ['compare the-eq]
              ['actual a]
              ['expected e])
     (match e
       [(? res:exn?)
        (chk*
         (*check? "exception?" (res:exn? a))
         (*check= "exn-message" string=?
                  (exn-message (res:exn-x a))
                  (exn-message (res:exn-x e)))
         (*chk-succ!))]
       [(? res:values?)
        (chk*
         (*check? "values" (res:values? a))
         (define as (res:values-vs a))
         (define es (res:values-vs e))
         (*check= "how many values" = (length as) (length es))
         (for ([a (in-list as)]
               [e (in-list es)]
               [i (in-naturals)])
           (*check= (format "value ~a" i) (protect "check" the-eq) a e))
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
       [(? res:exn?) (void)]
       [(? res:values?)
        (*check-one-value-matching-? "exception expectation"
                                     valid-exn-expect? e)]))
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

(begin-for-syntax
  (define-syntax-class test-expr-inner
    #:attributes (stx e)
    [pattern (#:stx s e)
             #:attr stx #'(quote-syntax/keep-srcloc s)]
    [pattern (#:src s e)
             #:attr stx #`(datum->syntax #f 'e s)]
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
             #:attr unit (syntax/loc #'a (check? D-identity a.exp))]
    [pattern (~seq #:? ?:test-expr a:test-expr)
             #:attr unit (syntax/loc #'a (check? ?.exp a.exp))]
    [pattern (~seq #:x a:test-expr b:test-expr)
             #:attr unit (syntax/loc #'a (check-exn a.exp b.exp))]
    [pattern (~seq #:eq eq?:test-expr a:test-expr b:test-expr)
             #:attr unit (syntax/loc #'a (check eq?.exp a.exp b.exp))]
    [pattern (~seq #:= a:test-expr b:test-expr)
             #:attr unit (syntax/loc #'a (check D-equal? a.exp b.exp))]
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

;; this should match any string which is a valid racket identifier
;; and does not contain the '=' character.
(define id-regexp "[^]\\[\\(\\){}\",\'`;#\\|\\\\=]+")
(define key-val-regexp (regexp (string-append id-regexp "=.+")))

(define (get-filters)
  (for/fold ([names empty] [filters empty] [files empty] [lines empty])
            ([arg (vector->list (current-command-line-arguments))])
    (cond
      [(regexp-match-exact? #rx"[Ff][Ii][Ll][Ee]=.+" arg)
       (define files (cons (regexp (substring arg 5)) files))
       (values names filters files lines)]
      [(regexp-match-exact? #rx"[Ll][Ii][Nn][Ee]=.+" arg)
       (define lines (cons (string->number (substring arg 5)) lines))
       (values names filters files lines)]
      [(regexp-match-exact? key-val-regexp arg)
       (define split (string-split arg "="))
       (define arg-name (string->symbol (car split)))
       (define arg-val (cadr split))
       (values names (cons (cons arg-name arg-val) filters) files lines)]
      [else
       (values (cons (regexp arg) names) filters files lines)])))

(define (arguments-say-to-run file line)
  (define-values
    (names-to-run k/v-filters files-to-run lines-to-run)
    (get-filters))
  (define with-chk-hash
    (for/fold ([ht #hasheq()]) ([chk-var (flatten-with-chk-param)])
      (hash-set ht (car chk-var) (cdr chk-var))))
  (and
   (andmap (lambda (k/v)
             (define hash-fail (gensym))
             (define val-or-fail (hash-ref with-chk-hash (car k/v) hash-fail))
             (and (not (eq? val-or-fail hash-fail))
                  (equal? (~a val-or-fail) (cdr k/v))))
           k/v-filters)
   (or (empty? files-to-run)
       (ormap (lambda (file-rex)
                (regexp-match? file-rex (if file (path->string file) "")))
              files-to-run))
   (or (empty? lines-to-run)
       (ormap (lambda (ln) (equal? ln line)) lines-to-run))
   (or (empty? names-to-run)
       (ormap (lambda (name-rex)
                (regexp-match? name-rex (hash-ref with-chk-hash 'name "")))
              names-to-run))))

(define-syntax (chk stx)
  (syntax-parse stx
    [(_ e:test ...)
     (quasisyntax/loc stx
       (when (arguments-say-to-run
              #,(syntax-source-file-name stx)
              #,(syntax-line stx))
         (let () e.unit ...)))]))

(define-simple-macro (chk* e      ...+) (*chk* (λ () e ...)))
(define-simple-macro (*chk-invert e   ) (*chk-invert* (λ () e)))

(provide chk
         chk*
         with-chk
         chk-inform!)
