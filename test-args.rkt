#lang racket/base

(require chk
         syntax/parse/define
         racket/format)

(define-simple-macro (with-cmd (args ...) e)
  (parameterize ([current-command-line-arguments (vector args ...)])
    (let () e)))

(define-simple-macro (run-test (args ...) e)
  (with-cmd (args ...)
    (let ([result (open-output-string)])
      (parameterize ([current-error-port result])
        (begin e (get-output-string result))))))

(define-simple-macro (check-error (args ...) e)
  (when (zero? (string-length (run-test (args ...) e)))
    (eprintf "TEST FAILED: (check-error ~s ~a)\n"
             (map ~a (list args ...)) (quote e))))

(define-simple-macro (check-no-error (args ...) e)
  (let ([result (run-test (args ...) e)])
    (unless (zero? (string-length result))
      (eprintf "TEST FAILED: (check-no-error ~s ~a)\n~a\n"
               (map ~a (list args ...)) (quote e) result))))

(module+ test
  (define (go)
    (with-chk (['name "foo"]
               ['number 6])
      (chk 1 2)))
  (check-error ("foo") (go))
  (check-error ("number=6") (go))
  (check-no-error ("number=7") (go))
  (check-no-error ("zog") (go))
  (check-error ("foo" "zog") (go))
  (check-no-error ("foo" "number=7") (go))
  (check-error ("foo" "file=test-args.rkt" "line=32") (go))
  (check-no-error ("foo" "file=test-args.rkt" "line=1") (go))
  (check-no-error ("foo" "file=foobar.rkt" "line=32") (go))
  (define (go2)
    (chk 2 3))
  (check-error () (go2))
  (check-error ("file=test-args.rkt") (go2))
  (check-no-error ("foo") (go2))
  )
