#lang racket/base

(require
  (for-syntax racket/base syntax/parse)
  chk)

(define-syntax (run-test stx)
  (syntax-parse stx
    [(_ (args ...) e ...+)
     (quasisyntax/loc stx
       (let ([result (open-output-string)])
         (parameterize ([current-error-port result]
                        [current-command-line-arguments (vector args ...)])
           (begin e ... (get-output-string result)))))]))

(module+ test
  (struct ts (a b) #:transparent)
  (define (go)
    (with-chk (['name "foo"]
               ['number 6]
               ['struct (ts 'a 5)])
      (chk 1 2)))
  ;; Check error
  (when (zero? (string-length (run-test ("foo") (go))))
    (eprintf "Test 1 failed\n"))
  ;; Check error
  (when (zero? (string-length (run-test ("number=6" "struct=(ts 'a 5)") (go))))
    (eprintf "Test 2 failed\n"))
  ;; Check no error
  (unless (zero? (string-length (run-test ("number=7") (go))))
    (eprintf "Test 3 failed\n"))
  ;; Check no error
  (unless (zero? (string-length (run-test ("zog") (go))))
    (eprintf "Test 4 failed\n"))
  ;; Check error - multiple names, but one match
  (when (zero? (string-length (run-test ("foo" "zog") (go))))
    (eprintf "Test 5 failed\n"))
  ;; Check no error - name matches, but key/val doesn't
  (unless (zero? (string-length (run-test ("foo" "number=7") (go))))
    (eprintf "Test 6 failed\n"))
  ;; Check error - name, file, and line all match
  (when (zero? (string-length (run-test ("foo" "file=test-args.rkt" "line=39") (go))))
    (eprintf "Test 7 failed\n"))
  ;; Check no error - wrong line number
  (unless (zero? (string-length (run-test ("foo" "file=test-args.rkt" "line=2") (go))))
    (eprintf "Test 8 failed\n"))
  ;; Check no error - wrong file
  (unless (zero? (string-length (run-test ("foo" "file=foobar.rkt" "line=21") (go))))
    (eprintf "Test 9 failed\n"))

  (define (go2)
    (chk 2 3))
  ;; Check error
  (when (zero? (string-length (run-test () (go2))))
    (eprintf "Test 10 failed\n"))
  ;; Check error - should print since file name is correct
  (when (zero? (string-length (run-test ("file=test-args.rkt") (go2))))
    (eprintf "Test 11 failed\n"))
  ;; Check no error
  (unless (zero? (string-length (run-test ("foo") (go2))))
    (eprintf "Test 12 failed\n"))
  )
