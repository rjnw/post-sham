#lang racket

(provide (except-out (all-defined-out)
                     runtime-error))

(define ((runtime-error name) . args)
  (error 'post:runtime "runtime parameter ~a not initialized" name))
(define runtime-eval (make-parameter (runtime-error 'runtime-eval)))
(define runtime-app (make-parameter
                     (make-keyword-procedure
                      (λ (kws kw-args f . args)
                        (keyword-apply f kws kw-args args)))))
(define runtime-eval-literal (make-parameter (runtime-error 'runtime-eval-literal)))
(define runtime-eval-rkt (make-parameter (runtime-error 'runtime-eval-rkt)))
(define runtime-eval-environment (make-parameter (runtime-error 'runtime-eval-environment)))
(define runtime-record-context (make-parameter (runtime-error 'runtime-record-context)))
(define runtime-type-checker (make-parameter (runtime-error 'runtime-type-checker)))
