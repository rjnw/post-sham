#lang racket

(require racket/stxparam
         syntax/parse/define)

(provide record union datatype forall function rkt lit)

(define-syntax-parser syntax-constructors
  [(_ s:id ...)
   #`(begin
       (define-syntax-parameter s
         (λ (stx)
           (raise-syntax-error 'post:signature
                               "invalid use of post constructor" stx))) ...)])

(syntax-constructors record union datatype forall function rkt lit)


(module+ test
  (syntax-parameterize ([record (λ (stx) #'32)])
    record))
