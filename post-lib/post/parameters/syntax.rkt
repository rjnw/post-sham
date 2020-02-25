#lang racket

(require racket/stxparam
         syntax/parse/define)

(provide record union datatype forall function value rkt lit ->)

(define-syntax-parser syntax-constructors
  [(_ s:id ...)
   #`(begin
       (define-syntax-parameter s
         (λ (stx)
           (raise-syntax-error 'post:syntax
                               "invalid use of post syntax" stx))) ...)])

(syntax-constructors record function union datatype forall rkt lit value)

(define-syntax-parameter -> (make-rename-transformer #'function))

(module+ test
  (syntax-parameterize ([record (λ (stx) #'32)])
    record))
