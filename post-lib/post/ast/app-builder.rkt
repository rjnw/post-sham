#lang racket

(require "core.rkt")

(provide (all-defined-out))

(define generic-functor
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (void))))

(define generic-module
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))

(define generic-record
  (make-keyword-procedure
   (λ (kws kw-args r . rst)
     (void))))

(define hoas-functor
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (void))))

(define hoas-module
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))
