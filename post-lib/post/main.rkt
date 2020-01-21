#lang racket

(require (all-defined-out))




(define-syntax (post-module stx)
  #'42)

(define-syntax (post-struct stx)
  (syntax-parse stx
    [(_ sig:signature () ...)]))
