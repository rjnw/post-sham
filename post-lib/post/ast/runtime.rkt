#lang racket

(require "core.rkt")
(provide (all-defined-out))

(define current-functor (make-parameter '()))
(define current-functor-input (make-parameter '()))

(define (check-type value type-sig)
  value)
(define (functor-input value)
  value)
