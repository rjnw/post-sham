#lang racket

(provide (all-defined-out))

(define module-name (make-parameter #f))
(define module-local-name (make-parameter #f))
(define context-name (make-parameter #f))

(define functor-name (make-parameter #f))
(define functor-local-name (make-parameter #f))

(define let-vars (make-parameter #f))
