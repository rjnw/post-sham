#lang racket

(provide (all-defined-out))

(define interpreting? (make-parameter #f))
(define interpreter (make-parameter #f))
(define interpreter-environment (make-parameter #f))
(define interpreter-module-context (make-parameter #f))
(define interpreter-type-checker (make-parameter #f))
