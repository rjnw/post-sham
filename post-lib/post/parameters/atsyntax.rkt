#lang racket

(provide (all-defined-out))

(define record-name (make-parameter #f))
(define record-local-name (make-parameter #f))
(define context-name (make-parameter #f))

(define function-name (make-parameter #f))
(define function-local-name (make-parameter #f))

(define let-vars (make-parameter #f))

(define signature-gen-name (make-parameter #f))
(define signature-orig-name (make-parameter #f))
