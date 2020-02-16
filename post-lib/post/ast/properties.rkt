#lang racket

(provide (all-defined-out))

(define key-kind '#%post-kind)
(define key-type '#%post-type)
(define key-possible-types '#%post-possible-types)
(define key-expr '#%post-expr)
(define key-functor '#%post-functor)
(define key-module '#%post-module)

(define (type? stx)
  (syntax-property stx key-kind))
(define (typeof stx)
  (syntax-property stx key-type))
(define (possible-types stx)
  (syntax-property stx key-possible-types))


(define (set-kind! stx k)
  (syntax-property stx key-kind k #t))
(define (set-type! stx type)
  (syntax-property stx key-type type #t))

(define (set-possible-types! stx types)
  (syntax-property stx key-possible-types types))
(define (add-possible-type! stx type)
  (if (possible-types stx)
      (set-possible-types stx (cons type (possible-types stx)))
      (set-possible-types stx (list type))))
