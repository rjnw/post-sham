#lang racket

(require "core.rkt"
         "metadata.rkt")
(require (prefix-in r: racket))
(provide (all-defined-out))

(define decl? ast:decl?)
(define (deep-decl? d)
  (and (decl? d)
       (r:symbol? (ast:decl-name d))
       (deep-signature? (ast:decl-sig d))))

(define (mark-deep-wf-sig! s [v #t])
  (if (and (ast:ast-md s)
           (metadata:ast:signature:signature? (ast:ast-md s)))
      (metadata:ast:signature:set-signature-wf?! (ast:ast-md s) v)
      (ast:set-ast-md! s (metadata:ast:signature:signature v)))
  s)

(define signature? ast:signature:signature?)
(define (deep-signature? s)
  (define (mark-wf-sig! check s)
    (mark-deep-wf-sig! s (check s)))
  (and (signature? s)
       (or (and (metadata:ast:signature:signature? (ast:ast-md s))
                (metadata:ast:signature:signature-wf? (ast:ast-md s)))
           (cond
             [(kind? s) (mark-wf-sig! deep-kind? s)]
             [(list? s) (mark-wf-sig! deep-list? s)]
             [(cons? s) (mark-wf-sig! deep-cons? s)]
             [(record? s) (mark-wf-sig! deep-record? s)]
             [(module? s) (mark-wf-sig! deep-module? s)]
             [(functor? s) (mark-wf-sig! deep-functor? s)]
             [else (or (symbol? s)
                       (string? s)
                       (integer? s))]))))

(define kind? ast:signature:kind?)
(define (deep-kind? s)
  (and (kind? s)
       (deep-signature? (ast:signature:kind-type s))))

(define void? ast:signature:void?)
(define symbol? ast:signature:symbol?)
(define string? ast:signature:string?)
(define integer? ast:signature:integer?)
(define list? ast:signature:list?)
(define (deep-list? s)
  (and (list? s)
       (deep-signature? (ast:signature:list-element s))))
(define cons? ast:signature:cons?)
(define (deep-cons? s)
  (and (cons? s)
       (deep-signature? (ast:signature:cons-a s))
       (deep-signature? (ast:signature:cons-d s))))
(define record? ast:signature:record?)
(define (deep-record? s)
  (and (record? s)
       (symbol? (ast:signature:record-name s))
       (andmap deep-decl? (ast:signature:record-decls s))))
(define module? ast:signature:module?)
(define (deep-module? s)
  (and (module? s)
       (symbol? (ast:signature:module-name s))
       (andmap deep-decl? (ast:signature:module-defs s))))
(define functor? ast:signature:functor?)
(define (deep-functor? s)
  (and (functor? s)
       (symbol? (ast:signature:functor-name s))
       (andmap deep-decl? (ast:signature:functor-args s))
       (deep-signature? (ast:signature:functor-ret s))))

(module* expr #f
  (provide (all-defined-out))
  (define expr? ast:expr:expr?)
  (define void? ast:expr:void?)
  (define functor? ast:expr:functor?)
  (define module? ast:expr:module?)
  (define record? ast:expr:record?)
  (define let? ast:expr:let?)
  (define ref? ast:expr:ref?)
  (define lit? ast:expr:lit?)
  (define app? ast:expr:app?)
  (define switch? ast:expr:switch?)
  (define begin? ast:expr:begin?)
  (define while? ast:expr:while?))
