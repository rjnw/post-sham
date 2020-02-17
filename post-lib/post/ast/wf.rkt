#lang racket

(require "core.rkt"
         "metadata.rkt")
(provide (except-out (all-defined-out) md-get md-set!))

(define decl? ast:decl?)
(define (deep-decl? d)
  (and (decl? d)
       (or (ast:name:name? (ast:decl-name d))
           (syntax? (ast:decl-name d))
           (symbol? (ast:decl-name d))
           (false? (ast:decl-name d)))
       (deep-signature? (ast:decl-sig d))))

(define (md-get a)
  (cond [(ast:signature:signature? a) (ast:signature:signature-md a)]
        [(ast:expr:dexpr? a) (ast:expr:dexpr-md a)]
        [else (error 'post:wf:md "given ast does not have a metadata attached: ~a" a)]))
(define (md-set! a m)
  (cond [(ast:signature:signature? a) (ast:signature:set-signature-md! a m)]
        [(ast:expr:dexpr? a) (ast:expr:set-dexpr-md! a m)]
        [else (error 'post:wf:md "given ast does not have a metadata attached: ~a" a)]))

(define (mark-deep-wf-sig! s [v #t])
  (if (and (md-get s)
           (metadata:ast:signature:signature? (md-get s)))
      (metadata:ast:signature:set-signature-wf?! (md-get s) v)
      (md-set! s (metadata:ast:signature:signature v)))
  s)

(define signature? ast:signature:signature?)
(define (deep-signature? s)
  (define (mark-wf-sig! check s)
    (mark-deep-wf-sig! s (check s)))
  (and (signature? s)
       (or (and (metadata:ast:signature:signature? (md-get s))
                (metadata:ast:signature:signature-wf? (md-get s)))
           (cond
             [(record? s) (mark-wf-sig! deep-record? s)]
             [(function? s) (mark-wf-sig! deep-function? s)]
             [(rkt? s) (mark-deep-wf-sig! s #t)]
             [else (or (symbol? s)
                       (string? s)
                       (integer? s))]))))

(define record? ast:signature:record?)
(define (deep-record? s)
  (and (record? s)
       (andmap deep-decl? (ast:signature:record-defs s))))
(define function? ast:signature:function?)
(define (deep-function? s)
  (and (function? s)
       (andmap deep-decl? (ast:signature:function-args s))
       (deep-signature? (ast:signature:function-ret s))))
(define rkt? ast:signature:rkt?)
(module* expr #f
  (provide (all-defined-out))
  (define expr? ast:expr:expr?)
  (define functor? ast:expr:function?)
  (define record? ast:expr:record?)
  (define let? ast:expr:let?)
  (define mref? ast:expr:mref?)
  (define rkt? ast:expr:rkt?)
  (define lit? ast:expr:lit?)
  (define app? ast:expr:app?)
  (define case? ast:expr:case?))
