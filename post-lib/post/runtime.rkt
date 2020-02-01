#lang racket

(require "ast/core.rkt"
         "ast/pp.rkt"
         (prefix-in wf: "ast/wf.rkt")
         (prefix-in wfe: (submod "ast/wf.rkt" expr))
         (prefix-in ce: (submod "ast/constructor.rkt" expr)))
(provide (all-defined-out))

(define current-module-context (make-parameter (make-hash)))
(define current-functor (make-parameter '()))
(define current-functor-input (make-parameter '()))

(define (check-isof? value type-sig)
  (unless (isof? value type-sig)
    (error 'post:runtime:typecheck "failed isof? test: ~a, ~a" (pp:expr value) (pp:sig type-sig)))
  value)
(define (isof? value type-sig)
  (isof-literal? value type-sig))
(define (functor-input value)
  value)

(define (isof-literal? value sig)
  (cond
    [(wf:integer? sig) (integer? value)]
    [(wf:string? sig) (string? value)]
    [(wf:list? sig) (and (list? value)
                         (andmap (curryr isof? (ast:signature:list-element sig)) value))]
    [(wf:cons? sig) (and (cons? value)
                         (isof? (car value) (ast:signature:cons-a sig))
                         (isof? (cdr value) (ast:signature:cons-d sig)))]
    [(wf:void? sig) (wfe:void? value)]))

(define (infer-literal value sig)
  (define (check value sig)
    (if (isof? value sig)
        value
        (error 'post:literal:type "literal does not match given type: ~a, ~a" (pp:expr value) (pp:sig sig))))
  (if (wf:signature? sig)
      (ce:lit sig (check value sig))
      (error 'post:literal:infertype "not able to infer type for literal: ~a" value)))

(define (try-coerce val sig)
  val)
