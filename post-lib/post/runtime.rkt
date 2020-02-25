#lang racket

(require "ast/core.rkt"
         "ast/pp.rkt"
         (prefix-in wf: "ast/wf.rkt")
         (prefix-in wfe: (submod "ast/wf.rkt" expr))
         (prefix-in ce: (submod "ast/constructor.rkt" expr)))
(provide (all-defined-out))

(define current-module-context (make-parameter (make-hash)))
(define current-function (make-parameter '()))
(define current-function-input (make-parameter '()))
(define current-forall-input (make-parameter '()))

(define (check-isof? value type-sig)
  (unless (isof? value type-sig)
    (error 'post:runtime:typecheck "failed isof? test: ~a, ~a" (pp:expr value) (pp:sig type-sig)))
  value)
(define (isof? value type-sig)

  #t)
(define (validate-functor-input decl value)
  value)
(define (validate-let-input decl value)
  value)
(define (validate-forall-input decl value)
  (if (wf:decl? value)
      (ast:decl-sig value)
      value))

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
