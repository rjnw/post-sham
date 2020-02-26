#lang racket

(require "ast/core.rkt"
         "ast/pp.rkt"
         (prefix-in wf: "ast/wf.rkt")
         (prefix-in wfe: (submod "ast/wf.rkt" expr))
         (prefix-in ce: (submod "ast/constructor.rkt" expr)))
(provide (all-defined-out))

(define current-module-context (make-parameter (make-hash)))
(define current-forall-input (make-parameter '()))

;; current-function
(struct function-context [value inputs prev])
(define current-function-context (make-parameter '()))
(define (build-function-context value inputs prev)
  (function-context value inputs prev))

;; current-record
(struct record-context [value input collector prev])
(define current-record-context (make-parameter '()))
(define (build-record-context value input collector prev)
  (record-context value input collector prev))
(define (new-record-collector) (make-hash))
(define (from-record-context rcontexts) #f)

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



(define (lookup-type-in-record-context name record-contexts)
  #f)

(define (with-inferred-type full-sig inferred-sig)
  full-sig)

(define (add-to-current-record-context value record-contexts)
  (void))
