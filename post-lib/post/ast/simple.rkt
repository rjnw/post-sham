#lang racket

(require (for-syntax syntax/parse
                     syntax/parse/define))

(define-syntax-parser function
  [(_ name:id (inp-args:id ...) type:expr body:expr)
   (syntax-build-function #'name (syntax->list #'(inp-args ...)) #'type #'body)])

(define-syntax (signature stx)
  )
(define-syntax (module stx)
  (syntax-parse stx
    [(_ ([inp:id t:expr] ...) sig:expr
        parts:expr ...)
     (syntax-build-module (map cons (syntax->list #`(inp ...)) (syntax->list #`(t ...)))
                               #'sig
                               (syntax->list #`(parts ...)))]))

(define-syntax (expr stx)
  (syntax-parse stx
    [(_ v:expr)]))
