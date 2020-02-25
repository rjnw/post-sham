#lang racket

(require syntax/parse/define)

(require (for-syntax (prefix-in sds: (submod "syntax.rkt" definers signature))
                     (prefix-in sde: (submod "syntax.rkt" definers expr))
                     racket/syntax
                     ;; (prefix-in p: post/parameters/syntax)
                     )
         (prefix-in p: post/parameters/syntax))
(provide (all-defined-out))

(define-syntax-parser expr
  [(_ e) (sde:expr (syntax-local-name) #`e)])
(define-syntax-parser define-expr
  [(_ n:id e) (sde:define-expr #`n #`e)])

(define-syntax-parser define-signature
  [(_ name:id s:expr)
   (let* ([gen-name (generate-temporary #'name)]
          [compile-time-defs (sds:define-transformer gen-name #`name #`s)]
          [run-time-defs (sds:define-value gen-name #`name #`s)])
     #`(begin #,compile-time-defs
              #,run-time-defs))]
  [(_ (name:id vars:id ...) s:expr)
   #`(define-signature name (p:forall [vars ...] s))])
(define-syntax-parser signature
  [(_ s:expr) (sds:value (if (syntax-local-name)
                             (syntax-local-name)
                             (generate-temporary #'signature))
                         #`s)])
