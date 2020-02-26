#lang racket

(require syntax/parse/define)

(require (for-syntax (prefix-in sd- (submod "syntax.rkt" definers))
                     (prefix-in ts- (submod "transformer.rkt" signature))
                     racket/syntax
                     (prefix-in p- post/parameters/syntax))
         (prefix-in p- post/parameters/syntax))
(provide (all-defined-out))

(define-syntax-parser expr
  [(_ e) (sd-expr (syntax-local-name) #`e)])
(define-syntax-parser define-expr
  [(_ n:id e) (sd-define-expr #`n #`e)])

(define-syntax-parser define-signature
  [(_ name:id s:expr)
   (let* ([gen-name (generate-temporary #'name)]
          [compile-time-defs (ts-define-signature-transformer gen-name #`name #`s)]
          [run-time-defs (sd-define-signature gen-name #`name #`s)])
     #`(begin #,compile-time-defs
         #,run-time-defs))]
  [(_ (name:id vars:id ...) s:expr)
   #`(define-signature name (p-forall [vars ...] s))])
(define-syntax-parser signature
  [(_ s:expr) (sd-signature (if (syntax-local-name)
                                (syntax-local-name)
                                (generate-temporary #'signature))
                            #`s)])
