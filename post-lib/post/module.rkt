#lang racket

(require (for-syntax syntax/parse))

(define-syntax (post-module stx)
  (syntax-parse stx
    [(_ (inp:id v:expr) sig:expr
        parts:expr ...)
     #`(letrec ([inp (λ (s) (post-struct-lookup v s))]
                [m (post:module (cons 'inp v) sig
                                (λ args
                                  (parameterize ([post-current-module m]
                                                 [post-module-input args])
                                    (begin parts ...))))])
         m)]))

(define-syntax (val stx)
  (syntax-parse stx
    [(_ name:id v:expr)
     #`(if (not (false? (post-current-module)))
           (post-add-to-module 'name (post-current-module) v)
           (error "using val outside post-module"))]))
