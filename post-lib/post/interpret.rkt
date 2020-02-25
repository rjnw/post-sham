#lang racket

(require (prefix-in a: (submod post/ast/core ast))
         (prefix-in s: (submod post/ast/core ast signature))
         (prefix-in e: (submod post/ast/core ast expr))
         post/runtime
         post/parameters/interpreter
         post/parameters/runtime
         syntax/parse/define)
(provide interpret)

(define-simple-macro (interpret pexpr)
  (let ([env (basic-interpreter-environment)])
    (parameterize ([interpreting? #t]
                   [runtime-eval basic-interpreter]
                   [runtime-eval-literal interpret-literal]
                   [runtime-eval-rkt interpret-rkt]
                   [runtime-eval-environment env]
                   [runtime-record-context (build-interpreter-context (runtime-record-context))]
                   [runtime-type-checker check-type])
      (basic-interpreter pexpr env))))

(define (basic-interpreter pe env)
  (define tc (runtime-type-checker))
  (define ev (runtime-eval))
  (define (be pe env)
    (match pe
      [(e:function name sig md bodyb appb)
       (match-define (s:function s-md arg-decls ret-sig) sig)
       (λ args
         (tc
          (ev (apply bodyb
                     (map tc args (map a:decl-sig arg-decls)))
              env)
          ret-sig))]
      #;[(e:record name sig md defb appb) ...]
      [(e:let sig vars vals bodyb)
       (tc
        (be (bodyb (map tc
                        (map (curryr be env) vals)
                        (map a:decl-sig vars))))
        sig)]
      [(e:rkt sig value)
       ((runtime-eval-rkt) value sig env)]
      [(e:lit sig value)
       ((runtime-eval-literal) value sig env)]
      [(e:app sig md rator rands)
       (tc (apply (ev rator env) rands) sig)]
      [else (error 'grack:interpreter:todo "pe:~a" pe)]))
  (be pe env))

(define (basic-interpreter-environment) (make-hash))
(define (env-extend! env decl value)
  (hash-set! env decl value))
(define (env-extend*! env decls vals)
  (map (curry env-extend! env) decls vals))
(define (env-lookup env key)
  (hash-ref env key))

(define (interpret-literal value sig env) value)
(define (interpret-rkt value sig env) value)
(define (check-type v sig) v)

(define (build-interpreter-context mod-context) mod-context)
