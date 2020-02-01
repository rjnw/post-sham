#lang racket

(require (prefix-in a: (submod post/ast/core ast))
         (prefix-in s: (submod post/ast/core ast signature))
         (prefix-in e: (submod post/ast/core ast expr))
         post/runtime
         post/parameters/interpreter
         syntax/parse/define)
(provide interpret)

(define-simple-macro (interpret pexpr)
  (let ([env (basic-interpreter-environment)])
    (parameterize ([interpreting? #t]
                   [interpreter basic-interpreter]
                   [interpreter-environment env]
                   [interpreter-module-context (build-interpreter-context (current-module-context))]
                   [interpreter-type-checker check-type])
      (basic-interpreter pexpr env))))

(define (basic-interpreter pe env)
  (define (be pe env)
    (match pe
      [(e:void md sig) (void)]
      [(e:functor md sig bodyb appb)
       (match-define (s:functor s-md s-name arg-decls ret-sig) sig)
       (λ args
         (check-type (be (apply bodyb (map check-type args (map a:decl-sig arg-decls))) env) ret-sig))]
      [(e:module md sig defb appb)
       (interpret-module-collector (defb (interpreter-module-context)) env)]
      [(e:record md sig vals appb)
       (interpret-record vals appb env)]
      [(e:let md sig vars vals bodyb)
       (check-type
        (be (bodyb (map check-type (map (curryr be env) vals) (map a:decl-sig vars))))
        sig)]
      [(e:ref md sig decl)
       (check-type (env-lookup decl env) sig)]
      [(e:lit md sig value)
       (interpret-literal value sig env)]
      [(e:app md sig rator rands)
       (check-type (apply (be rator env) rands)
                   sig)]
      [(e:switch md sig test branches default)
       (define tval (be test env))
       (define (loop bs)
         (match bs
           ['() (be default env)]
           [(cons (cons chk thn) rst)
            (if (equal? (be chk env) tval)
                (be thn env)
                (loop rst))]))
       (loop branches)]
      [(e:begin md sig exprs)
       (check-type (last (map (curryr be env) exprs))
                   sig)]
      [(e:while md sig test body)
       (error 'post:interpreter "todo")]
      [else pe]))
  (be pe env))

(define (basic-interpreter-environment) (make-hash))
(define (env-extend! env decl value)
  (hash-set! env decl value))
(define (env-extend*! env decls vals)
  (map (curry env-extend! env) decls vals))
(define (env-lookup env key)
  (hash-ref env key))

(define (interpret-module-collector collector env)
  (error 'post:interpreter "todo"))
(define (interpret-record vals appb env)
  (error 'post:interpreter "todo"))
(define (interpret-literal value sig env)
  value)
(define (check-type v sig)
  v)

(define (build-interpreter-context mod-context)
  #f)
