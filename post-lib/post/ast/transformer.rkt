#lang racket

(module info racket
  (provide (except-out (all-defined-out)
                       yield-original))
  (struct ast [])
  (define (yield-original v stx)
    (with-syntax ([orig (decl-orig-name v)])
      (syntax-case stx ()
        [(self . args) #`(orig . args)]
        [_ #`orig])))
  (struct decl ast [orig-name] #:property prop:procedure yield-original)
  (module* signature #f
    (provide (except-out (all-defined-out)
                         yield-original-value))
    (struct signature [])
    (struct type signature [])
    (struct lit signature [])
    (struct rkt signature [])
    (struct function signature [argns args ret])
    (struct union signature [ctors subs])
    (struct datatype signature [ctor args])
    (struct record signature [defs sigs])
    (struct forall signature [binds sig]))
  (module* expr #f
    (provide (all-defined-out))
    (struct expr [])
    (struct dexpr decl [])
    (struct function dexpr [args])
    (struct record dexpr [defs])))
