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
    (define (yield-original-value v stx)
      (define appb (signature-app-builder v))
      (with-syntax ([orig (signature-value-id v)])
        (syntax-case stx ()
          [(_  . args) (if appb (appb v #`args) (raise-syntax-error 'post:signature "invalid use" stx))]
          [_ #`orig])))
    (struct dsignature decl [sig])
    (struct signature [value-id app-builder] #:property prop:procedure yield-original-value)
    (struct type signature [])
    (struct lit signature [])
    (struct rkt signature [])
    (struct function signature [args ret])
    (struct union signature [subtype])
    (struct datatype signature [args])
    (struct record signature [defs])
    (struct forall signature [binds]))
  (module* expr #f
    (provide (all-defined-out))
    (struct expr [])
    (struct dexpr decl [])
    (struct function dexpr [args])
    (struct record dexpr [defs])))
