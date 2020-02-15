#lang racket

(module info racket
  (struct ast [])
  (define (yield-original v stx)
    (with-syntax ([orig (decl-name v)])
      (syntax-parse stx
        [(_ . args) #`(orig . args)]
        [_ #`orig])))
  (struct decl ast [name] #:property prop:procedure yield-original)
  (module* signature #f
    (define (yield-original-value v stx)
      (define appb (signature-app-builder v))
      (with-syntax ([orig (signature-value-id v)])
        (syntax-parse stx
          [(_ . args) (appb v #'args)]
          [_ #`orig])))
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
    (struct expr [])
    (struct dexpr decl [])
    (struct function dexpr [args])
    (struct record dexpr [defs])))
