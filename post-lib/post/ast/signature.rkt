#lang racket


(module info racket
  (module* signature #f
    (define (signature-info-procedure v stx)
      (match v
        [(type value-ref) (void)]
        [(lit value-ref) (void)]
        [(rkt value-ref) (void)]
        [(function value-ref args ret) (void)]
        [(union value-ref subtypes) (void)]
        [(datatype value-ref args) (void)]
        [(record value-ref defs) (void)]
        [(forall value-ref binds) (void)]))

    (struct signature [value-ref]
      #:property prop:procedure signature-info-procedure)
    (struct type signature [])
    (struct lit signature [])
    (struct rkt signature [])
    (struct function signature [args ret])
    (struct union signature [subtype])
    (struct datatype signature [args])
    (struct record signature [defs])
    (struct forall signature [binds])))
