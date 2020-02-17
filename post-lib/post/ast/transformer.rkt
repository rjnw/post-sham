#lang racket

(module info racket
  (require (for-template racket))
  (provide (except-out (all-defined-out)
                       transformer))
  (struct ast [])
  (define (transformer v . args)
    (if (and (equal? (length args) 1)
             (syntax? (car args)))
        (with-syntax ([orig (decl-orig-name v)])
          (syntax-case (car args) ()
            [(self . args) #`(orig . args)]
            [_ #`orig]))
        (apply (decl-info v) args)))
  (struct decl ast [orig-name info] #:property prop:procedure transformer)
  (module* signature #f
    (provide (except-out (all-defined-out)
                         generic-sig
                         forall-app))
    (define (generic-sig s . args)
      (error "using post signature information as a function"))
    (struct signature [] #:property prop:procedure generic-sig)
    (struct type signature [])
    (struct lit signature [])
    (struct rkt signature [])
    (struct function signature [argns args ret])
    (struct union signature [ctors subs])
    (struct datatype signature [ctor args])
    (struct record signature [defs sigs])
    (define (forall-app s . args) (forall-sig s)) ;; TODO
    (struct forall signature [binds sig] #:property prop:procedure forall-app))
  (module* expr #f
    (provide (all-defined-out))
    (struct expr [])
    (struct dexpr decl [])
    (struct function dexpr [args])
    (struct record dexpr [defs])))
