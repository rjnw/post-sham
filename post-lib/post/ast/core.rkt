#lang racket

(module ast racket
  (provide (all-defined-out))

  (struct ast [(md #:mutable)])
  (struct decl ast [name (sig #:mutable)])
  (module* signature #f
    (provide (all-defined-out))
    (struct signature ast [])
    (struct kind signature [type])
    (struct symbol signature [])
    (struct string signature [])
    (struct integer signature [])
    (struct void signature [])
    (struct bool signature [])
    (struct list signature [element])
    (struct cons signature [a d])
    (struct record signature [name decls])      ;[symbol (list decl)]
    (struct module signature [name defs])
    (struct functor signature [name args ret]))  ;[symbol (list decl) sig]

  (module* expr #f
    (provide (except-out (all-defined-out)
                         functor-app-builder
                         module-app-builder
                         record-app-builder))
    (struct expr ast [(sig #:mutable)])
    (struct void expr [])
    (define functor-app-builder
      (make-keyword-procedure
       (λ (kws kw-args . rst)
         (define appb (functor-appb (first rst)))
         (keyword-apply appb kws kw-args rst))))
    (struct functor expr [bodyb appb]
      #:property prop:procedure functor-app-builder)
    (define module-app-builder
      (make-keyword-procedure
       (λ (kws kw-args . rst)
         (define appb (module-appb (first rst)))
         (keyword-apply appb kws kw-args rst))))
    (struct module expr [defb appb]
      #:property prop:procedure module-app-builder)
    (define record-app-builder
      (make-keyword-procedure
       (λ (kws kw-args . rst)
         (define appb (record-appb (first rst)))
         (keyword-apply appb kws kw-args rst))))
    (struct record expr [vals appb]              ;[(list (cons decl expr))]
      #:property prop:procedure record-app-builder)
    (struct let expr [vars vals body])          ;[(list decl) (list expr) expr]
    (struct ref expr [decl])                     ;[decl]
    (struct lit expr [value])                    ;[any]
    (struct app expr [rator rands])              ;[expr (list expr)]
    (struct switch expr [test branches default]) ;[expr (list (cons expr expr)) expr]
    (struct begin expr [exprs])                  ;[(list expr)]
    (struct while expr [test body])))

(require (prefix-in ast: (submod "." ast))
         (prefix-in ast:signature: (submod "." ast signature))
         (prefix-in ast:expr: (submod "." ast expr)))

(provide (all-from-out (submod "." ast))
         (all-from-out (submod "." ast signature))
         (all-from-out (submod "." ast expr)))
