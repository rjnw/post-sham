#lang racket

(module ast racket
  (provide (all-defined-out))

  (struct ast [])
  (struct decl ast [name (sig #:mutable)])

  (module* name #f
    (struct name [stx])
    (struct orig name [])
    (struct gen name []))
  (module* signature #f
    (provide (all-defined-out))
    (struct signature ast [name])
    (struct type signature [base])
    (struct lit signature [sham check/c coerce])
    (struct rkt signature [check/c coerce])
    (struct function signature [args ret])    ;[(list decl) sig]
    (struct union signature [subtypes])       ;[(list subtypes)]
    (struct subtype signature [id args])      ;[symbol (list decl)]
    (struct record signature [defs])          ;[(list decl)]
    (struct forall signature [memo binds type]));[(list decl) sig]

  (module* expr #f
    (provide (except-out (all-defined-out)
                         function-app-builder
                         record-app-builder))
    (define function-app-builder
      (make-keyword-procedure
       (λ (kws kw-args f . rst) (keyword-apply (function-appb f) kws kw-args rst))))
    (define record-app-builder
      (make-keyword-procedure
       (λ (kws kw-args m . rst) (keyword-apply (record-appb m) kws kw-args rst))))

    (struct expr ast [(sig #:mutable)])
    (struct type expr [])
    (struct function expr [md bodyb appb]  #:property prop:procedure function-app-builder)
    (struct record expr [defb appb]      #:property prop:procedure record-app-builder)
    (struct union expr [subtype args])
    (struct let expr [vars vals bodyb])          ;[(list decl) (list expr) λ]
    (struct mref expr [mod decl])                ;[module-path decl]
    (struct rkt expr [value])                    ;[any]
    (struct lit expr [value])                    ;[any]
    (struct app expr [rator rands])              ;[expr (list expr)]
    (struct case expr [test branches]))          ;[expr (list (cons pat expr))]

  (module* pat #f
    (provide (all-defined-out))
    (struct pat ast [])
    (struct allbinds pat [decls pats])
    (struct union pat [sig pats])
    (struct subtype pat [sig pats expr])
    (struct bind pat [decl])
    (struct check-with pat [check? expr])))

(require (prefix-in ast: (submod "." ast))
         (prefix-in ast:signature: (submod "." ast signature))
         (prefix-in ast:expr: (submod "." ast expr))
         (prefix-in ast:pat: (submod "." ast pat)))

(provide (all-from-out (submod "." ast))
         (all-from-out (submod "." ast signature))
         (all-from-out (submod "." ast expr))
         (all-from-out (submod "." ast pat)))
