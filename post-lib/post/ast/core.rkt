#lang racket

(module value racket
  (provide (all-defined-out))
  (struct value [(md #:mutable)])
  (struct translated value [post sham])
  (struct compiled value [pre bin]))

(module ast racket
  (provide (all-defined-out))

  (struct ast [(md #:mutable)])
  (struct decl ast [name sig])

  (module* signature #f
    (provide (all-defined-out))
    (struct signature ast [])
    (struct kind signature [type])
    (struct symbol signature [])
    (struct string signature [])
    (struct integer signature [])
    (struct list signature [element])
    (struct cons signature [a d])
    (struct record signature [name decls])      ;[(list decl)]
    (struct module signature [name defs])
    (struct functor signature [name args ret])) ;[(list sig) sig]

  (module* expr #f
    (provide (all-defined-out))
    (struct expr ast [(type #:mutable)])
    (struct void expr [])
    (struct functor expr [input body appb]
      #:property prop:procedure (struct-field-index appb))
    (struct module expr [defs appb]
      #:property prop:procedure (struct-field-index appb))
    (struct record expr [vals appb]              ;[(list (cons decl expr))]
      #:property prop:procedure (struct-field-index appb))
    (struct let expr [vars vals body])           ;[(list decl) (list expr) expr]
    (struct var expr [sym])                      ;[symbol]
    (struct literal expr [value])                ;[any]
    (struct app expr [rator rands])              ;[expr (list expr)]
    (struct switch expr [test branches default]) ;[expr (list (cons expr expr)) expr]
    (struct block expr [exprs])                  ;[(list expr)]
    (struct while expr [test body])              ;[expr expr]

    (module* hoas #f
      (provide (all-defined-out))
      (struct hoas expr [])
      (struct functor hoas [name input bodyb])
      (struct module hoas [name bodyb])
      (struct let hoas [vars valb bodyb]))))

(require (prefix-in ast: (submod "." ast))
         (prefix-in ast:signature: (submod "." ast signature))
         (prefix-in ast:expr: (submod "." ast expr)))

(provide (all-from-out (submod "." ast))
         (all-from-out (submod "." ast signature))
         (all-from-out (submod "." ast expr)))

;; metadata storing syntax objects

(module _metadata racket
  (provide (all-defined-out))
  (struct metadata [])
  (module* ast #f
    (provide (all-defined-out))
    (struct ast metadata [])

    (module* signature #f
      (provide (all-defined-out))
      (struct signature metadata [(wf? #:mutable)])
      (struct list signature [element])
      (struct cons signature [a d])
      (struct record signature [vals])
      (struct module signature [defs])
      (struct functor signature [args ret]))
    (module* expr #f
      (provide (all-defined-out))
      (struct expr [])
      (struct functor expr [name input sig])
      (struct module expr [defs])
      (struct records expr [vals])
      (struct var expr [syn])
      (struct let expr [ids vals types])
      (struct literal expr [syn])
      (struct app expr [rator rands])
      (struct switch expr [test branches default])
      (struct blocks expr [exprs])
      (struct while expr [test body]))))

(module metadata racket
  (require (prefix-in metadata:ast: (submod ".." _metadata ast))
           (prefix-in metadata:ast:signature: (submod ".." _metadata ast signature))
           (prefix-in metadata:ast:expr: (submod ".." _metadata ast expr)))
  (provide (all-from-out (submod ".." _metadata ast))
           (all-from-out (submod ".." _metadata ast signature))
           (all-from-out (submod ".." _metadata ast expr))))
