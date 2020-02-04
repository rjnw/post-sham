#lang racket

;; metadata storing syntax objects

(module _metadata racket
  (provide (all-defined-out))
  (struct metadata [])
  (module* ast #f
    (provide (all-defined-out))
    (struct ast metadata [])
    (struct decl ast [])

    (module* signature #f
      (provide (all-defined-out))
      (struct forall signature [memo])
      (struct record signature [name]))
    (module* expr #f
      (provide (all-defined-out))
      (struct expr [])
      (struct lambda expr [])
      (struct record expr []))))

(require (prefix-in metadata:ast: (submod "." _metadata ast))
         (prefix-in metadata:ast:signature: (submod "." _metadata ast signature))
         (prefix-in metadata:ast:expr: (submod "." _metadata ast expr)))
(provide (all-from-out (submod "." _metadata ast))
         (all-from-out (submod "." _metadata ast signature))
         (all-from-out (submod "." _metadata ast expr)))