#lang racket

(require (for-syntax (prefix-in syn: (submod post/ast/syntax ast signature))
                     syntax/parse))
(provide (all-defined-out))

(define-syntax (define-rkt-type stx)
  (syntax-parse stx
    [(_ i:id chk:expr coerce:expr)
     (syn:rkt #'i #'chk #'coerce)]
    [(_ (i:id v:id ...) chk:expr coerce:expr)
     (syn:forall #'i (syntax->list #`(i ...))
                 (build-list (length (syntax->list #`(i ...)))
                             (λ _ (syn:type #f #'#f)))
                 (syn:rkt (generate-temporary #'i) #'chk #'coerce))]))

(define-rkt-type boolean boolean? coerce-to-boolean)
(define-rkt-type integer integer? coerce-to-integer)
(define-rkt-type symbol symbol? coerce-to-symbol)
(define-rkt-type string string? coerce-to-string)
(define-rkt-type (list a) (list/c (/c a)) (coerce-to-list-of a))
(define-rkt-type (list* as) (list*/c (map /c as) (coerce-to-list*-of as)))
(define-rkt-type (cons a d) (cons/c (/c a) (/c d)) (coerce-to-cons-of a d))
(define-rkt-type (vector a) (vector/c (/c a)) (coerce-to-vector-of a))
(define-rkt-type (vector* as) (vector*/c (map /c as) (coerce-to-vector*-of as)))
