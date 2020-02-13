#lang racket

(require syntax/parse/define
         "simple.rkt")
(provide (all-defined-out))

(define-syntax-parser define-rkt-type
  [(_ i:id chk:expr coerce:expr)
   #`(define-signature i (rkt chk coerce))]
  [(_ (i:id v:id ...) chk:expr coerce:expr)
   #`(define-signature i (type-forall [v ...] (rkt chk coerce)))])
(define-syntax-parser define-rkt-types
  [(_ (t:expr c:expr) ...)
   #`(begin (define-rkt-type t c) ...)])

(define-rkt-types
  [boolean boolean?]
  [integer integer?]
  [symbol symbol?]
  [string string?]
  [(listof a) (listof (/c a))]
  [(list as) (apply list/c (map /c as))]
  [(cons a d) (cons/c (/c a) (/c d))]
  [(vectorof a) (vectorof (/c a))]
  [(vector as) (apply vector/c (map /c as))])
