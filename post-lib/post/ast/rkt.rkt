#lang racket

(require syntax/parse/define
         post/parameters/syntax
         (for-syntax post/parameters/syntax)
         (prefix-in rkt: racket)
         "core.rkt"
         "simple.rkt")
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax-parser define-rkt-type
  [(_ i:id chk:expr coerce:expr)
   #`(define-signature i (rkt chk coerce))]
  [(_ i:id chk:expr)
   #`(define-rkt-type i chk identity)]
  [(_ (i:id v:id ...) chk:expr coerce:expr)
   #`(define-signature i (forall [v ...] (rkt chk coerce)))]
  [(_ (i:id v:id ...) chk:expr)
   #`(define-rkt-type (i v ...) chk identity)])

(define-syntax-parser define-rkt-types
  [(_ (t:expr c:expr) ...)
   #`(begin (define-rkt-type t c) ...)])

(define (/c t) (ast:signature:rkt-check/c t))
(define-rkt-types
  [boolean boolean?]
  [integer integer?]
  [symbol symbol?]
  [string string?]
  [(listof a) (rkt:listof (/c a))]
  [(list as) (apply list/c (map /c as))]
  [(cons a d) (cons/c (/c a) (/c d))]
  [(vectorof a) (rkt:vectorof (/c a))]
  [(vector as) (apply vector/c (map /c as))])

;; (module+ test
;;   (require "pp.rkt")
;;   (pp:sig boolean)
;;   (pp:sig listof)
;;   #;(define-signature r (record [transitions (cons symbol symbol)]))
;;   )
