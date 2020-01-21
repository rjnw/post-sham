#lang racket

(provide (all-defined-out))
(require (for-syntax syntax/parse))

(struct post:signature [] #:prefab)
(struct post:signature:symbol [] #:prefab)
(struct post:signature:string [] #:prefab)
(struct post:signature:integer [] #:prefab)
(struct post:signature:list [element] #:prefab)
(struct post:signature:cons [a d] #:prefab)
(struct post:signature:function [args ret] #:prefab)
(struct post:signature:module [args] #:prefab)

(define-syntax (post-signature stx)
  (define-syntax-class signature
    (pattern (~datum symbol)
             #:attr type #`(post:signature:symbol))
    (pattern (~datum string)
             #:attr type #`(post:signature:string))
    (pattern (~datum integer)
             #:attr type #`(post:signature:integer))
    (pattern ((~datum list) s:signature)
             #:attr type #`(post:signature:list #,(attribute s.type)))
    (pattern ((~datum cons) a:signature b:signature)
             #:attr type #`(post:signature:cons #,(attribute a.type) #,(attribute b.type)))
    (pattern ((~datum ->) inp:signature ... out:signature)
             #:attr type #`(post:signature:function #,(attribute inp.type)
                                                    #,(attribute out.type)))
    (pattern ((~datum module) (i:id v:signature) ...)
             #:with (t ...) (attribute v.type)
             #:attr type #`(post:signature:module (make-hash `((i . ,t) ...))))
    (pattern t:expr
             #:attr type #`t))
  (syntax-parse stx
    [(_ s:signature) (attribute s.type)]))


(module+ test
  (define m
    (post-signature
     (module
         [a symbol]
         [b (list symbol)]
         [c (module [d symbol])])))
  (pretty-print m))
