#lang racket

(require post/ast/core
         (for-syntax syntax/parse))

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
