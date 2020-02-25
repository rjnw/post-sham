#lang racket

(require post/ast/core
         post/ast/pp
         post/parameters/runtime)

(provide (all-defined-out))

(define generic-forall
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (match-let ([(ast:signature:forall md binds typeb _) f])
       (apply typeb rst)))))

(define generic-function
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (begin
       (match-let* ([(ast:expr:function name sig md bodyb _) f]
                    [(ast:signature:function s-md arg-decls ret-sig) sig])
         (printf "app-builder function: ~a\n" rst)
         (apply bodyb rst))))))

(define generic-record
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))
