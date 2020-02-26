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
     (match-let* ([(ast:expr:function name sig md bodyb _) f]
                    [(ast:signature:function s-md arg-decls ret-sig) sig])
         ((runtime-type-checker)
          ((runtime-eval) (apply bodyb
                                 (map (runtime-type-checker) rst (map ast:decl-sig arg-decls)))
                          (runtime-eval-environment))
          ret-sig)))))

(define generic-record
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))
