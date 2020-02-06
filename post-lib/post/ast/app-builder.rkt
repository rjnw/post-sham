#lang racket

(require post/ast/core
         post/ast/pp
         post/parameters/interpreter)

(provide (all-defined-out))

(define generic-function
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (if (interpreting?)
         (begin
           (match-let* ([(ast:expr:function name sig md bodyb _) f]
                        [(ast:signature:function s-md arg-decls ret-sig) sig])
             ((interpreter-type-checker)
              ((interpreter)
               (apply bodyb (map (interpreter-type-checker)
                                 rst
                                 (map ast:decl-sig arg-decls)))
               (interpreter-environment))
              ret-sig)))
         (void)))))

(define generic-record
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))
