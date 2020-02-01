#lang racket

(require post/ast/core
         post/ast/pp
         post/parameters/interpreter)

(provide (all-defined-out))

(define generic-functor
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (if (interpreting?)
         (begin
           (match-let* ([(ast:expr:functor md sig bodyb _) f]
                        [(ast:signature:functor s-md s-name arg-decls ret-sig) sig])
             ((interpreter-type-checker)
              ((interpreter)
               (apply bodyb (map (interpreter-type-checker)
                                 rst
                                 (map ast:decl-sig arg-decls)))
               (interpreter-environment))
              ret-sig)))
         (void)))))

(define generic-module
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))

(define generic-record
  (make-keyword-procedure
   (λ (kws kw-args r . rst)
     (void))))

(define hoas-functor
  (make-keyword-procedure
   (λ (kws kw-args f . rst)
     (void))))

(define hoas-module
  (make-keyword-procedure
   (λ (kws kw-args m . rst)
     (void))))
