#lang racket

(require post/ast/core
         post/parameter/syntax
         post/parameter/runtime)
(require racket/stxparam)

(module ast racket

  (define (decl name-stx sig-stx)
    #`(ast:decl #'#,name-stx #,name-stx #,sig-stx))

  (define (signature inferred-name stx)
    (syntax-parse stx
      [(~datum symbol) #`(ast:signature:symbol #f)]
      [(~datum string) #`(ast:signature:string #f)]
      [(~datum integer) #`(ast:signature:integer #f)]
      [((~datum list) s) (with-syntax ([ss (signature #f s)])
                           #`(ast:signature:list #f ss))]
      [((~datum cons) a b) (with-syntax ([sa (signature #f a)]
                                         [sb (signature #f b)])
                             #`(ast:signature:cons #f sa sb))]
      [((~datum record) (i:id s:expr) ...)
       (let ([in (syntax->list #`(i ...))])
         (with-syntax ([(ss ...) (map decl in (map signature in (syntax->list #`(s ...))))]
                       [name (if inferred-name inferred-name (generate-temporaries '(record)))])
           #`(ast:signature:record (cons #'name (list #'i ...)) name ss ...)))]
      [((~datum module) (i:id s:expr) ...)
       (let ([in (syntax->list #`(i ...))])
         (with-syntax ([(ss ...) (map decl in (map signature in (syntax->list #`(s ...))))]
                       [name (if inferred-name inferred-name (generate-temporaries '(module)))])
           #`(ast:signature:module (cons #'name (list #'i ...)) name ss ...)))]
      [((~datum functor) (i:id s:expr) ... r:expr)
       (let ([in (syntax->list #`(i ...))])
         (with-syntax ([(ss ...) (map decl in (map signature in (syntax->list #`(s ...))))]
                       [name (if inferred-name inferred-name (generate-temporaries '(functor)))]
                       [rs (signature #f #'r)])
           #`(ast-signature-functor (cons #'name (list #'i ...)) name (list ss ...) rs)))]
      [s #`s]))

  (module* expr #f
    (provide functor value record)

    (module hoas #f
      (provide functor module record let)
      (define (functor name arg-names arg-sigs ret-sig body)
        (with-syntax ([(input-args) (generate-temporaries '(fiargs))]
                      [(asigs) (generate-temporaries '(asigs))]
                      [(f) (generate-temporaries (list name))]
                      [(an ...) arg-names]
                      [(as ...) arg-sigs]
                      [fs full-sig]
                      [n name])
          (syntax-parameterize ([atsyntax-functor-name (cons name (syntax-parameter-value current-functor-name))])
            (λ (k)
              #`(letrec ([asigs (list as ...)]
                         [f (ast:expr:hoas:functor
                             (metadata:ast:expr:functor #'n (list (cons #'an #'as) ...) #'fs)
                             n
                             (map ast:decl `(an ...) asigs)
                             (signature:functor asigs #,ret-sig)
                             (λ input-args
                               (parameterize ([current-functor (cons f (current-functor))]
                                              [current-functor-input
                                               (cons input-args (current-functor-input))])
                                 (match-let ([(list an ...)
                                              (map functor-input
                                                   (map check-type input-args (list as ...)))])
                                   #,body))))])
                  #,(k #'f))))))

      (define (module name sig body)
        (with-syntax
          ([(context) (generate-temporaries '(context))]
           [m (generate-temporaries (list name))]
           [n name]
           [s sig]
           [b body])
          (syntax-parameterize ([atsyntax-module-name (cons name (syntax-parameter-value current-module-name))])
            (λ (k)
              #`(letrec ([m (ast:expr:hoas:module
                             (metadata:ast:expr:module #'n #'s #'b)
                             n
                             s
                             (λ (context)
                               (parameterize ([current-module (cons m (current-module))]
                                              [current-module-context (cons context (current-module-context))]
                                              [current-module-collector (make-hash)])
                                 (begin b
                                        (check-valid-module-collected s)
                                        (current-module-collector)))))])
                  #,(k #'m))))))
      (define (let vars vals body)
        (with-syntax
          ([(vn ...) (generate-temporaries vars)]
           [(vl ...) #`(#,@vals)]
           [(l) (generate-temporaries '(let))])
          (syntax-parameterize ([current-let-vars (cons vars (syntax-parameter-value current-let-vars))])
            (λ (k)
              #`(letrec ([l (ast:expr:hoas:let
                             (metadata:ast:expr:let #'#,vars #'#,vals #'#,body)
                             (list vl ...)
                             (λ input-args
                               (match-let ([(list vn ...) (map check-type input-args (list vs ...))])
                                 #,body)))])
                  #,(k #'l)))))))

    (require (prefix-in hoas: (submod "." hoas)))
    (define (functor name arg-names arg-sigs ret-sig body)
      (define hoas-builder (hoas:functor name arg-names arg-sigs ret-sig body))
      (hoas-builder (λ (f) #`(ast:expr:hoas:intermediate #,f build-functor))))
    (define (let vars vals body)
      (define hoas-builder (hoas:let vars vals body))
      (hoas-builder (λ (l) #`(ast:expr:hoas:intermediate #,l build-let))))))




(define (value value)
  (check-valid-module (add-to-module (build-value value))))

(define (function name args type body)
  (check-valid-module (add-to-module (build-function name args type body))))

(define (lambda args type body)
  (function (syntax (gensym 'lambda)) args type body))
