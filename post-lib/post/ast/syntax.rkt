#lang racket

(require (for-template racket "core.rkt")
         (prefix-in atsyntax- "../parameters/syntax.rkt"))

(require syntax/parse
         racket/syntax)

(module* ast #f
  (require (for-template (prefix-in md: (submod "core.rkt" _metadata ast))))

  (provide (all-defined-out))
  (define (decl name-stx sig-stx)
    #`(ast:decl (md:decl #'#,name-stx) '#,name-stx #,sig-stx))

  (module* signature #f
    (require (for-template (prefix-in sig: (submod "constructor.rkt" signature))))
    (require (for-template (prefix-in mds: (submod "core.rkt" _metadata ast signature))))
    (provide (all-defined-out))
    (define (signature inferred-name stx)
      (syntax-parse stx
        [(~datum symbol) (symbol)]
        [(~datum string) (string)]
        [(~datum integer) (integer)]
        [((~datum list) s) (list #'s)]
        [((~datum cons) a b) (cons #'a #'b)]
        [(~datum bool) (bool)]
        [((~datum record) (i:id s:expr) ...)
         (record inferred-name (syntax->list #`(i ...)) (syntax->list #`(s ...)))]
        [((~datum module) (i:id s:expr) ...)
         (module inferred-name (syntax->list #`(i ...)) (syntax->list #`(s ...)))]
        [((~or (~datum ->)
               (~datum functor))
          (i:id s:expr) ... r:expr)
         (functor inferred-name (syntax->list #`(i ...)) (syntax->list #`(s ...)) #'r)]
        [s #`s]))
    (define (bool) #`(sig:bool))
    (define (symbol) #`(sig:symbol))
    (define (string) #`(sig:string))
    (define (integer) #`(sig:integer))
    (define (list s)
      (with-syntax ([ss (signature #f s)])
        #`(sig:list ss)))
    (define (cons a b)
      (with-syntax ([sa (signature #f a)]
                    [sb (signature #f b)])
        #`(sig:cons sa sb)))
    (define (record iname is ss)
      (with-syntax ([(sds ...) (map decl is (map signature is ss))]
                    [name (if iname iname (generate-temporary 'record))])
        #`(sig:record 'name `(,sds ...) (mds:record #f #'name))))
    (define (module iname is ss)
      (with-syntax ([(sds ...) (map decl is (map signature is ss))]
                    [name (if iname iname (generate-temporary 'module))])
        #`(sig:module 'name `(,sds ...) (mds:module #f #'name))))
    (define (functor iname is ss r)
      (with-syntax ([(sds ...) (map decl is (map signature is ss))]
                    [name (if iname iname (generate-temporary 'functor))]
                    [rs (signature #f r)])
        #`(sig:functor 'name `(,sds ...) rs (mds:functor #f #'name)))))

  (module* expr #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))
                           (prefix-in ce: (submod "constructor.rkt" expr))
                           (prefix-in mde: (submod "core.rkt" _metadata ast expr))
                           (prefix-in mds: (submod "core.rkt" _metadata ast signature))
                           (prefix-in rt: "runtime.rkt"))
             (prefix-in sig: (submod ".." signature)))
    (provide (all-defined-out))

    (define ((functor-k name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx) bodyb k)
      (parameterize ([atsyntax-functor-name (cons name-stx (atsyntax-functor-name))])
        (with-syntax ([name name-stx]
                      [(arg-names ...) arg-names-stx]
                      [body body-stx]
                      [(arg-sigs ...) (map sig:signature arg-names-stx arg-sigs-stx)]
                      [(arg-sig-names ...) (generate-temporaries arg-names-stx)]
                      [(arg-decls-names ...) (generate-temporaries arg-names-stx)]
                      [ret-sig (sig:signature #f ret-sig-stx)]
                      [(ret-sig-name) (generate-temporaries '(functor-ret-sig))]
                      [(input-name) (generate-temporaries `(functor-input))]
                      [(functor-name functor-sig-name) (generate-temporaries `(,name-stx functor-signature))])
          (with-syntax ([(arg-sig-decls ...) (map decl arg-names-stx (syntax->list #`(arg-sig-names ...)))]
                        [full-sig #`(cs:functor `name (list arg-decls-names ...)
                                                ret-sig-name (mds:functor #f #'name))])
            #`(let* ([arg-sig-names arg-sigs]
                     ...
                     [arg-decls-names arg-sig-decls]
                     ...
                     [ret-sig-name ret-sig])
                (let ([functor-sig-name full-sig])
                  (letrec ([functor-name
                            (ce:functor
                             functor-sig-name
                             (λ input-name
                               (parameterize ([rt:current-functor
                                               (cons functor-name (rt:current-functor))]
                                              [rt:current-functor-input
                                               (cons input-name (rt:current-functor-input))])
                                 (match-let ([(list arg-names ...)
                                              (map rt:functor-input (map rt:isof? input-name
                                                                         (list arg-sig-names ...)))])
                                   #,(bodyb #'body #'ret-sig-name))))
                             #:md (mde:functor))])
                    #,(k #'functor-name #'functor-sig-name))))))))
    (define (functor name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx)
      (define builder (functor-k name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx))
      (builder (λ (a b) a) (λ (a b) a)))

    (define ((module-k name-stx sig-stx body-stxs) bodyb k)
      (parameterize ([atsyntax-module-name (cons name-stx (atsyntax-module-name))])
        (with-syntax
          ([name name-stx]
           [(module-name context-name sig-name) (generate-temporaries `(,name-stx module-context module-signature))]
           [sig (sig:signature name-stx sig-stx)]
           [bs body-stxs])
          (parameterize ([atsyntax-context-name (cons #'context-name atsyntax-context-name)]
                         [atsyntax-module-local-name (cons #'module-name atsyntax-module-local-name)])
            #`(let* ([sig-name sig])
                (letrec ([module-name
                          (ce:module
                           sig-name
                           (λ (context-name)
                             (parameterize ([rt:current-module
                                             (cons module-name (rt:current-module))]
                                            [rt:current-module-context
                                             (cons context-name (rt:current-module-context))]
                                            [rt:current-module-collector
                                             (rt:new-module-collector)])
                               (begin #,(bodyb body-stxs #'sig-name #'context-name)
                                      (rt:check-valid-module-collected)
                                      (rt:current-module-collector))))
                           #:md (mde:module))])
                  #,(k #'module-name #'sig-name)))))))

    (define ((let-k sig-stx var-stxs var-sig-stxs val-stxs body-stxs) bodyb k)
      (with-syntax ([(var-sigs ...) (map sig:signature var-stxs var-sig-stxs)])
        (with-syntax
          ([(let-name) (generate-temporaries `(let))]
           [(vars ...) var-stxs]
           [(vals ...) val-stxs]
           [(var-decls ...) (map decl var-stxs (syntax->list #`(var-sigs ...)))]
           [(var-decl-names ...) (generate-temporaries var-stxs)]
           [(sig-name) (generate-temporaries `(let-signature))]
           [sig (sig:signature #f sig-stx)])
          (with-syntax ([(var-refs ...) (map ref (syntax->list #`(var-decl-names ...)))])
            (parameterize ([atsyntax-let-vars (cons var-stxs (atsyntax-let-vars))])
              #`(let* ([var-decl-names var-decls]
                       ...
                       [vars var-refs]
                       ...
                       [sig-name sig]
                       [let-name
                        (ce:let
                         sig-name
                         (list var-decl-names ...)
                         (map rt:try-coerce (list vals ...) (list var-sigs ...))
                         #,(bodyb body-stxs #'sig-name)
                         #:md (mde:let))])
                  #,(k #'let-name #'sig-name)))))))

    (define (ref dcl-stx)
      #`(ce:ref (ast:decl-sig #,dcl-stx) #,dcl-stx))
    (define (lit val-stx maybe-sig-stx)
      #`(rt:infer-literal #,val-stx #,maybe-sig-stx))
    (define (app rator-stx rand-stxs)
      #`(ce:app (rt:functor-return (rt:typeof #,rator-stx)) #,rator-stx (list #,@rand-stxs)))
    (define (switch sig-stx test-stx branch-pair-stxs default-stx)
      #`(ce:switch #,sig-stx #,test-stx #,@branch-pair-stxs #,default-stx))
    (define (begin sig-stx expr-stxs)
      #`(ce:begin #,sig-stx (list #,@expr-stxs)))
    (define (while sig-stx test-stx body-stx)
      #`(ce:block #,sig-stx #,test-stx #,body-stx))
    (define (void) #`(ce:void))))
