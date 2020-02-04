#lang racket

(require (for-template racket "core.rkt")
         (prefix-in atsyntax- "../parameters/syntax.rkt"))

(require syntax/parse
         racket/syntax)

(module* ast #f
  (require (for-template (prefix-in md: (submod "metadata.rkt" _metadata ast))))

  (provide (all-defined-out))
  (define (decl name-stx sig-stx) #`(ast:decl #'#,name-stx #,sig-stx))

  (module* signature #f
    (require (for-template (prefix-in sig: (submod "constructor.rkt" signature))))
    (require (for-template (prefix-in mds: (submod "metadata.rkt" _metadata ast signature))))
    (provide (except-out (all-defined-out)
                         gen-name))
    (define (signature inferred-name stx)
      (syntax-parse stx
        [((~datum type) t:expr)
         (type inferred-name #'t)]
        [((~datum lit) sham-type:expr make:expr coerce:expr)
         (lit inferred-name #'sham-type #'make #'coerce)]
        [((~datum rkt) check:expr coerce:expr)
         (rkt inferred-name #'check #'coerce)]
        [((~datum union) subtypes:expr ...)
         (union inferred-name (syntax->list #`(subtypes ...)))]
        [((~datum subtype) i:id args:expr ...)
         (subtype inferred-name #'i (syntax->list #`(args ...)))]
        [((~datum record) (i:id s:expr) ...)
         (record inferred-name (syntax->list #`(i ...)) (syntax->list #`(s ...)))]
        [((~datum forall) (i:id s:expr) ... r:expr)
         (forall inferred-name (syntax->list #`(i ...)) (syntax->list #`(s ...)) #'r)]
        [s #`s]))
    (define (gen-name name hint)
      (if name name
          #`(ast:name:gen #'#,(generate-temporary hint))))

    (define (type iname t)
      (with-syntax ([name (gen-name iname 'kind)]
                    [ts (signature #f t)])
        #`(sig:kind name ts)))
    (define (lit iname st m c)
      (with-syntax ([name (gen-name iname 'lit)])
        #`(sig:lit name #,st #,m #,c)))
    (define (rkt iname chk coerce)
      (with-syntax ([name (gen-name iname 'rkt)])
        #`(sig:rkt name #,chk #,coerce)))
    (define (record iname is ss)
      (with-syntax ([(sds ...) (map decl is (map signature is ss))]
                    [name (gen-name iname 'record)])
        #`(sig:record name (list sds ...))))
    (define (forall iname bind-names bind-sigs type)
      (with-syntax ([name (gen-name iname 'forall)]
                    [(bds ...) (map decl bind-names (map signature bind-names bind-sigs))]
                    [(bns ...) (generate-temporaries bind-names)]
                    [binds (generate-temporary 'forall-binds)]
                    [ts (signature #f type)])
        #`(let ([bns bds] ...)
            (let ([binds (list bns ...)])
              (sig:forall name
                          binds
                          (λ forall-input
                            (match-let [(list bind-names ...) (rt:check-forall-binds forall-input binds)]
                              (parameterize ([rt:current-forall-binds
                                              (cons (map cons binds forall-input)
                                                    (rt:current-forall-binds))])
                                ts)))))))))

  (module* expr #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))
                           (prefix-in ce: (submod "constructor.rkt" expr))
                           (prefix-in rt: post/runtime))
             (prefix-in sig: (submod ".." signature)))
    (provide (all-defined-out))

    (define ((function-k name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx) bodyb k)
      (parameterize ([atsyntax-function-name (cons name-stx (atsyntax-function-name))])
        (with-syntax ([name name-stx]
                      [(arg-names ...) arg-names-stx]
                      [body body-stx]
                      [(arg-sigs ...) (map sig:signature arg-names-stx arg-sigs-stx)]
                      [ret-sig (sig:signature #f ret-sig-stx)]
                      [(args-sig-name ret-sig-name) (generate-temporaries `(function-args-sig function-ret-sig))]
                      [(input-name) (generate-temporary 'function-input)]
                      [(function-name function-sig-name) (generate-temporaries `(,name-stx function-signature))])
          (with-syntax ([(arg-sig-decls ...) (map decl arg-names-stx (syntax->list #`(arg-sigs ...)))]
                        [full-sig #`(cs:function (list arg-names ...) ret-sig-name)])
            #`(let* ([arg-names arg-sig-decls]
                     ...
                     [args-sig-name (list arg-names ...)]
                     [ret-sig-name ret-sig]
                     [function-sig-name full-sig])
                (letrec ([function-name
                          (ce:function
                           function-sig-name
                           (λ input-name
                             (parameterize ([rt:current-function (cons function-name (rt:current-function))]
                                            [rt:current-function-input (cons input-name (rt:current-function-input))])
                               (match-let ([(list arg-names ...)
                                            (map rt:function-input (map rt:check-isof? input-name args-sig-name))])
                                 #,(bodyb #'body #'ret-sig-name)))))])
                  #,(k #'function-name #'function-sig-name)))))))

    (define (function name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx)
      (define builder (function-k name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx))
      (builder (λ (a b) a) (λ (a b) a)))

    (define ((record-k name-stx sig-stx body-stxs) bodyb k)
      (parameterize ([atsyntax-record-name (cons name-stx (atsyntax-record-name))])
        (with-syntax
          ([name name-stx]
           [(record-name context-name sig-name) (generate-temporaries `(,name-stx record-context record-signature))]
           [sig (sig:signature name-stx sig-stx)]
           [bs body-stxs])
          (parameterize ([atsyntax-context-name (cons #'context-name atsyntax-context-name)]
                         [atsyntax-record-local-name (cons #'record-name atsyntax-record-local-name)])
            #`(let* ([sig-name sig])
                (letrec ([record-name
                          (ce:record
                           sig-name
                           (λ (context-name)
                             (parameterize ([rt:current-record
                                             (cons record-name (rt:current-record))]
                                            [rt:current-record-context
                                             (cons context-name (rt:current-record-context))]
                                            [rt:current-record-collector
                                             (rt:new-record-collector)])
                               (begin #,(bodyb body-stxs #'sig-name #'context-name)
                                      (rt:check-valid-record-collected)
                                      (rt:current-record-collector))))
                           #:md (mde:record))])
                  #,(k #'record-name #'sig-name)))))))

    (define ((let-k sig-stx var-stxs var-sig-stxs val-stxs body-stxs) bodyb k)
      (with-syntax ([(var-sigs ...) (map sig:signature var-stxs var-sig-stxs)])
        (with-syntax
          ([let-name (generate-temporary 'let)]
           [let-input (generate-temporary 'let-input)]
           [(vars ...) var-stxs]
           [(vals ...) val-stxs]
           [(var-decls ...) (map decl var-stxs (syntax->list #`(var-sigs ...)))]
           [(var-decl-names ...) (generate-temporaries var-stxs)]
           [sig-name (generate-temporary 'let-signature)]
           [sig (sig:signature #f sig-stx)])
          (parameterize ([atsyntax-let-vars (cons var-stxs (atsyntax-let-vars))])
            #`(let* ([var-decl-names var-decls]
                     ...
                     [sig-name sig]
                     [let-name
                      (ce:let
                       sig-name
                       (list var-decl-names ...)
                       (list vals ...)
                       (λ let-input
                         (match-let ([(list vars ...) (map rt:try-coerce let-input (list var-sigs ...))])
                           #,(bodyb body-stxs #'sig-name)))
                       #:md (mde:let))])
                #,(k #'let-name #'sig-name))))))

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
