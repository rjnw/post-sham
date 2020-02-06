#lang racket

(require (for-template racket "core.rkt")
         (prefix-in atsyntax- "../parameters/syntax.rkt"))

(require syntax/parse
         racket/syntax)

(module* ast #f
  (require (for-template (prefix-in md: (submod "metadata.rkt" _metadata ast))))

  (provide (all-defined-out))
  (define (decl name-stx sig-stx) #`(ast:decl #'#,name-stx #,sig-stx))
  (define (name orig hint)
    (if orig
        #`(ast:name:orig #'#,orig)
        #`(ast:name:gen #'#,(generate-temporary hint))))

  (module* signature #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))))
    (require (for-template (prefix-in mds: (submod "metadata.rkt" _metadata ast signature))))
    (provide (all-defined-out))
    (define (type t)
      #`(cs:type #,t))
    (define (lit st m c)
      #`(cs:lit #,st #,m #,c))
    (define (rkt chk coerce)
      #`(cs:rkt #,chk #,coerce))
    (define record
      (case-lambda
        [(name def-names def-sigs) #`(cs:record #,(map decl def-names def-sigs)
                                   #:md (cs:md:record #,name))]
        [(name defs) #`(cs:record defs #:md (cs:md:record #,name))]))
    (define function
      (case-lambda
        [(arg-names arg-sigs ret-sig) #`(cs:function #,(map decl arg-names arg-sigs) #,ret-sig)]
        [(arg-decls ret-sig) #`(cs:function (list #,@arg-decls) #,ret-sig)]))
    (define (forall bind-names bind-sigs type-stx)
      (with-syntax ([(bds ...) (map decl bind-names bind-sigs)]
                    [(bns ...) (generate-temporaries bind-names)]
                    [binds (generate-temporary 'forall-binds)]
                    [type type-stx])
        #`(let (binds-decls (list bds ...))
            (cs:forall binds-decls
                       (λ forall-input
                         (match-let [(list #,@bind-names) (map rt:validate-forall-input binds-decls forall-input)]
                           (parameterize ([rt:current-forall-binds
                                           (cons (map cons binds-decls forall-input)
                                                 (rt:current-forall-binds))])
                             ts))))))))

  (module* expr #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))
                           (prefix-in ce: (submod "constructor.rkt" expr))
                           (prefix-in rt: post/runtime))
             (prefix-in sig: (submod ".." signature)))
    (provide (all-defined-out))

    (define ((function-k maybe-name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx) bodyb k)
      (define ((default hint) stx) (if stx stx hint))

      (parameterize ([atsyntax-function-name (cons maybe-name-stx (atsyntax-function-name))])
        (with-syntax ([nast (name maybe-name-stx)]
                      [(arg-names ...) (map name arg-names-stx)]
                      [(arg-names-temp ...) (map (default #'post-function-arg-name) arg-names-stx)]
                      [(arg-sigs ...) arg-sigs-stx]
                      [ret-sig ret-sig-stx]
                      [body body-stx]
                      [(args-sig-name ret-sig-name) (generate-temporaries `(post-function-args-sig
                                                                            post-function-ret-sig))]
                      [(input-name) (generate-temporary 'post-function-input)]
                      [(function-name function-sig-name function-decl-name)
                       (generate-temporaries `(,(if maybe-name-stx maybe-name-stx 'post-function)
                                               post-function-signature
                                               post-function-decl))])
          (with-syntax ([(arg-sig-decls ...) (map decl
                                                  (syntax->list #`(arg-names ...))
                                                  (syntax->list #`(arg-sigs ...)))]
                        [full-sig #`(cs:function args-sig-name ret-sig-name)]
                        [function-decl (decl #'nast #'function-sig-name)])
            #`(let* ([arg-names-temp arg-sig-decls]
                     ...
                     [args-sig-name (list arg-names-temp ...)]
                     [ret-sig-name ret-sig]
                     [function-sig-name full-sig]
                     [function-decl-name function-decl])
                (letrec ([function-name
                          (ce:function
                           nast
                           function-sig-name
                           (λ input-name
                             (parameterize ([rt:current-function (cons (cons function-decl-name function-name)
                                                                       (rt:current-function))]
                                            [rt:current-function-input (cons (map cons args-sig-name input-name)
                                                                             (rt:current-function-input))])
                               (match-let ([(list arg-names-temp ...)
                                            (map rt:validate-function-input
                                                 args-sig-name input-name)])
                                 #,(bodyb #'body #'ret-sig-name)))))])
                  #,(k #'function-name #'function-sig-name #'function-decl-name)))))))

    (define (function name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx)
      (define builder (function-k name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx))
      (builder (λ (a b) a) (λ (a b c) a)))

    (define ((record-k maybe-name-stx sig-stx body-stxs) bodyb k)
      (parameterize ([atsyntax-record-name (cons maybe-name-stx (atsyntax-record-name))])
        (with-syntax
          ([nast (name maybe-name-stx)]
           [(record-name record-input-context-name record-sig-name record-decl-name)
            (generate-temporaries `(,(if maybe-name-stx maybe-name-stx 'post-record)
                                    record-input-context record-signature record-decl))]
           [record-sig sig-stx])
          (with-syntax
            ([record-decl (decl #'nast #'record-sig-name)])
            (parameterize ([atsyntax-context-name (cons #'record-input-context-name (atsyntax-context-name))]
                           [atsyntax-record-local-name (cons #'record-name (atsyntax-record-local-name))])
              #`(let* ([record-sig-name record-sig]
                       [record-decl-name record-decl])
                  (letrec ([record-name
                            (ce:record
                             nast
                             record-sig-name
                             (λ (record-input-context-name)
                               (parameterize ([rt:current-record
                                               (cons (cons record-decl-name record-name) (rt:current-record))]
                                              [rt:current-record-input-context
                                               (cons record-input-context-name (rt:current-record-input-context))]
                                              [rt:current-record-collector
                                               (rt:new-record-collector)])
                                 (begin #,@(bodyb body-stxs #'record-sig-name #'record-input-context-name)
                                        (rt:check-valid-record-collected (rt:current-record-collector)
                                                                         record-sig-name)
                                        (rt:current-record-collector))))
                             #:md (mde:record))])
                    #,(k #'record-name #'record-sig-name #'record-decl-name))))))))
    (define (record name-stx sig-stx body-stxs)
      (define builder (record-k name-stx sig-stx body-stxs))
      (builder (λ (a b) a) (λ (a b c) a)))

    (define ((let-k sig-stx var-stxs var-sig-stxs val-stxs body-stxs) bodyb k)
      (with-syntax
        ([(let-name let-input-name let-sig-name var-decls-name)
          (generate-temporaries `(post-let post-let-input post-let-signature post-let-var-decls))]
         [(vars ...) var-stxs]
         [(vals ...) val-stxs]
         [(var-decls ...) (map decl (map name var-stxs) var-sig-stxs)]
         [sig sig-stx])
        (parameterize ([atsyntax-let-vars (cons var-stxs (atsyntax-let-vars))])
          #`(let* ([var-decls-name (list var-decls ...)]
                   [let-sig-name sig]
                   [let-name
                    (ce:let
                     let-sig-name
                     var-decls-name
                     (list vals ...)
                     (λ let-input-name
                       (match-let ([(list vars ...) (map rt:validate-let-input var-decls-name let-input-name)])
                         #,(bodyb body-stxs #'let-sig-name)))
                     #:md (mde:let))])
              #,(k #'let-name #'let-sig-name)))))
    (define (let sig-stx var-stxs var-sig-stxs val-stxs body-stxs)
      (define builder (let-k sig-stx var-stxs var-sig-stxs val-stxs body-stxs))
      (builder (λ (a b) a) (λ (a b) a)))

    (define (ref dcl-stx)
      #`(ce:ref (ast:decl-sig #,dcl-stx) #,dcl-stx))
    (define (lit val-stx maybe-sig-stx)
      #`(rt:infer-literal #,val-stx #,maybe-sig-stx))
    (define (app rator-stx rand-stxs)
      #`(ce:app (rt:functor-return (rt:typeof #,rator-stx)) #,rator-stx (list #,@rand-stxs)))))
