#lang racket

(require (for-template
          racket
          racket/stxparam
          post/ast/core
          (prefix-in cs: (submod "constructor.rkt" signature))
          (prefix-in ce: (submod "constructor.rkt" expr))
          (prefix-in p: post/parameters/syntax)
          (prefix-in rt: post/runtime)
          (prefix-in md: (submod "metadata.rkt" _metadata ast))
          (prefix-in mde: (submod "metadata.rkt" _metadata ast expr))
          (prefix-in mds: (submod "metadata.rkt" _metadata ast signature))))
(require racket/syntax
         syntax/parse
         syntax/stx)

(define (decl name-stx sig-stx) #`(ast:decl #'#,name-stx #,sig-stx))
(define (orig-name stx) #`(ast:name:orig #'#,stx))
(define (gen-name stx) #`(ast:name:gen #'#,stx))
(define (name orig hint) (if orig (orig-name orig) (gen-name (generate-temporary hint))))
(define (debug-pp stx) (pretty-print (syntax->datum stx)) stx)

(module* expr #f
  (provide (all-defined-out))
  (define (expr-function stx)
    (syntax-parse stx
      [(_ ([arg-names:id (~optional (~datum :)) arg-sigs:expr] ... (~optional (~datum :)) ret-sig:expr)
          body:expr ...)
       (define all-arg-names (syntax->list #`(arg-names ...)))
       (define all-arg-sigs (syntax->list #`(arg-sigs ...)))
       (define inferred-name (syntax-local-name))
       (with-syntax ([nast (name inferred-name 'post-function)]
                     [(arg-decl-names ...) (stx-map generate-temporary all-arg-names)]
                     [(args-sig-name ret-sig-name)
                      (generate-temporaries `(post-function-args-sig
                                              post-function-ret-sig))]
                     [input-name (generate-temporary 'post-function-input)]
                     [infer-type-name (generate-temporary 'post-function-infer-type)]
                     [name-ast (generate-temporary 'name-ast)]
                     [(function-name function-sig-name)
                      (generate-temporaries
                       (list (if inferred-name inferred-name 'post-function)
                             'post-function-signature))])
         (with-syntax ([(arg-sig-decls ...) (map decl all-arg-names all-arg-sigs)]
                       [full-sig #`(cs:function args-sig-name ret-sig-name)])
           #`(let* ([arg-decl-names arg-sig-decls]
                    ...
                    [name-ast nast]
                    [infer-type-name (rt:lookup-type-in-record-context name-ast (rt:current-record-context))]
                    [args-sig-name (list arg-decl-names ...)]
                    [ret-sig-name ret-sig]
                    [function-sig-name (rt:with-inferred-type full-sig infer-type-name)])
               (letrec ([function-name
                         (ce:function
                          name-ast
                          function-sig-name
                          (λ input-name
                            (parameterize
                                ([rt:current-function-context
                                  (rt:build-function-context function-name
                                                             (map cons args-sig-name input-name)
                                                             (rt:current-function-context))])
                              (match-let ([(list arg-names ...) input-name])
                                body ...))))])
                 (rt:add-to-current-record-context function-name (rt:current-record-context))
                 function-name))))]))

  (define (expr-record stx)
    (syntax-parse stx
      [(_ sig:expr bodys:expr ...)
       (define inferred-name (syntax-local-name))
       (with-syntax
         ([nast (name inferred-name 'post-record)]
          [(record-name record-input-context-name record-collector-name)
           (generate-temporaries (list (if inferred-name inferred-name 'post-record)
                                       'record-input-context 'record-collector))])
         #`(letrec ([record-name
                     (ce:record
                      nast
                      sig
                      (λ (record-input-context-name)
                        (let ([record-collector-name (rt:new-record-collector)])
                          (parameterize
                              ([rt:current-record-context
                                (rt:build-record-context record-name record-input-context-name
                                                         record-collector-name (rt:current-record-context))])
                            (begin
                              bodys ...
                              (rt:from-record-context (rt:current-record-context))))))
                      #:md (mde:record))])
             record-name))]))

  (define (expr-let stx)
    (syntax-parse stx
      [(_ ([vars vals vars-sig] ...) body)
       (define all-vars #`(vars ...))
       (define all-vals #`(vals ...))
       (define all-vars-sig #`(vars-sig ...))
       (with-syntax ([(let-name var-decls-name) (generate-temporaries `(post-let post-let-var-decls))]
                     [(var-decls ...) (stx-map decl (stx-map orig-name all-vars) all-vars-sig)])
         #`(let* ([var-decls-name (list var-decls ...)]
                  [let-name (ce:let (cs:unknown)
                                    var-decls-name
                                    (list vals ...)
                                    (λ (vars ...) body)
                                    #:md (mde:let))])
             let-name))]))
  (define (expr-value stx)
    (syntax-parse stx
      [(_ name:id v:expr)
       #`v]))

  (define (expr-ref dcl-stx)
    #`(ce:ref (ast:decl-sig #,dcl-stx) #,dcl-stx))
  (define (expr-lit val-stx maybe-sig-stx)
    #`(rt:infer-literal #,val-stx #,maybe-sig-stx))
  (define (expr-app rator-stx rand-stxs)
    #`(ce:app (rt:functor-return (rt:typeof #,rator-stx))
              #,rator-stx
              (list #,@rand-stxs)))
  (define (parameterize-for-record stx)
    #`(syntax-parameterize
          ([p:record expr-record]
           [p:value expr-value]
           [p:function expr-function])
        #,stx))
  (define (parameterize-for-expr stx)
    #`(syntax-parameterize
          ([p:record expr-record]
           [p:function expr-function])
        #,stx)))

;; signatures
(module* signature #f
  (provide (all-defined-out))
  (define (sig-record stx)
    (syntax-parse stx
      [(_ [def:id s:expr] ...)
       #:with (decls ...) (stx-map decl (stx-map orig-name #`(def ...)) #`(s ...))
       #`(cs:record (list decls ...))]))
  (define (sig-union stx)
    (syntax-parse stx
      [(_ (c:id ts:expr ...) ...)
       #:with (decls ...) (stx-map decl
                                   (stx-map orig-name #`(c ...))
                                   (stx-map (λ (c t) #`(p:datatype c #,t))
                                            #`(c ...)
                                            #`((ts ...) ...)))
       #`(cs:union (list decls ...))]))
  (define (sig-datatype stx)
    (syntax-parse stx
      [(_ c:id [t:expr ...])
       #:with (tdecls ...) (stx-map decl (stx-map (const #f) #`(t ...)) #`(t ...))
       #`(cs:datatype #,(orig-name #'c) (list tdecls ...)
                      #:md (mds:datatype #f #'c))]))
  (define (sig-forall stx)
    (syntax-parse stx
      [(_ [v:id ...] t:expr)
       (define all-vs #`(v ...))
       (with-syntax ([(bds ...) (stx-map decl
                                     (stx-map orig-name all-vs)
                                     (stx-map (const #`(cs:type #f)) all-vs))])
         #`(cs:forall (list bds ...) (λ (v ...) t)))]))
  (define (sig-function stx)
    (syntax-parse stx
      [(_ [v:id vt:expr] ... ret:expr)
       #:with (arg-decls ...) (stx-map decl (stx-map orig-name #`(v ...)) #`(vt ...))
       #`(cs:function (list arg-decls ...) ret)]))

  (define (sig-rkt stx)
    (syntax-parse stx
      [(_ chk:expr coerce:expr) #`(cs:rkt chk coerce)]))
  (define (sig-lit stx)
    (syntax-parse stx
      [(_ sham:expr check:expr coerce:expr)
       #`(cs:lit sham check coerce)]))
  (define (parameterize-for-signature stx)
    #`(syntax-parameterize
          ([p:record sig-record]
           [p:function sig-function]
           [p:union sig-union]
           [p:datatype sig-datatype]
           [p:forall sig-forall]
           [p:rkt sig-rkt]
           [p:lit sig-lit])
        #,stx)))



(module* definers #f
  (require (submod ".." expr)
           (submod ".." signature))
  (provide signature define-signature
           expr define-expr)

  (define (signature infer-name sig-stx)
    (parameterize-for-signature sig-stx))
  (define (define-signature gen-name orig-name sig-stx)
    #`(define #,gen-name #,(signature orig-name sig-stx)))

  (define (expr infer-name expr-stx)
    (parameterize-for-expr expr-stx))
  (define (define-expr name expr-stx)
    #`(define #,name #,(parameterize-for-expr expr-stx))))
