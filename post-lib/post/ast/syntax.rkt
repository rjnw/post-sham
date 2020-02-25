#lang racket

(module ast racket
  (require (for-template racket
                         "core.rkt"
                         (prefix-in md: (submod "metadata.rkt" _metadata ast)))
           (prefix-in atsyntax- "../parameters/atsyntax.rkt")
           syntax/parse
           racket/syntax)

  (provide (all-defined-out))
  (define (decl name-stx sig-stx) #`(ast:decl #'#,name-stx #,sig-stx))
  (define (name orig hint)
    (if orig
        #`(ast:name:orig #'#,orig)
        #`(ast:name:gen #'#,(generate-temporary hint))))

  (module* signature #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))
                           (prefix-in mds: (submod "metadata.rkt" _metadata ast signature))
                           (prefix-in rt: post/runtime)))
    (provide (all-defined-out))
    (define (type (t #f))
      #`(cs:type #,t))
    (define (lit st m c)
      #`(cs:lit #,st #,m #,c))
    (define (rkt chk coerce)
      #`(cs:rkt #,chk #,coerce))
    (define (union ctors subs)
      #`(cs:union (list #,@(map decl ctors subs))))
    (define (datatype ctor argns argts)
      #`(cs:datatype (list #,@(map decl argns argts)) #:md (mds:datatype #f #'#,ctor)))
    (define record
      (case-lambda
        [(def-names def-sigs) #`(cs:record (list #,@(map decl def-names def-sigs)))]
        [(defs) #`(cs:record defs)]))
    (define function
      (case-lambda
        [(arg-names arg-sigs ret-sig)
         #`(cs:function (list #,@(map decl arg-names arg-sigs)) #,ret-sig)]
        [(arg-decls ret-sig) #`(cs:function (list #,@arg-decls) #,ret-sig)]))
    (define (forall bind-names bind-sigs type-stx)
      (with-syntax ([(bds ...) (map decl bind-names bind-sigs)]
                    [binds-decls (generate-temporary 'forall-binds)]
                    [forall-input (generate-temporary 'forall-input)]
                    [type type-stx])
        #`(let ([binds-decls (list bds ...)])
            (cs:forall binds-decls
                       (λ forall-input
                         (match-let
                             ([(list #,@bind-names)
                               (map rt:validate-forall-input binds-decls forall-input)])
                           (parameterize ([rt:current-forall-input
                                           (cons (map cons binds-decls forall-input)
                                                 (rt:current-forall-input))])
                             type))))))))

  (module* expr #f
    (require (for-template (prefix-in cs: (submod "constructor.rkt" signature))
                           (prefix-in ce: (submod "constructor.rkt" expr))
                           (prefix-in rt: post/runtime))
             (prefix-in sig: (submod ".." signature)))
    (provide (all-defined-out))

    (define ((function-k maybe-name-stx arg-names-stx arg-sigs-stx ret-sig-stx body-stx)
             bodyb k)
      (define ((default hint) stx) (if stx stx hint))

      (parameterize ([atsyntax-function-name (cons maybe-name-stx (atsyntax-function-name))])
        (with-syntax ([nast (name maybe-name-stx 'post-function)]
                      [(arg-names ...) (map (λ (s) (name s 'post-function-arg)) arg-names-stx)]
                      [(arg-names-temp ...)
                       (map (default #'post-function-arg-name) arg-names-stx)]
                      [(arg-sigs ...) arg-sigs-stx]
                      [ret-sig ret-sig-stx]
                      [body body-stx]
                      [(args-sig-name ret-sig-name)
                       (generate-temporaries `(post-function-args-sig
                                               post-function-ret-sig))]
                      [input-name (generate-temporary 'post-function-input)]
                      [(function-name function-sig-name function-decl-name)
                       (generate-temporaries
                        `(,(if maybe-name-stx maybe-name-stx 'post-function)
                          post-function-signature
                          post-function-decl))])
          (with-syntax ([(arg-sig-decls ...) (map decl
                                                  (syntax->list #`(arg-names ...))
                                                  (syntax->list #`(arg-sigs ...)))]
                        [full-sig #`(cs:function args-sig-name ret-sig-name)]
                        [function-decl (decl #'nast #'function-sig-name)])
            (define (f x) (pretty-print (syntax->datum x)) x)
            (f #`(let* ([arg-names-temp arg-sig-decls]
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
                              (parameterize
                                  ([rt:current-function
                                    (cons (cons function-decl-name function-name)
                                          (rt:current-function))]
                                   [rt:current-function-input
                                    (cons (map cons args-sig-name input-name)
                                          (rt:current-function-input))])
                                (match-let ([(list arg-names-temp ...) input-name])
                                  #,@(bodyb #'body #'ret-sig-name)))))])
                   #,(k #'function-name #'function-sig-name #'function-decl-name))))))))

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
            (parameterize
                ([atsyntax-context-name
                  (cons #'record-input-context-name (atsyntax-context-name))]
                 [atsyntax-record-local-name
                  (cons #'record-name (atsyntax-record-local-name))])
              #`(let* ([record-sig-name record-sig]
                       [record-decl-name record-decl])
                  (letrec ([record-name
                            (ce:record
                             nast
                             record-sig-name
                             (λ (record-input-context-name)
                               (parameterize
                                   ([rt:current-record
                                     (cons (cons record-decl-name record-name)
                                           (rt:current-record))]
                                    [rt:current-record-input-context
                                     (cons record-input-context-name
                                           (rt:current-record-input-context))]
                                    [rt:current-record-collector
                                     (rt:new-record-collector)])
                                 (begin
                                   #,@(bodyb
                                       body-stxs
                                       #'record-sig-name
                                       #'record-input-context-name)
                                   (rt:check-valid-record-collected
                                    (rt:current-record-collector)
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
          (generate-temporaries
           `(post-let post-let-input post-let-signature post-let-var-decls))]
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
                       (match-let ([(list vars ...)
                                    (map rt:validate-let-input
                                         var-decls-name
                                         let-input-name)])
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
      #`(ce:app (rt:functor-return (rt:typeof #,rator-stx))
                #,rator-stx
                (list #,@rand-stxs)))))

(module value-transformer racket
  (module* signature #f
    (require (prefix-in ast: (submod ".." ".." ast signature))
             (prefix-in p: (for-template post/parameters/syntax))
             syntax/parse)
    (provide (all-defined-out))
    (define (record stx)
      (syntax-parse stx
        [(_ [def:id s:expr] ...)
         (ast:record (syntax->list #`(def ...)) (syntax->list #`(s ...)))]))
    (define (union stx)
      (syntax-parse stx
        [(_ (c:id ts:expr ...) ...)
         (ast:union
          (syntax->list #`(c ...))
          (map (λ (c t) #`(p:datatype c #,t))
               (syntax->list #`(c ...))
               (syntax->list #`((ts ...) ...))))]))
    (define (datatype stx)
      (syntax-parse stx
        [(_ c:id [v:id ...])
         (ast:datatype #'c
                       (map (const #f) (syntax->list #`(v ...)))
                       (syntax->list #`(v ...)))]))
    (define (forall stx)
      (syntax-parse stx
        [(_ [v:id ...] t:expr)
         (ast:forall (syntax->list #`(v ...))
                     (map (const (ast:type))
                          (syntax->list #`(v ...)))
                     #'t)]))
    (define (function stx)
      (syntax-parse stx
        [(_ [v:id vt:expr] ... ret:expr)
         (ast:function (syntax->list #`(v ...))
                       (syntax->list #`(vt ...))
                       #`ret)]))
    (define (rkt stx)
      (syntax-parse stx
        [(_ chk:expr coerce:expr)
         (ast:rkt #`chk #`coerce)]))
    (define (lit stx)
      (syntax-parse stx
        [(_ sham:expr check:expr coerce:expr)
         (ast:lit #`sham #`check #`coerce)])))
  (module* expr #f
    (require (prefix-in ast: (submod ".." ".." ast expr))
             syntax/parse)
    (provide (all-defined-out))
    (define (function stx)
      (syntax-parse stx
        [(_ ([inp-args:id (~optional (~datum :)) inp-types:expr] ... (~optional (~datum :)) ret-type:expr)
            body:expr ...)
         (ast:function (syntax-local-name)
                       (syntax->list #`(inp-args ...))
                       (syntax->list #`(inp-types ...))
                       #`ret-type
                       (syntax->list #`(body ...)))]))
    (define (record stx) #'42)
    (define (value stx) #'42)))

(module syntax-transformer racket
  (module* signature #f
    (provide (all-defined-out))
    (require (for-template racket)
             syntax/parse
             (for-template (prefix-in t: (submod "transformer.rkt" info signature))
                           (prefix-in p: post/parameters/syntax)))
    (define (record stx)
      (syntax-parse stx
        [(_ (c:id t:expr) ...)
         #`(t:record (list #'c ...) (list t ...))]))
    (define (union stx)
      (syntax-parse stx
        [(_ (c:id ts:expr ...) ...)
         #`(t:union (list #'c ...)
                    (list #,@(map (λ (c t) #`(p:datatype #,c #,t))
                                  (syntax->list #`(c ...))
                                  (syntax->list #`((ts ...) ...)))))]))
    (define (datatype stx)
      (syntax-parse stx
        [(_ c:id [v:id ...])
         #`(t:datatype #'c #'(v ...))]))
    (define (forall stx)
      (syntax-parse stx
        [(_ [v:id ...] t:expr)
         #`(t:forall (list #'v ...) t)]))
    (define (function stx)
      (syntax-parse stx
        [(_ [v:id vt:expr] ... ret:expr)
         #`(t:function (list #'v ...) (list vt ...) ret)]))
    (define (rkt stx) #`(t:rkt))
    (define (lit stx) #`(t:lit))))

(module definers racket
  (require (for-template (except-in racket ->)
                         racket/stxparam
                         post/parameters/syntax)
           racket/stxparam
           post/parameters/syntax
           (prefix-in vts- (submod ".." value-transformer signature))
           (prefix-in vte- (submod ".." value-transformer expr))
           (for-syntax (prefix-in sts- (submod ".." syntax-transformer signature)))
           (prefix-in atsyntax- post/parameters/atsyntax)
           (prefix-in ti: (submod post/ast/transformer info))
           (prefix-in tis: (submod post/ast/transformer info signature)))
  (define (build-define-value name value)
    #`(define #,name #,value))
  (define (build-define-syntax name value)
    #`(define-syntax #,name #,value))

  (module* signature #f
    (provide value
             define-value
             define-transformer)
    (define (build-decl-info gen-name orig-name vstx)
      (with-syntax ([gn gen-name])
        #`(ti:decl #'#,gen-name #,vstx)))
    (define (parameterize-constructors
             record-value
             union-value
             datatype-value
             forall-value
             function-value
             rkt-value
             lit-value
             stx)
      #`(syntax-parameterize
            ([record #,record-value]
             [union #,union-value]
             [datatype #,datatype-value]
             [forall #,forall-value]
             [function #,function-value]
             [rkt #,rkt-value]
             [lit #,lit-value])
          #,stx))

    (define (parameterize-constructors-for-value sig-stx)
      (parameterize-constructors
       #`vts-record
       #`vts-union
       #`vts-datatype
       #`vts-forall
       #`vts-function
       #`vts-rkt
       #`vts-lit
       sig-stx))

    (define (parameterize-constructors-for-syntax sig-stx)
      (parameterize-constructors
       #`sts-record
       #`sts-union
       #`sts-datatype
       #`sts-forall
       #`sts-function
       #`sts-rkt
       #`sts-lit
       sig-stx))

    (define (value infer-name sig-stx)
      (parameterize-constructors-for-value sig-stx))
    (define (define-value gen-name orig-name sig-stx)
      (build-define-value gen-name (value orig-name sig-stx)))
    (define (define-transformer gen-name orig-name sig-stx)
      #`(begin
          #,(build-define-syntax
             orig-name
             (build-decl-info gen-name
                              orig-name
                              (parameterize-constructors-for-syntax sig-stx)))
          (define-for-syntax #,orig-name (syntax-local-value #'#,orig-name)))))

  (module* expr #f
    (provide expr define-expr)
    (define (parameterize-constructors-for-value stx)
      #`(syntax-parameterize
            ([record vte-record]
             [function vte-function])
          #,stx))
    (define (expr infer-name expr-stx)
      (parameterize-constructors-for-value expr-stx))
    (define (define-expr name expr-stx)
      (build-define-value name (parameterize-constructors-for-value expr-stx)))

    ;; (define (function name inp-args inp-types ret-type bodys)
    ;;   (ast:function name inp-args inp-types ret-type
    ;;                 (if (equal? (length bodys) 1)
    ;;                     (car bodys)
    ;;                     (block bodys))))
    ;; (define (define-function name inp-args inp-types ret-type bodys)
    ;;   #`(define #,name #,(function name inp-args inp-types ret-type bodys)))
    ))
