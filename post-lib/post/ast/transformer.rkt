#lang racket

(module info racket
  (require (for-template racket))
  (provide (except-out (all-defined-out)
                       transformer))
  (struct ast [])
  (define (transformer v . args)
    (if (and (equal? (length args) 1)
             (syntax? (car args)))
        (with-syntax ([orig (decl-orig-name v)])
          (syntax-case (car args) ()
            [(self . args) #`(orig . args)]
            [_ #`orig]))
        (apply (decl-info v) args)))
  (struct decl ast [orig-name info] #:property prop:procedure transformer)
  (module* signature #f
    (provide (except-out (all-defined-out)
                         generic-sig
                         forall-app))
    (define (generic-sig s . args)
      (error "using post signature information as a function"))
    (struct signature [] #:property prop:procedure generic-sig)
    (struct type signature [])
    (struct lit signature [])
    (struct rkt signature [])
    (struct function signature [argns args ret])
    (struct union signature [ctors subs])
    (struct datatype signature [ctor args])
    (struct record signature [defs sigs])
    (define (forall-app s . args) (forall-sig s)) ;; TODO
    (struct forall signature [binds sig] #:property prop:procedure forall-app))
  (module* expr #f
    (provide (all-defined-out))
    (struct expr [])
    (struct dexpr decl [])
    (struct function dexpr [args])
    (struct record dexpr [defs])))

(module* signature #f
  (require (for-template racket)
           (for-syntax syntax/parse)
           racket/stxparam
           (prefix-in t- (submod ".." info))
           (prefix-in ti- (submod ".." info signature))
           (prefix-in p- post/parameters/syntax))
  (provide define-signature-transformer)
  (define (build-decl-info gen-name orig-name vstx)
    (with-syntax ([gn gen-name])
      #`(t-decl #'#,gen-name #,vstx)))
  (define (define-signature-transformer gen-name orig-name sig-stx)
    #`(begin
        (define-syntax #,orig-name
          #,(build-decl-info gen-name
                             orig-name
                             (parameterize-for-signature-t sig-stx)))
        (define-for-syntax #,orig-name (syntax-local-value #'#,orig-name))))

  (begin-for-syntax (define (tsig-record stx)
                      (syntax-parse stx
                        [(_ (c:id t:expr) ...)
                         #`(ti-record (list #'c ...) (list t ...))]))
                    (define (tsig-union stx)
                      (syntax-parse stx
                        [(_ (c:id ts:expr ...) ...)
                         #`(ti-union (list #'c ...)
                                     (list #,@(map (λ (c t) #`(p-datatype #,c #,t))
                                                   (syntax->list #`(c ...))
                                                   (syntax->list #`((ts ...) ...)))))]))
                    (define (tsig-datatype stx)
                      (syntax-parse stx
                        [(_ c:id [v:id ...])
                         #`(ti-datatype #'c #'(v ...))]))
                    (define (tsig-forall stx)
                      (syntax-parse stx
                        [(_ [v:id ...] t:expr)
                         #`(ti-forall (list #'v ...) t)]))
                    (define (tsig-function stx)
                      (syntax-parse stx
                        [(_ [v:id vt:expr] ... ret:expr)
                         #`(ti-function (list #'v ...) (list vt ...) ret)]))
                    (define (tsig-rkt stx) #`(ti-rkt))
                    (define (tsig-lit stx) #`(ti-lit)))
  (define (parameterize-for-signature-t stx)
    #`(syntax-parameterize
          ([p-record tsig-record]
           [p-function tsig-function]
           [p-union tsig-union]
           [p-datatype tsig-datatype]
           [p-forall tsig-forall]
           [p-rkt tsig-rkt]
           [p-lit tsig-lit])
        #,stx)))
