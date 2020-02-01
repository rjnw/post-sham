#lang racket


(require (prefix-in a: (submod "core.rkt" ast))
         (prefix-in s: (submod "core.rkt" ast signature))
         (prefix-in e: (submod "core.rkt" ast expr))
         (prefix-in ma: (submod "core.rkt" _metadata ast))
         (prefix-in ms: (submod "core.rkt" _metadata ast signature))
         (prefix-in me: (submod "core.rkt" _metadata ast expr)))

(provide (all-defined-out))


(define pp:debug-metadata (make-parameter #f))
(define (pp:decl d)
  (match d
    [(a:decl md n s) `(,n ,(pp:sig s) ,@(pp:md md))]))

(define (pp:sig sig)
  (match sig
    [(s:kind md t) `(sig:kind ,(pp:sig t) ,@(pp:md md))]
    [(s:symbol md) `(sig:symbol ,@(pp:md md))]
    [(s:string md) `(sig:string ,@(pp:md md))]
    [(s:integer md) `(sig:integer ,@(pp:md md))]
    [(s:void md) `(sig:void ,@(pp:md md))]
    [(s:bool md) `(sig:bool ,@(pp:md md))]
    [(s:list md e) `(sig:list ,(pp:sig e) ,@(pp:md md))]
    [(s:cons md a d) `(sig:cons ,(pp:sig a) ,(pp:sig d) ,@(pp:md md))]
    [(s:record md name decls) `(sig:record ,name ,(map pp:decl decls) ,@(pp:md md))]
    [(s:module md name defs) `(sig:module ,name ,(map pp:decl defs) ,@(pp:md md))]
    [(s:functor md name args ret) `(sig:functor ,name ,(map pp:decl args) ,(pp:sig ret) ,@(pp:md md))]
    [(s:signature md) `(sig:unknown-signature ,(pp:md md))]
    [else `(sig:unknown ,sig)]))

(define (pp:expr e)
  (match e
    [(e:void md s) `void]
    [(e:functor md s bodyb appb) `(expr:functor ,(pp:sig s))]
    [(e:module md s defb appb) `(expr:module ,(pp:sig s))]
    [(e:record md s vals appb)
     (define (ppr d)
       (match-define (cons dcl expr) d)
       `(,(pp:decl dcl) : ,(pp:expr expr)))
     `(expr:record ,(pp:sig s) ,@(pp:md md) ,(map ppr vals))]
    [(e:let md s vars vals body)
     (define (ppv var val)
       `(,(pp:decl var) : ,(pp:expr val)))
     `(let ,(pp:sig s)
        ,(map ppv vars vals)
        ,@(pp:md md)
        ,(pp:expr body))]
    [(e:ref md s dec)
     `(expr:ref ,(pp:sig s) ,(pp:decl dec) ,@(pp:md md))]
    [(e:lit md s val)
     `(expr:lit ,(pp:sig s) ,val)]
    [(e:app md s rator rands)
     `(expr:app ,(pp:sig s) ,(pp:expr rator) ,(map pp:expr rands))]
    [(e:switch md s test branches default)
     (define (pps branch)
       `(expr:case ,(pp:expr (car branch)) ,(pp:expr (cdr branch))))
     `(expr:switch ,(pp:sig)
              ,(pp:expr test)
              ,(map pps branches)
              ,(pp:expr default))]
    [(e:begin md s es)
     `(expr:begin ,(pp:sig s) ,@(map pp:expr es))]
    [(e:while md s test body)
     `(expr:block ,(pp:sig s) ,(pp:expr test) ,(pp:expr body))]
    [(e:expr md s) `(expr:unknown-expr ,(pp:sig s))]
    [else `(expr:unknown ,e)]))

(define (pp:md md)
  (if (pp:debug-metadata)
      `(#:md
        (match md
          [(ma:decl n) `(md:decl ,n)]
          [(ms:list wf? e) `(ms:list #:wf? ,wf? #:element ,e)]
          [(ms:cons wf? a d) `(ms:cons #:wf? ,wf? #:a ,a #:d ,d)]
          [(ms:record wf? name) `(ms:record #:wf? ,wf? ,name)]
          [(ms:module wf? name) `(ms:module #:wf? ,wf? ,name)]
          [(ms:functor wf? name) `(ms:functor #:wf? ,wf? ,name)]
          [(ms:signature wf?) `(ms:signature #:wf? ,wf?)]

          [(me:functor) `(me:functor)]
          [(me:module) `(me:module)]
          [(me:record) `(me:record)]
          [(me:ref) `(me:ref)]
          [(me:let) `(me:let)]
          [(me:literal) `(me:literal)]
          [(me:app) `(me:app)]
          [(me:switch) `(me:switch)]
          [(me:block) `(me:block)]
          [(me:while) `(me:while)]
          [else `(md ,md)]))
      '()))
