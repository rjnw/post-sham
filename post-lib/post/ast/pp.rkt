#lang racket

(module _pp racket
  (require (prefix-in a: (submod "core.rkt" ast))
           (prefix-in s: (submod "core.rkt" ast signature))
           (prefix-in e: (submod "core.rkt" ast expr))
           (prefix-in ma: (submod "metadata.rkt" _metadata ast))
           (prefix-in ms: (submod "metadata.rkt" _metadata ast signature))
           (prefix-in me: (submod "metadata.rkt" _metadata ast expr)))
  (provide (all-defined-out))
  (define debug-metadata (make-parameter #f))
  (define (decl d)
    (match d
      [(a:decl m n s) `(,n ,(sig s) ,@(md m))]))

  (define (sig s)
    (match s
      [(s:kind m t) `(sig:kind ,(sig t) ,@(md m))]
      [(s:symbol m) `(sig:symbol ,@(md m))]
      [(s:string m) `(sig:string ,@(md m))]
      [(s:integer m) `(sig:integer ,@(md m))]
      [(s:void m) `(sig:void ,@(md m))]
      [(s:bool m) `(sig:bool ,@(md m))]
      [(s:list m e) `(sig:list ,(sig e) ,@(md m))]
      [(s:cons m a d) `(sig:cons ,(sig a) ,(sig d) ,@(md m))]
      [(s:record m name decls) `(sig:record ,name ,(map decl decls) ,@(md m))]
      [(s:module m name defs) `(sig:module ,name ,(map decl defs) ,@(md m))]
      [(s:functor m name args ret) `(sig:functor ,name ,(map decl args) ,(sig ret) ,@(md m))]
      [(s:signature m) `(sig:unknown-signature ,(md m))]
      [else `(sig:unknown ,s)]))

  (define (expr e)
    (match e
      [(e:void m s) `void]
      [(e:functor m s bodyb appb) `(expr:functor ,(sig s))]
      [(e:module m s defb appb) `(expr:module ,(sig s))]
      [(e:record m s vals appb)
       (define (ppr d)
         (match-define (cons dcl expr) d)
         `(,(decl dcl) : ,(expr expr)))
       `(expr:record ,(sig s) ,@(md m) ,(map ppr vals))]
      [(e:let m s vars vals body)
       (define (ppv var val)
         `(,(decl var) : ,(expr val)))
       `(let ,(sig s)
          ,(map ppv vars vals)
          ,@(md m)
          ,(expr body))]
      [(e:ref m s dec)
       `(expr:ref ,(sig s) ,(decl dec) ,@(md m))]
      [(e:lit m s val)
       `(expr:lit ,(sig s) ,val)]
      [(e:app m s rator rands)
       `(expr:app ,(sig s) ,(expr rator) ,(map expr rands))]
      [(e:switch m s test branches default)
       (define (pps branch)
         `(expr:case ,(expr (car branch)) ,(expr (cdr branch))))
       `(expr:switch ,(sig)
                     ,(expr test)
                     ,(map pps branches)
                     ,(expr default))]
      [(e:begin m s es)
       `(expr:begin ,(sig s) ,@(map expr es))]
      [(e:while m s test body)
       `(expr:block ,(sig s) ,(expr test) ,(expr body))]
      [(e:expr mp s) `(expr:unknown-expr ,(sig s))]
      [else `(expr:unknown ,e)]))

  (define (md m)
    (if (debug-metadata)
        `(#:md
          (match m
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
            [else `(md ,m)]))
        '())))

(require (prefix-in pp: (submod "." _pp)))
(provide (all-from-out (submod "." _pp)))
