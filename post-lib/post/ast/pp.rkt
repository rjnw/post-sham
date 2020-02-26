#lang racket

(module _pp racket
  (require (prefix-in a: (submod "core.rkt" ast))
           (prefix-in n: (submod "core.rkt" ast name))
           (prefix-in s: (submod "core.rkt" ast signature))
           (prefix-in e: (submod "core.rkt" ast expr))

           (prefix-in ma: (submod "metadata.rkt" _metadata ast))
           (prefix-in ms: (submod "metadata.rkt" _metadata ast signature))
           (prefix-in me: (submod "metadata.rkt" _metadata ast expr)))
  (provide (all-defined-out))
  (define debug-metadata (make-parameter #f))
  (define (decl d)
    (match d
      [(a:decl n s) `($ ,(name n) ,(sig s))]
      [else `(decl:unknown ,d)]))

  (define (name n)
    (match n
      [(n:orig s) `(n:orig ,s)]
      [(n:gen s) `(n:gen ,s)]
      [(? syntax?) (syntax->datum n)]
      [(? false?) '_]
      [else n]))
  (define (sig s)
    (match s
      [(s:function md args ret) `(sig:function ,(map decl args) ,(sig ret))]
      [(s:record md decls) `(sig:record ,(map decl decls))]
      [(s:type md b) `(sig:type ,(sig b))]
      [(s:lit md sham check coerce) `(sig:lit )]
      [(s:rkt md check coerce) `(sig:rkt )]
      [(s:union md subtypes) `(sig:union ,(map decl subtypes))]
      [(s:datatype md ctor args) `(sig:datatype ,(map decl args))]
      [(s:forall md binds typeb appb) `(sig:forall ,(map decl binds))]
      [(s:signature m) `(sig:unknown-signature ,(md m))]
      [else `(sig:unknown ,s)]))

  (define (expr e)
    (match e
      [(e:function n s m bodyb appb) `(expr:function ,(sig s) ,@(md m))]
      [(e:record n s m defb appb) `(expr:record ,(sig s) ,@(md m))]
      [(e:let s vars vals bodyb)
       (define (ppv var val)
         `(,(decl var) : ,(expr val)))
       `(let ,(sig s)
          ,(map ppv vars vals))]
      [(e:mref mod s dec)
       `(expr:ref ,(sig s) ,(decl dec))]
      [(e:lit s val)
       `(expr:lit ,(sig s) ,val)]
      [(e:rkt s val)
       `(expr:rkt ,(sig s) ,val)]
      [(e:app s md rator rands)
       `(expr:app ,(sig s) ,(expr rator) ,(map expr rands))]
      [(e:case s test branches)
       `(expr:case ,(sig)
                   ,(expr test) 'todo:pat)]
      [else `(expr:unknown ,e)]))

  (define (md m)
    (if (debug-metadata)
        `(#:md
          (match m
            [(ma:decl n) `(md:decl ,n)]
            [(ms:record wf? name) `(ms:module #:wf? ,wf? ,name)]
            [(ms:forall wf? memo) `(ms:functor #:wf? ,wf? ,name)]
            [(ms:signature wf?) `(ms:signature #:wf? ,wf?)]

            [(me:function) `(me:functor)]
            [(me:record) `(me:record)]
            [(me:app) `(me:app)]
            [else `(md ,m)]))
        '())))

(require (prefix-in pp: (submod "." _pp)))
(provide (all-from-out (submod "." _pp)))
