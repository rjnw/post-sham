#lang racket

(require post/ast/core
         post/ast/metadata
         post/runtime
         sham/ast/simple)

(define (build-sham-module! ex-record)
  (match-define (ast:expr:ex:record orig-record collector-context _) ex-record)
  (match-define (ast:expr:record name sig md _ _) orig-record)
  (match-define (metadata:ast:expr:record memo smods) md)
  (define (translate-rkt-value val typ) (ui64 0))
  (define (translate-type typ) i64)
  (define sdefs
    (for/list ([(k v) (in-hash (record-context-collector collector-context))])
      (define kstx (ast:name-stx k))
      (cond
        [(ast:expr:rkt? v) (dglobal #f (syntax->datum kstx)
                                    (rkt->sham (ast:expr:rkt-value v) (ast:expr:expr-sig v)))]
        [(ast:expr:ex:function? v)
         (match-define (ast:expr:ex:function forig _ body _) v)
         (match-define (ast:signature:function md args ret) (ast:decl-sig forig))
         (dfunction #f (syntax->datum kstx)
                    (arg-ids args) (arg-types args) (translate-type ret)
                    (translate-expr body))])
      ))
  (define smodule (dmodule #f
                           (symbol->string (datum->syntax (ast:name-stx name)))
                           sdefs))
  (hash-set! smods ex-record smodule))
