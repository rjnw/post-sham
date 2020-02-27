#lang racket

(require "ast/core.rkt"
         "ast/metadata.rkt"
         "ast/pp.rkt"
         (prefix-in wf: "ast/wf.rkt")
         (prefix-in wfe: (submod "ast/wf.rkt" expr))
         (prefix-in ce: (submod "ast/constructor.rkt" expr))
         (prefix-in cs: (submod "ast/constructor.rkt" signature)))
(provide (all-defined-out))

(define #%global-module-context (make-hash))
(define current-module-context (make-parameter #%global-module-context))
(define current-forall-input (make-parameter '()))

;; current-record
(struct record-context [value input collector prev])
(define #%global-record-context (record-context #f #%global-module-context (make-hash) #f))
(define current-record-context (make-parameter #%global-record-context))
(define (build-record-context value input collector prev)
  (record-context value input collector prev))
(define (new-record-collector) (make-hash))
(define (from-record-context rcontexts) rcontexts)
(define (from-current-record-context)
  (from-record-context (current-record-context)))

(define (check-isof? value type-sig)
  (unless (isof? value type-sig)
    (error 'post:runtime:typecheck "failed isof? test: ~a, ~a" (pp:expr value) (pp:sig type-sig)))
  value)
(define (isof? value type-sig)
  (match value
    [(? ast:expr:expr?) (equal? (ast:expr:expr-sig value) type-sig)]
    [else #f]))

(define (infer-literal value sig)
  (define (check value sig)
    (if (isof? value sig)
        value
        (error 'post:literal:type "literal does not match given type: ~a, ~a" (pp:expr value) (pp:sig sig))))
  (if (wf:signature? sig)
      (ce:lit sig (check value sig))
      (error 'post:literal:infertype "not able to infer type for literal: ~a" value)))

(define (try-coerce val sig)
  (match* (val sig)
    [(_ (ast:signature:rkt md check/c coerce)) (if (check/c val) (ast:expr:rkt sig (coerce val)) #f)]
    [((? ast:expr:expr?) (? ast:signature:signature?)) #:when (equal? (ast:expr:expr-sig val) sig) val]
    [((? ast:expr:dexpr?) (? ast:signature:signature?)) #:when (equal? (ast:decl-sig val) sig) val]
    [((? ast:expr:ex:expanded?) (? ast:signature:signature?))
     #:when (equal? (ast:decl-sig (ast:expr:ex:expanded-orig val)) sig) val]
    [(v s) (error 'post:runtime:coerce "error coercing TODO expr:\n~a\n sig:\n~a\n" (pp:expr val) (pp:sig sig))]))



;; return signature for name if found in record context o/w #f
(define (lookup-type-in-record-context name rc)
  (define (equal-bound-name? n1 d2)
    (define n2 (ast:decl-name d2))
    (match* (n1 n2)
      [((ast:name:orig ns1) (ast:name:orig ns2))
       (bound-identifier=? ns1 ns2)]
      [(_ _) #f]))
  (define (equal-symbol-name? n1 d2)
    (define n2 (ast:decl-name d2))
    (match* (n1 n2)
      [((ast:name:orig ns1) (ast:name:orig ns2))
       (equal? (syntax->datum ns1) (syntax->datum ns2))]
      [(_ _) #f]))
  (define (lookup-type-in-record-signature n sig)
    (match-define (ast:signature:record md decls) sig)
    (define decl-names (map ast:decl-name decls))
    (cond
      [(member n decls equal-bound-name?) => (compose ast:decl-sig car)]
      [(member n decls equal-symbol-name?) => (compose ast:decl-sig car)]
      [else #f]))
  (define (lookup)
    (match-define (record-context rec inp collector prev) rc)
    (match-define (ast:expr:record rec-name rec-sig md defb appb) rec)
    (lookup-type-in-record-signature name rec-sig))
  (if (eq? rc #%global-record-context) #f (lookup)))

(define (lookup-type-in-current-record-context name)
  (lookup-type-in-record-context name (current-record-context)))

(define (add-to-record-context! name value rc)
  (match-define (record-context rec inp collector prev) rc)
  (hash-set! collector name value))

(define (add-to-current-record-context! name value)
  (add-to-record-context! name value (current-record-context)))

(define (with-inferred-type full-sig inferred-sig)
  full-sig)

(define (of-type value type)
  (cond
    [(false? type) (error 'post:runtime:typecheck "could not infer type for ~a" value)]
    [(isof? value type) value]
    [(try-coerce value type)]
    [else (error 'post:runtime:typecheck "failed typechecking ~a for ~a" value (pp:sig type))]))

(define (instantiate-record rec (mc (current-module-context)))
  (match-define (ast:expr:record rec-name rec-sig md defb appb) rec)
  (match-define (metadata:ast:expr:record memo) md)
  (define (lookup-collected sym collected)
    (define (equal-name-symbol? n sym)
      (match n
        [(ast:name:orig stx) (equal? sym (syntax->datum stx))]
        [else #f]))
    (for/first ([(k v) (in-hash collected)]
                #:when (equal-name-symbol? k sym))
      v))
  (define generic-ex-record
    (make-keyword-procedure
     (λ (kws kw-args m . rst)
       (match-define (ast:expr:ex:record rec cntxt _) m)
       (match-define (record-context rec_ inp collected prev) cntxt)
       (match rst
         [(list (and s (? symbol?))) (lookup-collected s collected)]
         [else (error 'post:generic-ex-record "TODO")]))))
  (define (build-ex-record)
    (define collected (defb mc))
    (define result (ast:expr:ex:record rec collected generic-ex-record))
    (hash-set! memo mc result)
    result)
  (hash-ref! memo mc build-ex-record))

;; function context
(struct function-context [value inputs prev])
(define current-function-context (make-parameter #f))
(define (build-function-context value inputs prev)
  (function-context value inputs prev))
(define (lookup-type-in-function-context fc)
  (match-define (function-context fval inp prev) fc)
  (ast:signature:function-ret (ast:decl-sig fval)))
(define (lookup-type-in-current-function-context)
  (if (current-function-context)
      (lookup-type-in-function-context (current-function-context))
      (cs:unknown '())))
