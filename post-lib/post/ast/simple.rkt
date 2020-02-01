#lang racket

(require syntax/parse/define)

(require (for-syntax (prefix-in ss: (submod "syntax.rkt" ast signature))
                     (prefix-in se: (submod "syntax.rkt" ast expr))
                     racket/pretty))
(provide (all-defined-out))

(define-syntax-parser function
  [(_ ([inp-args:id (~optional (~datum :)) inp-types:expr] ... (~optional (~datum :)) ret-type:expr)
      body:expr ...)
   (se:functor (syntax-local-name)
               (syntax->list #`(inp-args ...))
               (syntax->list #`(inp-types ...))
               #`ret-type
               (se:begin (ss:signature #f #`ret-type) (syntax->list #`(body ...))))])

(define-syntax-parser signature
  [(_ s) (ss:signature (syntax-local-name) #'s)])

;; (define-syntax (module stx)
;;   (syntax-parse stx
;;     [(_ ([inp:id (~optional (~datum :)) t:expr] ...) sig:expr
;;         parts:expr ...) #'42]))

;; (define-syntax (val stx)
;;   (syntax-parse stx
;;     [(_ name:id v:expr)
;;      #`(post-add-val-to-current-module name v)]
;;     [(_ (name:id (inp-args:id (~optional (~datum :)) inp-types:expr) ... (~optional (~datum :)) ret-type:expr)
;;         body:expr ...)
;;      #`(post-add-val-to-current-module
;;         name
;;         (post-function name (inp-args ...) (-> inp-types ... ret-type)
;;                        (post-begin body ...)))]))

;; (define-simple-macro (add-val-to-current-module name value)
;;   (if (not (false? (post-current-module)))
;;       (post-add-to-module 'name (post-current-module) value)
;;       (error 'post "using val outside post-module")))

;; (define (add-to-module name mod value)
;;   (printf "adding ~a to mod ~a\n" name (post-module-name mod)))
