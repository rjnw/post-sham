#lang racket

(require (for-syntax syntax/parse))

(define (build-post-struct sig vals)
  ())

(define-syntax (post-struct stx)
  (syntax-parse stx
    [(_ sig:expr [s:id v:expr] ...)
     #`(build-post-struct sig (make-hash `((s . ,v) ...)))]))
