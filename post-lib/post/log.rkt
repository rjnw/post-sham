#lang racket

(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define-logger post)
(define-logger post-compiletime post)
(define-logger post-runtime post)

(define (make-post-log-receiver topic)
  (make-log-receiver post-logger topic))

(define-syntax (build-log-receiver stx)
  #`(begin
      #,@(for/list ([topic '(fatal error warning info debug)])
           (with-syntax ([t (datum->syntax stx `(quote ,topic))]
                         [logger-id (format-id stx "post-~a-log-receiver" topic)]
                         ;; [start-log-id (format-id stx "post-start-logging-~a" topic)]
                         [show-id (format-id stx "show-log-~a" topic)])
             #`(begin
                 (define logger-id (make-log-receiver post-logger t))
                 (define (show-id)
                   (define l (sync/timeout 0.1 logger-id))
                   (when l
                     (pretty-print l)
                     (printf "~a\n" (vector-ref l 1))
                     (show-id)) ))))))

(build-log-receiver)
(log-post-info "testing post logger ~a" 42)
(show-log-info)
