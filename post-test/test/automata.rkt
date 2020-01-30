#lang racket

(require (prefix-in post- post))

(define fsa-sig
  (post-signature
   (module
       [start symbol]
       [end (list symbol)]
     [transitions (list (cons symbol (list (cons symbol symbol))))])))

(define match-sig
  (post-signature
   (module [match (-> (list symbol) bool)])))

(define build-fsa-module
  (post-functor ([fsa fsa-sig]) match-sig
                (post-module
                 (define match-fmap
                   (make-hash (map build-transition-f (fsa transitions))))
                 (define (do-transition sym inp)
                   ((hash-ref match-fmap sym) inp))
                 (define (build-transition-f transition)
                   (cons
                    (car transition
                         (post-lambda ([inp (list symbol)] bool)
                                      (post-if (post-empty? inp)
                                               (post-build-switch (post-car inp)
                                                                  (map (build-switch-case (post-cdr inp))
                                                                       (cdr transition)))
                                               (if (memq (car transition) (fsa end))
                                                   post-true
                                                   post-false))))))
                 (define ((build-switch-case next) tpair)
                   (post-switch-case (car tpair)
                                     (do-transition (cdr tpair) next)))
                 (post-define (match [inp (list symbol)] bool)
                              (do-transition start inp)))))

(define-syntax (fsa-post-module stx)
  (syntax-parse stx
    [(_ name start^ (end^ ...) [state^ ([input^ next-state^] ...)] ...)
     #:with (res ...)
     (map (λ (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'true #'false))
          (syntax->list #'(state ...)))
     #'(post-instantiate-module
        build-fsa-module
        (post-module fsa-sig
         [post-define start 'start^]
         [post-define end '(end^ ...)]
         [post-define transitions `((state^ . ((input^ . next-state^) ...)) ...)]))]))
