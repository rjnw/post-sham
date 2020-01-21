#lang racket

(require post)

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
  (post-module (fsa fsa-sig) match-sig
               (define match-fmap
                 (make-hash (map build-transition-f (fsa 'transitions))))
               (define (build-transition-f transition)
                 (cons
                  (car transition
                       (post-lambda (inp)
                                    (post-if (list-empty? inp)
                                             (post-switch (post-car inp)
                                                          (map (build-switch-case
                                                                (post-cdr inp))
                                                               (cdr transition)))
                                             (if (memq (car transition) (fsa 'end))
                                                 post-true
                                                 post-false))))))
               (define ((build-switch-case next) tpair)
                 (switch-case (car tpair)
                              (post-app (hash-ref match-fmap (cdr tpair)) next)))
               (val (match (inp (list int)) bool)
                    (post-app (hash-ref match-fmap start) inp))))

(define-syntax (fsa-post-module stx)
  (syntax-parse stx
    [(_ name start^ (end^ ...) [state^ ([input^ next-state^] ...)] ...)
     #:with (res ...)
     (map (λ (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'true #'false))
          (syntax->list #'(state ...)))
     #'(post-instantiate-module
        build-fsa-module
        (post-module
         () fsa-sig
         [val start 'start^]
         [val end '(end^ ...)]
         [val transitions `((state^ . ((input^ . next-state^) ...)) ...)]))]))
