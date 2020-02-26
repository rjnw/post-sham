#lang racket

(require (prefix-in post- post)
         )

(define fsa-sig
  (post-signature
   (post-record
       [start symbol]
       [end (listof symbol)]
     [transitions (listof (cons symbol (listof (cons symbol symbol))))])))

(define match-sig
  (post-signature
   (record [match (-> (listof symbol) boolean)])))

(define build-fsa-module
  (post-function
   ([fsa fsa-sig] match-sig)
   (post-record
    (define match-fmap
      (make-hash (map build-transition-f (fsa transitions))))
    (define (do-transition sym inp)
      ((hash-ref match-fmap sym) inp))
    (define (build-transition-f transition)
      (cons
       (car transition
            (post-function ([inp (list symbol)] bool)
                           (post-if (post-empty? inp)
                                    (post-build-switch (post-car inp)
                                                       (map (build-switch-case (post-cdr inp))
                                                            (cdr transition)))
                                    (if (memq (car transition) (fsa end))
                                        true
                                        false))))))
    (define ((build-switch-case next) tpair)
      (post-switch-case (car tpair)
                        (do-transition (cdr tpair) next)))
    (post-value (match [inp (list symbol)] bool)
                (do-transition (fsa start) inp)))))

(define-syntax (fsa-post-module stx)
  (syntax-parse stx
    [(_ name start^ (end^ ...) [state^ ([input^ next-state^] ...)] ...)
     #:with (res ...)
     (map (λ (e) (if (memq (syntax-e e) (syntax->datum #'(end ...))) #'true #'false))
          (syntax->list #'(state ...)))
     #'(post-instantiate-module
        build-fsa-module
        (post-module fsa-sig
         [post-value start 'start^]
         [post-value end '(end^ ...)]
         [post-value transitions `((state^ . ((input^ . next-state^) ...)) ...)]))]))
