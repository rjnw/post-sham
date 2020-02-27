#lang racket

(require post/ast/simple
         post/parameters/syntax
         post/ast/rkt
         post/ast/pp
         post/runtime
         post/interpret)

(module+ test
  (define test3-inp-record-sig
    (signature (record
                [start symbol]
                [end (listof symbol)]
                [transitions (listof (cons symbol (listof (cons symbol symbol))))])))
  (define test3-record-sig
    (signature (record
                [match (-> (listof symbol) boolean)])))
  (define test3-inp-record
    (expr (record test3-inp-record-sig
                  [value start 'C]
                  [value end '(R)]
                  [value transitions
                         `((C . ((A . A)))
                           (A . ((A . A)
                                 (D . D)
                                 (R . R)))
                           (D . ((D . D)
                                 (R . R)))
                           (R . ()))])))
  (define test3-make
    (expr (function ([fsa test3-inp-record]) test3-record-sig
                    (record
                     (define match-fmap
                       (make-hash (map build-transition-f (fsa 'transitions))))
                     (define (do-transition sym inp)
                       ((hash-ref match-fmap sym) inp))
                     (define (build-transition-f transition)
                       (cons
                        (car transition
                             (function ([inp (list symbol)] bool)
                                       (case inp
                                         [empty (if (memq (car transition) (fsa 'end))
                                                    true
                                                    false)]
                                         [(cons fst rst)
                                          (build-case fst
                                                      (map (build-case-pat rst)
                                                           (cdr transition)))])))))
                     (define ((build-case-pat next) tpair)
                       (build-case-pat (car tpair)
                                       (do-transition (cdr tpair) next)))
                     (value (match [inp (list symbol)] bool)
                            (do-transition (fsa 'start) inp)))))))
