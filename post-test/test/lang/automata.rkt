#lang post

(require post/racket)

(provide make-automata
         automata-inp-sig
         automata-sig)

(signature automata-inp-sig
           (record
            [start symbol]
            [end (listof symbol)]
            [transitions (listof (cons symbol (listof (cons symbol symbol))))]))

(signature automata-sig
           (record
            [match (-> (listof symbol) boolean)]))

(functor (make-automata [inp automata-inp-sig] automata-sig)
         (define match-fmap
           (make-hash (map build-transitions-f inp:transitions)))
         (define (do-transition sym inp)
           ((hash-ref match-fmap sym) inp))
         (define (build-transition-f transition)
           (cons
            (car transition
                 (function ([inp (list symbol)] bool)
                           [(empty) (if (memq (car transition) inp:end) true false)]
                           [(cons fst rst) (build-case fst
                                                       (map (build-pat rst)
                                                            (cdr transition)))]))))
         (define ((build-pat next) tpair)
           (build-case-pat (car tpair)
                           (do-transition (cdr tpair) next)))
         (value (match inp)
                (do-transition fsa:start inp)))
