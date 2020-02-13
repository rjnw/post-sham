#lang racket

(require post/ast/simple
         post/ast/pp
         post/ast/rkt)

(module+ test
  (require rackunit)
  (define-signature int integer)
  (define-signature fsa
    (record
     [start symbol]
     [end (list symbol)]
     [transitions (listof (cons symbol (listof (cons symbol symbol))))]))
  (define-signature mtch
    (record [match (-> (listof symbol) bool)]))
  (define-signature dbl-md
    (record
     [build-set (-> [v (listof integer)]
                    (record [orig-list (-> (listof integer))]
                            [contains? (-> [v integer] bool)]))]))
  (define-signature (opt a)
    (union (none)
           (maybe a)))
  (define-signature opt-mod
    (record [(opt^ a) (union (none^)
                             (maybe^ a))]))
  (define-signature base
    (record (open opt-mod)))
  (parameterize ([pp:debug-metadata #f])
    (pretty-print (pp:sig fsa))
    (pretty-print (pp:sig mtch))
    (pretty-print (pp:sig dbl-md))))
