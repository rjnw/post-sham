#lang racket

(require post/ast/simple
         post/ast/pp)

(module+ test
  (require rackunit)
  (define int (signature integer))
  (define fsa
    (signature
     (module
         [start symbol]
         [end (list symbol)]
       [transitions (list (cons symbol (list (cons symbol symbol))))])))
  (define mtch
    (signature
     (module [match (-> (list symbol) bool)])))
  (define dbl-md
    (signature
     (module
         [build-set (-> [v (list integer)]
                        (module [orig-list (-> (list integer))]
                            [contains? (-> [v integer] bool)]))])))
  (parameterize ([pp:debug-metadata #f])
    (pretty-print (pp:sig fsa))
    (pretty-print (pp:sig mtch))
    (pretty-print (pp:sig dbl-md))))
