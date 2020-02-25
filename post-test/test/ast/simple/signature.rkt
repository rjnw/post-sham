#lang racket

(require post/ast/simple
         post/parameters/syntax
         (for-syntax post/parameters/syntax)
         post/ast/pp
         post/ast/rkt)

(module+ test
  (require rackunit)
  (define-signature int integer)
  (define-signature fsa
    (record
     [start symbol]
     [end (listof symbol)]
     [transitions (listof (cons symbol (listof (cons symbol symbol))))]))
  (define-signature u (union [a] [b]))
  (define-signature mtch
    (record [match (-> (listof symbol) boolean)]))
  (define-signature dbl-md
    (record
     [build-set (-> [v (listof integer)]
                    (record [orig-list (-> (listof integer))]
                            [contains? (-> [v integer] boolean)]))]))
  (define-signature (opt a)
    (union (none)
           (maybe a)))

  ;; (define-signature opt-mod
  ;;   (record [(opt^ a) (union (none^)
  ;;                            (maybe^ a))]))

  ;; (define-signature base
  ;;   (record (open opt-mod)))

  (parameterize ([pp:debug-metadata #f])
    (pretty-print (pp:sig fsa))
    (pretty-print (pp:sig mtch))
    (pretty-print (pp:sig dbl-md))
    (pretty-print (pp:sig (opt symbol)))))

(module test-prefix racket
  (require (prefix-in post- post/ast/simple)
           (prefix-in post- post/parameters/syntax)
           (prefix-in post- (for-syntax post/parameters/syntax))
           (prefix-in post- post/ast/rkt))
  (post-define-signature int post-integer)
  (post-define-signature fsa
    (post-record
     [start post-symbol]
     [end (post-listof post-symbol)]
     [transitions (post-listof (post-cons post-symbol (post-listof (post-cons post-symbol post-symbol))))])))
