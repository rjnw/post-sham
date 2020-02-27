#lang racket

(require post/ast/simple
         post/parameters/syntax
         post/ast/rkt
         post/ast/pp
         post/runtime
         post/interpret)

(module+ test
  ;; simple
  (define test1-record-sig (signature (record [v integer])))
  (define test1-record (expr (record test1-record-sig (value v 42))))
  ;; (pp:ex (instantiate-record test1-record))

  ;;
  (define test2-record-sig-inp test1-record-sig)
  (define test2-record-sig (signature (record [s string])))
  (define test2-functor
    (expr
     (function ([i test2-record-sig-inp] test2-record-sig)
               (record
                (value s (number->string (i 'v)))))))
  )
