#lang racket

(require post/ast/simple
         post/ast/pp
         post/interpret)

(module+ test
  (require rackunit)
  (define f (function ([a integer] [b integer] integer) a))
  (printf "interpted-value: ~a\n" (interpret (f 42 23))))
