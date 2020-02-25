#lang racket

(require post/ast/simple
         post/ast/rkt
         post/ast/pp
         post/interpret
         post/parameters/syntax)

(module+ test
  (require rackunit)
  (define f (expr (function ([a integer] [b integer] integer) a)))
  ;; (printf "interpted-value: ~a\n" (interpret (f 42 23)))
  )
