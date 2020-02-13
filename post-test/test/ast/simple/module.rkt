#lang racket

(require post/ast/simple
         post/ast/pp)

(module+ test
  (define-record test-record
    ([am a-sig]) test-sig
    (signature (opt a) (union (none) (maybe a)))
    (value v (maybe 4))))
