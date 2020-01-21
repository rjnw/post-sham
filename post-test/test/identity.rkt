#lang racket

(require post)

(define test-signature
  (post-signature

   (val id (forall (a) (-> a a)))))

(define test-module
  (post-module () #:signature test-signature
               (post-struct (function id (λ (i : a) i)))))
