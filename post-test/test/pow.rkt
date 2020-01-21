#lang racket

(require post)

(define pow-input
  (post-signature
   (val power int)))
(define pow-signature
  (post-signature
   (val pow (int -> int))))

(define pow-module
  (post-module (i pow-input) #:signature pow-signature
               (post-struct
                (define (gen-pow n p res)
                  (post-if (post-equal? p 0)
                           res
                           (gen-pow n (sub1 p) (post-* res n))))
                (val (pow (n : int))
                     (gen-pow n (. i power) 1)))))
(define 5pow-module
  (post-instantiate pow-module
                    (post-struct #:sig pow-input (val power ($ 5 int)))))

(define compiled-5pow-module (post-compile 5pow-module))

((. compiled-5pow-module pow) 10)
