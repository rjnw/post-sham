#lang post

(pow-input
  (sig (power int)))
(pow-sig
  (sig (pow (-> int int))))
(pow-module
  (mod (i pow-input) pow-sig
       (struct
         (val pow (λ (n : int) (gen-pow n (. i power))))
         (val gen-pow (λ (n : int) (p : int)
                         )))))

(test
 (open (compile (pow-module (struct (val power 5)))))
 (pow 10))
