#lang racket/base

;;modular power function.
;;modular power using fast exponentiation
;;and calculating the modulus of intervening steps. 
(define (modular-pow base exponent modulus)
  (define (iter base exponent m result)
    (if (<= exponent 0)
        result
        (let ([newbase (modulo (* base base) m)]
              [newresult (if (= 1 (bitwise-and 1 exponent))
                             (modulo (* result base) m)
                             result)]
              [newxp (arithmetic-shift exponent -1)])
          (iter newbase newxp m newresult))))
  (iter base exponent modulus 1))


(define (euler97)
  (modulo (+ 1 (* 28433 (modular-pow 2 7830457 (expt 10 10)))) (expt 10 10)))

(time (euler97))
