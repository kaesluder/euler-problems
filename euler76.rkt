#lang racket
(define (sieve-ways n limit)
  (define sieve-vector (make-vector (add1 limit) 0))
  (vector-set! sieve-vector 0 1)
  (for ([i (in-range 1 (add1 n))])
    (for ([j (in-range i (add1 limit))])
      (vector-set! sieve-vector j
                   (+ (vector-ref sieve-vector j)
                      (vector-ref sieve-vector (- j i))))))
  (values sieve-vector))

(define (e76) (vector-ref (sieve-ways 99 100) 100))
  

