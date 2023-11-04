#lang racket
(require "primes.rkt")

(define (partition-primes n)
  (define sieve-vector (make-vector (add1 n) 0))
  (vector-set! sieve-vector 0 1)
  ;; generate the primes less than n
  (define primes (sieve n))
  (for ([i primes])
    (for ([j (in-range i (add1 n))])
      (vector-set! sieve-vector j
                   (+ (vector-ref sieve-vector j)
                      (vector-ref sieve-vector (- j i))))))
  (values (vector-ref sieve-vector (ceiling n))))

(define (e77)
  (for/first ([n (in-naturals 1)]
              #:when (< 5000 (partition-primes n)))
    n))