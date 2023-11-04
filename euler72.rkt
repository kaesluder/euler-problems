#lang racket
(require "primes.rkt")
(define (sieve-as-list n) (vector->list (sieve n)))

(define (totient-sieve limit)
  (define primes (sieve-as-list limit))
  (for/fold ([hash1 (hash)])
    ([p primes])
    (define hash2 (hash-set hash1 p (- p 1)))
    (for/fold ([totient-hash hash2])
      ([i (in-range 2 (/ limit p))])
      (define n (* p i))
      (define old-totient (hash-ref totient-hash n n))
      (hash-set totient-hash n (* old-totient (/ (- p 1) p))))))

(define (totient-sieve2 limit)
  (define primes (sieve-as-list limit))
  (define totient-vector 
    (list->vector (sequence->list (in-range 0 (+ 1 limit)))))  
  ;;totient 1 = 0
  (vector-set! totient-vector 1 0)
  (for ([p primes]
     #:when (<= p limit))
    (vector-set! totient-vector p (- p 1))
    (for
      ([i (in-range 2 (+ 1(/ limit p)))]
       #:when (<= (* p i) limit))
      (define n (* p i))
      (define old-totient (vector-ref totient-vector n))
      (vector-set! totient-vector n (* old-totient (/ (- p 1) p)))
      ))
  totient-vector)

(define (e72) (time (for/sum ([i (totient-sieve2 1000000)]) i)))
(e72)


