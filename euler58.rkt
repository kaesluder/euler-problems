#lang racket

;;set up primality testing using
;;the functions in primes.rkt
(require "primes.rkt")


;;(define primeset (sieve 30000))
;;(define primelist (vector->list primeset))

;;curry this function to specify the 
;;primeset
(define (is-p? n)
  (is-prime-mr? n))

;;function for the side-length from number of iterations
(define (side-length n) (+ 1 (* 2 n)))

;;generate the three possible prime corner from n.
;;remaining corner is side-length^2
(define (gen-sides n) 
  (let ([y (side-length n)])
    (for/list ([i (in-range 1 4)])
      (+ (* y y) (* y i -1) i)))) 

;;count prime corners using filter/length idiom
(define (count-prime-sides n) 
  (length (filter is-p? (gen-sides n))))

(define (side-formula n x)
  (let ([y (side-length n)])
    (+ (* y y) (* y x -1) x)))

;;a second way to count corners 
;;runs slightly slower.
(define (count-prime-sides2 n) 
  (for/sum ([i (in-range 1 4)])
           (if (is-p? (side-formula n i))
               1
               0)))

;;number of diagonals including 1 in the 
;;center
(define (diagonals n) (+ 1 (* n 4)))

;;recursive iteration. 
;;should use tail-call optimization. 
(define (e58) 
  (define (iter n primesum)
    (if (and (> primesum 0) (< (* 10 primesum) (diagonals n)))
        (side-length n)
        (iter (+ n 1) (+ primesum (count-prime-sides n)))))
  (iter 1 0))