#lang racket/base 
(require "primes.rkt")
(require racket/set)
(require racket/vector)
(require racket/list)
(require racket/sequence)


(define (max-fourth limit)
  (floor (expt (- limit 12) 1/4)))

(define (max-cube limit fourth)
 (floor (expt (- limit 4 (expt fourth 4)) 1/3)))

(define (max-square limit fourth cube)
  (floor (expt (- limit (expt cube 3) (expt fourth 4)) 1/2)))

(define (filter-primes primes limit)
  (sequence->list (stop-before primes (lambda (x) (> x limit)))))
  

(define (count-e87-sums limit)
  (define primes (vector->list (sieve (integer-sqrt limit))))  
  (for*/fold ([results (set)]) 
      ([fourth (filter-primes primes (max-fourth limit))]
       [cube (filter-primes primes (max-cube limit fourth))]
       [square (filter-primes primes (max-square limit fourth cube))])
    (set-add results (+ (expt fourth 4) (expt cube 3) (expt square 2)))))

(define (count-e87-sums2 limit)
  (define primes (vector->list (sieve (integer-sqrt limit))))
  (define results (make-hash))
  (for*
      ([fourth (filter-primes primes (max-fourth limit))]
       [cube (filter-primes primes (max-cube limit fourth))]
       [square (filter-primes primes (max-square limit fourth cube))])
    (hash-set! results (+ (expt fourth 4) (expt cube 3) (expt square 2)) #t))
  (hash-count results))


(define (fold-squares limit cube fourth results primes)
  (define f (expt fourth 4))
  (define c (expt cube 3))
  (define squares (filter-primes primes (max-square limit fourth cube)))
  (define (fold-function x res) 
    (cons (+ f c (* x x)) res))
  (foldl fold-function results squares))

(define (iterate-cubes limit fourth primes)
  (define cubes (filter-primes primes (max-cube limit fourth)))
  (define (map-function cube) (fold-squares limit cube fourth (list) primes))
  (apply append (map map-function cubes)))

(define (iterate-fourths limit)
  (define primes (vector->list (sieve (integer-sqrt limit))))
  (define fourths (filter-primes primes (max-fourth limit)))
  (define (map-function fourth) (iterate-cubes limit fourth primes))
  (apply append (map map-function fourths)))

(define (count-e87-sums3 limit)
  (length (remove-duplicates (iterate-fourths limit))))

(define (count-e87-sums4 limit)
  (hash-count (make-hash (map (lambda (x) (cons x 1)) (iterate-fourths limit)))))
  
  
    
(time (count-e87-sums2 50000000))
;(time (count-e87-sums3 50000000))
;(time (count-e87-sums4 50000000))




