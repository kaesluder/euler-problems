#lang racket


;;The 5-digit number, 16807=7^5, is also a fifth power. 
;;Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
;; How many n-digit positive integers exist which are also an nth power?


(define (oom-predicate? base power)
  (= power (+ 1 (order-of-magnitude (expt base power)))))


(define (e63-loop1 x y)
  (for*/list ([base (in-range 1 x)]
              [power (in-range 1 y)]
              #:when (oom-predicate? base power)
              )
    (list base power (expt base power))))
    
(define (e63) (length (e63-loop1 10 23)))

(time (e63))
