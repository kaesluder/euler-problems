#lang racket

(define (next-lowest-fraction a b q) 
  (quotient (- (* q a) 1) b))

(define (test-distance a b p q)
  (= 1 (- (* a q) (* b p))))

;;returns the higher of the fractions
(define (higher-fraction a b p q)
  (if (< (/ a b) (/ p q))
      (values p q)
      (values a b)))

(define (find-next-lowest a b limit)
  (define (iter maxa maxb x)
    (define newa (next-lowest-fraction a b x))
    (define-values (nexta nextb) (higher-fraction maxa maxb newa x))
    (if (test-distance a b newa x)
        (reduce-fraction (cons nexta nextb))
        (iter nexta nextb (- x 1))))
  (iter (next-lowest-fraction a b limit) limit (- limit 1)))

(define (reduce-fraction fraction)
  (define x (gcd (car fraction) (cdr fraction)))
  (cons (/ (car fraction) x) (/ (cdr fraction) x)))

(define (euler70) (time (find-next-lowest 3 7 1000000)))

(euler70)




    
    