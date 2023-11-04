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

(define (reduce-fraction fraction)
  (define x (gcd (car fraction) (cdr fraction)))
  (cons (/ (car fraction) x) (/ (cdr fraction) x)))

(define (find-next-lowest a b limit)
  (define (iter maxa maxb x)
    (define newa (next-lowest-fraction a b x))
    (define-values (nexta nextb) (higher-fraction maxa maxb newa x))
    (if (test-distance a b newa x)
        (reduce-fraction (cons nexta nextb))
        (iter nexta nextb (- x 1))))
  (iter (next-lowest-fraction a b limit) limit (- limit 1)))

(define (farey-p a b c d n)
  (- (* (floor (/ (+ n b) d)) c) a))

(define (farey-q a b c d n)
  (- (* (floor (/ (+ n b) d)) d) b))

(define (build-farey-sequence start finish n)
  (define c (car start))
  (define d (cdr start))
  (define prev (find-next-lowest c d n))
  (define a (car prev))
  (define b (cdr prev))
  (define finish-frac (/ (car finish) (cdr finish)))
  (define (iter a b c d results)
    (define p (farey-p a b c d n))
    (define q (farey-q a b c d n))
    (if (>= (/ p q) finish-frac)
        (reverse results)
        (iter c d p q (cons (cons p q) results))))
  (iter a b c d '()))

(define (count-farey-sequence start finish n)
  (define c (car start))
  (define d (cdr start))
  (define prev (find-next-lowest c d n))
  (define a (car prev))
  (define b (cdr prev))
  (define finish-frac (/ (car finish) (cdr finish)))
  (define (iter a b c d result)
    (define p (farey-p a b c d n))
    (define q (farey-q a b c d n))
    (if (>= (/ p q) finish-frac)
        result
        (iter c d p q (+ 1 result))))
  (iter a b c d 0))

(define (count-farey-recursive a b n)
  (define medd (+ a b))
  (if (> medd n)
      0
      (+ 1 (count-farey-recursive a medd n) (count-farey-recursive medd b n))))
  
(define (e73) (count-farey-recursive 3 2 12000))
(time (e73))

