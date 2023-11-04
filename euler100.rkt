#lang racket

(define (roots x2 x y)
  (for*/fold ([results (list)])
    ([a (in-range 1 (quotient x2 x))]
     [b (in-range 1 (quotient x2 a))])
    (cons (list a b (- x2 (+ (* a x) (* b y)))) results)))

(define (next-value x y root-list)
  (define a (first root-list))
  (define b (second root-list))
  (define c (third root-list))
  (+ (* x a) (* y b) c))

(define (test-roots a b roots1 roots2)
  (define c (next-value a b roots1))
  (define d (next-value a b roots2))
  (= 1 (* 2 (/ c d) (/ (- c 1) (- d 1)))))

(define (find-roots x y x2 y2)
  (for*/or
      ([roota (roots x2 x y)]
       [rootb (roots y2 x y)])
    (if (test-roots x2 y2 roota rootb)
        (list roota rootb) #f)))

(define (recursive-solutions x y limit roots)
  (cond 
    [(> y limit) x]
    [else (recursive-solutions (next-value x y (first roots))
                               (next-value x y (second roots))
                               limit
                               roots)]))

(define (euler-100)
  (recursive-solutions 15 21 (expt 10 12) (find-roots 15 21 85 120)))

(time (euler-100))

  


  
