#lang racket/base
(require racket/list racket/vector racket/math)

(define (test-case n)
  ;useful test case of n^3
  (expt n 3))

(define (target-case-old n)
  ;;full expansion of the target case
  (+ 1
     (* n -1)
     (expt n 2)
     (* (expt n 3) -1)
     (expt n 4)
     (* (expt n 5) -1)
     (expt n 6)
     (* (expt n 7) -1)
     (expt n 8)
     (* (expt n 9) -1)
     (expt n 10)))

(define (target-case2 n)
  (for/sum ([i (in-range 11)]
            [j (list 1 -1 1 -1 1 -1 1 -1 1 -1 1)])
    (* j (expt n i))))

(define (target-case n)
  ;;the target case defined by euler problem 101
  (define (helper x y) (* x (expt n y)))
  (define exponents (build-list 11 values))
  (define coeffs (list 1 -1 1 -1 1 -1 1 -1 1 -1 1))
  (apply + (map helper coeffs exponents)))

(define (n-2 x1 x2 y1 y2)
  (+ (* 3 (/ (- y2 y1)(- x2 x1))) (/ (- (* x2 y1) (* x1 y1)) (- x2 x1))))

(define (points fn n)
  ;creates a vector f(n) for 1..n
  (for/vector ([i (in-range 1 (+ 1 n))])
    (fn i)))

(define (lagrange fn n)
  ;performs lagrange polynomial fitting for 
  ;polynomials up to degree n
  (define p (points fn n))
  (for/sum ([i (in-range 1 (+ 1 n))])
    (define coefficient 
      (for/product ([j (in-range 1 (+ 1 n))]
                    #:when (not (= i j)))
        (/ (+ 1 n (- j))
           (- i j))))
    (* coefficient (vector-ref p (- i 1)))))


(define (euler101)
  (for/sum ([i (in-range 1 11)])
    (lagrange target-case i)))

(time (euler101))



      
    


  


