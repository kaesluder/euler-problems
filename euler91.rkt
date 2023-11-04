#lang racket/base
(require racket/list)

(define (square x) (* x x))

(define (fp-equal? prec x y)
  (< (abs (- x y)) (expt 10 (* -1 prec)))) 

(define (dist-to-o x1 y1) 
  (exact->inexact (+ (square x1) (square y1))))

(define (coord-dist x1 y1 x2 y2)
  (define dif1 (- x1 x2))
  (define dif2 (- y1 y2))
  (dist-to-o dif1 dif2))

(define (right-triangle-sides? x y z)
  (define sorted (sort (list x y z) >))
  (= (first sorted) (apply + (cdr sorted))))

(define (right-triangle-coords? x1 y1 x2 y2)
  (define pq (coord-dist x1 y1 x2 y2))
  (define op (dist-to-o x1 y1))
  (define oq (dist-to-o x2 y2))
  (right-triangle-sides? pq op oq))

(define (run-series a b)
  (for*/sum ([x1 (reverse (build-list (add1 a) values))]
              [y1 (reverse (build-list (add1 b) values))]
              [x2 (reverse (build-list (add1 a) values))]
              [y2 (reverse (build-list (add1 b) values))]
              #:unless (or (= 0 x1 y1) (= 0 x2 y2))
              #:unless (and (= x1 x2) (= y1 y2))
              #:when (right-triangle-coords? x1 y1 x2 y2)
              )
            (values 1)))


(define (new-triangle x y i)
  (define newx (+ x (* i (/ y (gcd x y)))))
  (define newy (- y (* i (/ x (gcd x y)))))
  (cons newx newy))

;;find the maximum number of triangles in 
;;on the line going down and right
(define (max-i x y n)
  (min
   ;;number of steps to the limit
   (quotient (- n x) (/ y (gcd x y)))
   ;;number of steps to the y axis
   (quotient y (/ x (gcd x y)))))

;;find the maximum number of triangles
;;going up and left.
(define (min-i x y n)
  (* -1 (min 
         ;;number of steps to the limit
         (quotient (- n y) (/ x (gcd x y)))
         ;;number of steps to the x access
         (quotient x (/ y (gcd x y))))))

;;debugging function to list all the points 
;;that can be constructed from a given
;;right angle
(define (list-all-from-point x y n)
  (for/list ([i (in-range (min-i x y n) (add1 (max-i x y n)))]
             #:unless (zero? i))
    (new-triangle x y i)))

;;total number of triangles that can be 
;;consructed is simply max-i - min-i
(define (count-all-from-point x y n)
  (- (max-i x y n) (min-i x y n))) 

;;loop over all non-axis right angles 
(define (count-all-non-axis n)
  (for*/sum ([x (in-range 1 (add1 n))]
              [y (in-range 1 (add1 n))])
    (count-all-from-point x y n)))

(define (euler91 n)
  (+ 
   ;;all right angles at origin
   (square n) 
   ;;all right angles on an axis
   (* 2 (square n)) 
   ;;all right angles not on an axis
   (count-all-non-axis n)))

(time (euler91 50))

  