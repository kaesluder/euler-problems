#lang racket

(define (tri-height base side)
  (sqrt (- (* side side) (/ (* base base) 4))))

(define (tri-area base side)
  (/ (* base (tri-height base side)) 2))

(define (count-almost-triangles perimeter-limit)
  (for*/fold ( [triangles (list)])
      ([side (in-range 2 (+ 1 (quotient perimeter-limit 3)))]
       [x '(-1 1)])
    (define base (+ side x))
    (cond 
      [(and 
        (exact? (tri-area base side))
        (integer? (tri-area base side)))
       (values (cons (cons base side) triangles))]
      [else (values triangles)])))
    
    
(define (count-almost-triangles2 perimeter-limit)
  (for*/fold ([perimeter 0])
      ([side (in-range 2 (+ 1 (quotient perimeter-limit 3)))]
       [x '(-1 1)])
    (define base (+ side x))
    (cond 
      [(and 
        (exact? (tri-area base side))
        (integer? (tri-area base side))) 
       (values (+ perimeter base (* 2 side)))]
      [else (values perimeter)])))


;;formula for producing a 
;;pythagorean triplet 
;;m > n
;;m coprime n
(define (pythag-a m n) 
  (- (* m m) (* n n)))
(define (pythag-b m n)
  (* 2 m n))
(define (pythag-c m n)
  (+ (* m m) (* n n)))

(define (pass-lesser a b)
  (if (< a b)
      (values a)
      (values b)))

;;generate triplets from primitives.
;;limit is length of longest side
(define (gen-primitive-triples limit)
  (define max-m (integer-sqrt limit))
  (define (test b c) (= 1 (abs (- c (* 2 b)))))
  ;;formula for max-n double-checked through 
  ;;wolfram alpha
  (define (max-n m) (min (/ limit (* 2 m)) m))
  (for*/fold ([results (list)])
      ([m (in-range 2 max-m)]
              [n (in-range 1 (max-n m))]
              #:when (= 1 (gcd m n))
              #:when (odd? (- m n)))
    (define a (pythag-a m n))
    (define b (pythag-b m n))
    (define c (pythag-c m n))
    (cond [(or (test b c) (test a c)) 
           (cons (sort (list a b c) <) results)]
          [else results])))

(define (repeat li count)
  (for/fold ([result '()])
    ([i (in-range 0 count)])
    (append li result)))

(define (convergents times)
  (define (iter x y results count)
    (cond 
      [(= times count) (reverse results)]
      [else 
       (define nextx (+ (* 2 x) (* y 3)))
       (define nexty (+ (* y 2) x))
       (iter nextx nexty (cons (cons x y) results) (+ 1 count))]))
  (iter 2 1 (list) 0))

(define (a1 x) 
  (* 1/3 (+ 1 (* 2 x))))

(define (a2 x)
  (* 1/3 (+ -1 (* 2 x))))

(define (area1 x y)
  (* y (+ x 2) 1/3))

(define (area2 x y)
  (* y (- x 2) 1/3))

(define (valid? x)
  (and (integer? x)
       (positive? x)))


(define (base x y)
  (* 2 (sqrt (- (* x x) (* y y)))))

(define (test-convergents times limit)
  (for/fold
      ([results (list)])
      ([i (convergents times)])
    (define x (car i))
    (define y (cdr i))
    (define a (a1 x))
    (define b (a2 x))
    (define area-a (area1 x y))
    (define area-b (area2 x y))
    (cond
      [(> a (quotient limit 3)) results]
       [(> b (quotient limit 3)) results]
       [(and (valid? a)
             (valid? area-a))
        (cons (list a y (base a y) (+ a a (base a y))) results)]
       [(and (valid? b)
             (valid? area-b))
        (cons (list b y (base b y) (+ b b (base b y))) results)]
       [else results])))
        
     
(define (euler94)

  (apply + (map last (test-convergents 20 1000000000))))

(display (time (euler94)))
(newline)