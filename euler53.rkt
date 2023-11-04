#lang racket
(define (my-factorial x)
  (cond
    [(= x 0) 1]
    [else (for/product ([i (my-range x)]) i)]
    ))

(define (my-range x) 
  (for/list ([i (in-range x)]) (+ i 1)))

(define (my-factorial-div x y)
  (for/product ([i (in-range y x)]) (+ i 1)))

(define (combinations n r)
  (/ (my-factorial-div n r) (my-factorial (- n r)))) 

(define (gen-combinations l)
  (for*/list ([n (my-range l)]
              [r (my-range n)])
    (combinations n r)))

(define (filter-combinations l min)
  (filter (lambda (x) (> x min)) (gen-combinations l)))

(define (count-greater-than min row)
  (length (filter (lambda (x) (> x min)) row)))

(define (gen-triangle steps)
  (define (iter tri results x)
    (cond 
      [(> x steps) (+ results (count-greater-than 1000000 tri))]
      [else (iter 
             (map + (cons 0 tri) (append tri '(0)))
             (+ results (count-greater-than 1000000 tri))
             (+ x 1))]))
      (iter '(1) 0 1))
    