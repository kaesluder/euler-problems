#lang racket
(define (map-extend-helper item li)
  (map (lambda (xs) (cons xs item)) li))

(define (map-extend li lj)
  (define (curried-meh item)
    (map-extend-helper item lj))
  (apply append (map curried-meh li)))

(define (map-extend-multiple li times)
  (define list-list (map (lambda (x) (list x)) li))
  (define (iter results count)
    (cond [(= count times) results]
          [else 
           (iter
            (map-extend results li)
            (+ 1 count))]))
  (iter list-list 1))
    