#lang racket/base

(define (multiple-sum n limit)
  (let ms ((m n) (sum 0))
    (if (>= m limit)
        sum
        (ms (+ m n) (+ m sum)))))

(define (euler1 x y limit)
  (- (+ (multiple-sum x limit) (multiple-sum y limit))
     (multiple-sum (* x y) limit)))
