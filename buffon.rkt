#lang racket

(define (buffon n)
  (let loop ((drops 0) (hits 0))
    (if (= drops n) (/ drops hits 1.0)
      (if (< (random) (/ (sin (* 1.571 (random))) 2.0))
          (loop (+ drops 1) (+ hits 1))
          (loop (+ drops 1) hits)))))
