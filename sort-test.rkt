#lang racket

(define (sorted? proc lst)
  (for/and ([a lst]
            [b (cdr lst)])
    (proc a b)))