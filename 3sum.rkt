#lang racket/base

(define (iter1 li)
    (cond 
      [(< (length li) 3)
       (list)]
      [else 
       (define a (car li))
       (define result (iter2 (cdr li) a))
       (cond
         [result result]
         [else (iter1 (cdr li))])]))

(define (iter2 li a)
  (cond 
    [(< (length li) 2) #f]
    [else 
     (define b (car li))
     (define c (member (* -1 (+ a b)) (cdr li)))
     (if c (list a b (car c)) (iter2 (cdr li) a))]))



(define (3sum li) 
  (iter1 li)
       
  
  
  )
