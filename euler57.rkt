#lang racket
(require (planet dherman/memoize:3:1))

(define (number-to-list n)
  (define (iter n results)
    (if (= n 0)
        results
        (iter (quotient n 10) (cons (modulo n 10) results))))
  (iter n '()))

(define/memo (gen-sequence n)
  (if (= n 0) 
      2
      (+ 4/2 (/ 1/1 (gen-sequence (- n 1))))))

(define (sqr-two-series n)
  (let ([res (+ 1 (/ 1 (gen-sequence (- n 1))))])
    (cons (numerator res)(denominator res))))

(define (e57-list maxi)
  (map sqr-two-series (stream->list (in-range 1 maxi))))

(define (e57-filter-helper pairi)
  (> (order-of-magnitude (car pairi))
     (order-of-magnitude (cdr pairi))))
  
(define (e57) (length (filter e57-filter-helper (e57-list 1001))))


(define (iter prev)
  (+ 1 (/ 1 (+ 1 prev))))

(define (go)
  (define-values (_ count)
    (for/fold ([num 1] [count 0])
      ([i (in-range 1000)])
      (define next (iter num))
      (values next (if (> (order-of-magnitude (numerator next))
                          (order-of-magnitude (denominator next)))
                       (add1 count)
                       count))))
  count)
