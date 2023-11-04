#lang racket
(require (planet dherman/memoize:3:1))

(define/memo (partition k n)
  (cond [(> k n) 0]
        [(= k n) 1]
        [else (+ (partition (add1 k) n) (partition k (- n k)))]))



(define/memo (partition-generating n)
  (cond [(< n 0) 0]
        [(= n 0) 1]
        [else (for/fold ([pn 0])
                  ([k (in-range 1 (add1 (integer-sqrt n)))])
                (define n1 (- n (* 1/2 k (- (* 3 k) 1))))
                (define n2 (- n (* 1/2 k (+ (* 3 k) 1))))
                (define pn1 (partition-generating n1))
                (define pn2 (partition-generating n2))
                (cond [(odd? k) (+ pn pn1 pn2)]
                      [else (- pn pn1 pn2)]))]))



(define/memo (partition-generating-modulo n mod)
  (cond [(< n 0) 0]
        [(= n 0) 1]
        [else (for/fold ([pn 0])
                  ([k (in-range 1 (add1 (integer-sqrt n)))])
                (define n1 (- n (* 1/2 k (- (* 3 k) 1))))
                (define n2 (- n (* 1/2 k (+ (* 3 k) 1))))
                (define pn1 (partition-generating-modulo n1 mod))
                (define pn2 (partition-generating-modulo n2 mod))
                (cond [(odd? k) (modulo (+ pn pn1 pn2) mod)]
                      [else (modulo (- pn pn1 pn2) mod)]))]))

(define (euler78-loop) 
  (for/first ([i (in-naturals)]
              #:when (zero? (modulo (partition-generating i) 1000000)))
    (cons i (partition-generating i))))

;;(time (car (euler78-loop)))

(define (pentagonal n) 
  (* 1/2 n (- (* 3 n) 1)))

(define (generalized-pentagonal m)
  (cons (pentagonal m) (pentagonal (* m -1))))





