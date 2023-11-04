#lang racket/base
(require racket/list)

(define (continued-fraction s)
  (define m 0)
  (define d 1)
  (define a (floor (sqrt s)))
  (define (iter a m d results)
    (define m1 (- (* d a) m))
    (define d1 (/ (- s (* m1 m1)) d))
    (define a1 (floor (/ (+ (sqrt s) m1) d1)))
    (if (member (list a1 m1 d1) results)
        results
        (iter a1 m1 d1 (cons (list a1 m1 d1) results))))
  (iter a m d '()))

(define (is-square? s)
  (define sr (sqrt s))
  (= sr (floor sr)))

(define (repeat-count maximum)
  (for/list ([n (in-range 2 maximum)]
             #:when (not (is-square? n)))
    (cons n (length (continued-fraction n)))))

(define (euler64)
  (define (cdr-odd? x) (odd? (cdr x)))
  (count cdr-odd? (repeat-count 10000)))


(time (euler64))
    
    