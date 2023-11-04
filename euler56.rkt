#lang racket

(require (planet dherman/memoize:3:1))



(define (number-to-list n)
  (define (iter n results)
    (if (= n 0)
        results
        (iter (quotient n 10) (cons (modulo n 10) results))))
  (iter n '()))

(define (number-to-list2 n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list2 (quotient n 10)))))


(define (list-to-number numlist)
  (for/fold ([num 0])
    ([i numlist])
    (+ i (* 10 num))))

(define (sum-digits num)
  (apply + (number-to-list num)))

(define (euler56-list start limit)
  (for*/list ([i (in-range start limit)] 
               [j (in-range start limit)])
              (sum-digits (expt i j))))

(define (euler56-reduce start limit)
  (foldl max 0 (euler56-list start limit)))

(define (gen-multiples n times)
  (define (iter next counter results)
    (if (> counter times)
        results
        (iter (* next n) (+ 1 counter) (cons next results))))
  (iter n 1 '()))

(define (euler56-list2 start limit)
  (for/list ([i (in-range start limit)])
    (apply max 
           (map sum-digits 
                (gen-multiples i limit)))))

(define (euler56-reduce2 start limit)
  (apply max (euler56-list2 start limit)))

;;(displayln (euler56-reduce 100))
  