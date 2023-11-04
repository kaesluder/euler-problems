#lang racket

(require "primes.rkt")

;;convenience functions for 
;;sum and product
(define (product li)
  (apply * li))
(define (sum li)
  (apply + li))

(define (test-factors k factor-list)
  (define a (- k (length factor-list)))
  (define p (apply * factor-list))
  (define sum (apply + factor-list))
  (cond [(negative? a) #f]
        [(= a (- p sum)) #t]
        [else #f]))

;;all of this until the bottom is too slow
;;for large numbers.
(define (add-digits k x factor-list)
  (define p (product factor-list))
  (for/list ([i (in-range 2 (add1 (min x (quotient (* 2 k) p))))])
    (cons i factor-list)))

(define (good-cand? k factor-list)
  (define p (product factor-list))
  (cond [(> p k) #t]
        [else #f]))

(define (build-list k candidates)
  (cond [(andmap (lambda (x) (good-cand? k x)) candidates) candidates]
        [else (define good (filter (lambda (x) (good-cand? k x)) candidates))
              (define bad (filter (lambda (x) (not (good-cand? k x))) candidates))
              (define new (for/fold ([results (list)])
                            ([b bad])
                            (append results (add-digits k (car b) b))))
              (build-list k (append new good))]))


(define (sort-by-product x y) (< (product x) (product y)))

(define (create-candidates k)
          (filter (lambda (x) (test-factors k x ))
                  (build-list k (map list (sequence->list (in-range 2 (* k 2)))))))

;;slow
(define (find-min-product-sum k)
  (define cands (create-candidates k))
  (product (argmin product cands)))
  
;;slow algorithm but finds 
;;the minimum of the entire set
(define (find-sums-slow limit)
  (define results (make-hash))
  (for ([k (in-range 2 (add1 limit))])
    (hash-set! results (find-min-product-sum k) 1))
  (hash-keys results))

(define (find-sums limit)
  (define results (make-hash))
  (for ([k (in-range 2 (add1 limit))])
    (hash-set! results (first-candidate k) 1))
  (hash-keys results))

(define (first-candidate k)
  (for*/first ([outer (in-range 2 (add1 (* k 2)))]
               [inner (build-list k (list (list outer)))]
               #:when (test-factors k inner))
    inner))

(define (seek-best k)
  (for/fold ([best (list (add1 (* k 2)))])
            ([i (in-range 2 (* k 2))]
             #:when (< i (product best)))
            (define cands 
              (filter (lambda (x) (test-factors k x )) 
                      (build-list-best k best (list (list i)))))
            (cond [(empty? cands) best]
                  [else (argmin product (cons best cands))])))



(define (build-list-best k best candidates)
  (cond [(andmap (lambda (x) (good-cand? k x)) candidates) candidates]
        [else (define good (filter (lambda (x) (good-cand? k x)) candidates))
              (define bad (filter (lambda (x) (not (good-cand? k x))) candidates))
              (define new (for/fold ([results (list)])
                            ([b bad])
                            (append results (add-digits-best (product best) (car b) b))))
              (build-list-best k best (append new good))]))

(define (add-digits-best best x factor-list)
  (define p (product factor-list))
  (for/list ([i (in-range 2 (add1 (min x (quotient best p))))])
    (cons i factor-list)))

(define (find-sums-seek k)
  (define results (make-hash))
  (for ([i (in-range 2 (add1 k))])
    (define best (product (seek-best i)))
    (hash-set! results best best))
 (sum (hash-keys results)))


;;start of final solution
;;everything above this is too slow
;;above a few hundred.

;;extend a list by one factor
(define (recursive-add-digits li limit)
  (define (max-n item) 
    (add1 
     (min (car item) 
          (quotient limit (product item)))))
  (for*/list ([item li]
              [n (in-range 2 (max-n item))])
    (cons n item)))

;;builds the list of factorial candidates
(define (build-lists limit)
  (define max-digits (inexact->exact (floor (/ (log limit) (log 2)))))
  (define start-list (map list (sequence->list (in-range 2 (add1 limit)))))
  (for/fold ([results (list)])
    ([n (in-range 1 max-digits)])
    (append results
            ;;increase up to the appropriate number of digits
            (for/fold ([subseq start-list]) 
              ([m (in-range 1 (add1 n))])
              (recursive-add-digits subseq limit)))))


;;fit all the candidate factors onto
;;the list, choosing the minimum
;;if there's a conflict
(define (fit-slots li limit)
  (define slot-vector (make-vector (add1 limit)))
  (for ([i (in-range 2 (add1 limit))])
    ;;use +inf.f (positive infinity)
    ;;as a default value for k>= 2
    ;;k0 and k1 are set to 0
    (vector-set! slot-vector i +inf.f))
  (for ([item li])
    (define p (product item))
    (define s (sum item))
    (define place (+ (length item) (- p s)))
    (when (<= place limit)
      (when (< p (vector-ref slot-vector place))
        (vector-set! slot-vector place p))))
  slot-vector)

;;build a vector containing the min-value sums 
;;up to the limit. 
(define (build-vector limit)
  (define cands (build-lists (* 2 limit)))
  (fit-slots cands limit))

;;remove duplicates and sum over the vector
(define (sum-vector vec)
  (sum (remove-duplicates (vector->list vec))))

(define (e88) (sum-vector (build-vector 12000)))
;;(time (e88))      
    
  
  
  


    
               
  



