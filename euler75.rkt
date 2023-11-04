#lang racket/base
(require racket/list)
(require racket/vector)
(require racket/stream)


;;formula for producing a 
;;pythagorean triplet 
;;m > n
;;m coprime n
(define (pythag-a m n) 
  (- (* m m) (* n n)))
(define (pythag-b m n)
  (* 2 m n))
(define (pythag-c m n)
  (+ (* m m) (* n n)))

(define (pass-lesser a b)
  (if (< a b)
      (values a)
      (values b)))
  
;;generate triplets from primitives.
(define (gen-primitive-triples limit)
  (define max-m (integer-sqrt (quotient limit 2)))
  ;;formula for max-n double-checked through 
  ;;wolfram alpha
  (define (max-n m) 
    (pass-lesser m (/ (- limit (* 2 m m)) (* 2 m))))
  (for*/list ([m (in-range 2 max-m)]
              [n (in-range 1 (max-n m))]
              #:when (= 1 (gcd m n))
              #:when (odd? (- m n)))
    (sort (list (pythag-a m n) (pythag-b m n) (pythag-c m n)) <)))

;;convenience functions to create the sum
;;of a triplet, and to multiple each side
;;by a value
(define (sum li) (apply + li))
(define (times-n n li) (map (lambda (x) (* x n)) li))


;;first try to make certain that no duplicates
;;appear in the list of expanded triplets
(define (expand-primitives limit)
  (define triplet-sieve (make-hash))
  (define primitives (gen-primitive-triples limit))
  (for ([prim primitives])
    (define base-sum (sum prim))
    (for ([i (in-range 1 (+ 1 (quotient limit base-sum)))])
      ;;create a compound triplet 
      (define compound (times-n i prim))
      (define compound-sum (sum compound))
      ;;get the old list or '() as default
      (define old-list 
        (hash-ref triplet-sieve compound-sum '()))
      (hash-set! triplet-sieve compound-sum (remove-duplicates (cons compound old-list)))))
  (values triplet-sieve))

(define (count-hash-values h)
  (for/sum ([v (hash-values h)]
            #:when (= 1 (length v)))
           1))

;;expand triplets by multipling each side by 
;;i up to limit.
(define (expand-primitives2 limit)
  (define triplet-sieve (make-vector (+ 1 limit) 0))
  (define primitives (gen-primitive-triples limit))
  (for ([prim primitives])
    (define base-sum (sum prim))
    (vector-set! triplet-sieve base-sum 
                 (+ 1 (vector-ref triplet-sieve base-sum)))
    (for ([i (in-range 2 (+ 1 (quotient limit base-sum)))])
      ;;create a compound triplet 
      (define compound (times-n i prim))
      (define compound-sum (sum compound))
      (vector-set! 
       triplet-sieve 
       compound-sum 
       (+ 1 (vector-ref triplet-sieve compound-sum)))))
  (values triplet-sieve))      
  
(define (count-vector-values v)
  (vector-length (vector-filter (lambda (x) (= 1 x)) v)))

(define (expand-primitives3 limit)
  (define primitives (gen-primitive-triples limit))
  (define sums 
    (frequencies (apply append 
         (map (lambda (x) (stream->list (in-range x limit x))) 
              (map sum primitives)))))
  sums)
  
(define (frequencies lst)
  (foldl (lambda (key ht)
           (hash-update ht key add1 0))
         #hash() lst))


(define (euler75) (count-vector-values (expand-primitives2 1500000)))
(time (euler75))