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
;;limit is length of longest side
(define (gen-primitive-triples limit)
  (define max-m (integer-sqrt limit))
  ;;formula for max-n double-checked through 
  ;;wolfram alpha
  (define (max-n m) (min (/ limit (* 2 m)) m))
  (for*/list ([m (in-range 2 max-m)]
              [n (in-range 1 (max-n m))]
              #:when (= 1 (gcd m n))
              #:when (odd? (- m n)))
    (sort (list (pythag-a m n) (pythag-b m n)) <)))

(define (expand-primitives limit)
  (define prims (gen-primitive-triples (* 2 limit)))
  (apply append (for/list ([p prims])
    (define maxz (min (quotient (* 2 limit) (second p))
                      (quotient (* 1 limit) (first p))))
    (for/list ([z (in-range 1 (add1 maxz))])
      (list (* (first p) z) (* (second p) z))))))

(define (count-paths limit)
  (for*/sum ([j (in-range 1 (add1 limit))]
             [k (in-range 1 (* 2 (add1 j)))])
            (define c (sqrt (+ (* j j) (* k k))))
            (cond [(integer? c) 
                   (cond [(<= k j) (quotient k 2)]
                         [else (add1 (- j (quotient (add1 k) 2)))])]
                  [else 0])))

(define (count-paths-recursive m limit count)
  (cond [(> count limit) m]
        [else 
         (define newm (add1 m))
         (define newcount 
           (+ count 
              (for/sum ([k (in-range 1 (* 2 (add1 newm)))])
                        (define c (sqrt (+ (* newm newm) (* k k))))
                        (cond [(integer? c) 
                               (cond [(<= k newm) (quotient k 2)]
                         [else (add1 (- newm (quotient (add1 k) 2)))])]
                  [else 0]))))
         (count-paths-recursive newm limit newcount)]))
        
  





