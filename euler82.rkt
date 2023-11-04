#lang racket/base
(require racket/list)
(require racket/file)
(require racket/bool)
(define (load-matrix) (file->lines "matrix2.txt"))

(define (process-matrix m)
  (for/list ([line m])
    (map string->number (regexp-split #rx"," line))))

(define test-matrix (list
(list 131	673	234	103	18)
(list 201	96	342	965	150)
(list 630	803	746	422	111)
(list 537	699	497	121	956)
(list 805	732	524	37	331)))

(define (matrix-ref m row column) 
  (list-ref (list-ref m row) column))

(define (matrix-column m c)
  (for/list ([i (in-range 0 (length m))])
    (matrix-ref m i c)))

(define (matrix-diagonals1 m)
  (for/list ([x (in-range 0 (length (list-ref m 0)))])
    (for/list ([y (in-range 0 (add1 x))])
      (matrix-ref m (- x y) y))))

(define (matrix-diagonals2 m)
  (define limit (length (list-ref m 0)))
  (for/list ([x (in-range limit (sub1 (+ limit limit)))])
    (for/list ([y (in-range (add1 (- x limit)) limit)])
      (matrix-ref m (- x y) y))))

(define (matrix-diagonals m)
  (append (matrix-diagonals1 m) (matrix-diagonals2 m)))

(define (longer list1 list2)
  (cond [(> (length list1) (length list2)) list1]
        [else list2]))

(define (shorter list1 list2)
  (cond [(< (length list1) (length list2)) list1]
        [else list2]))

(define (sum-reduce l s)
  (define sum1 (map + s (drop-right l 1)))
  (define sum2 (map + s (cdr l)))
  (map min sum1 sum2))

(define (sum-build lista listb)
  (for/list ([b listb]
             [x (cons (car lista) lista)]
             [y (append lista (list (last lista)))])
    (+ b (min x y))))


(define (find-min-diag m)
  (define diagonals (matrix-diagonals m))
  (for/fold ([results (car diagonals)])
    ([item (cdr diagonals)])
    (cond [(< (length results) (length item))
           (sum-build results item)]
          [else
           (sum-reduce results item)])))


(define (sums-for-column prev-sums c)
  (for/list ([i (in-range 0 (length c))])
    (define curr (list-ref c i))
    (define adjacents (filter-not false?  
      (list 
       (+ curr (list-ref prev-sums i))
       (cond 
         [(> i 0) 
          (+ curr (list-ref prev-sums (sub1 i)) (list-ref c (sub1 i)))]
         [else #f]) 
       
       (cond [(< i (sub1 (length c)))
              (+ curr (list-ref prev-sums (add1 i)) (list-ref c (add1 i)))]
             [else #f]))))
    (apply min adjacents)))

(define (sum-slice li x y)
  (for/sum ([i (in-range x y)])
           (list-ref li i)))

(define (sums-for-column2 prev-sums c)
  (for/list ([i (in-range 0 (length c))])
    (define curr (list-ref c i))
    (define adjacents
      (append
       ;;get the one to the left
       (list (+ curr (list-ref prev-sums i)))
       ;;get all the ones up and left
       (for/list ([j (in-range 0 i)])
         (+ curr (list-ref prev-sums j) (sum-slice c j i)))
       (for/list ([k (in-range (add1 i) (length c))])
         (+ curr (list-ref prev-sums k) (sum-slice c (add1 i) (add1 k))))))                   
    (apply min adjacents)))

(define (walk-matrix m)
  (define len (length (list-ref m 0)))
  (for/fold ([results (matrix-column m 0)])
    ([i (in-range 1 len)])
    (sums-for-column2 results (matrix-column m i))))

(define (e82) (apply min (walk-matrix (process-matrix (load-matrix)))))
(time (e82))







  

    