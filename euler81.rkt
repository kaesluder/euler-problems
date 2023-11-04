#lang racket/base
(require racket/list)
(require racket/file)
(define (load-matrix) (file->lines "matrix.txt"))

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

(define (e81) (find-min-diag (process-matrix (load-matrix))))
(time (e81))












