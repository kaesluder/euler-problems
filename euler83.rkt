#lang racket


(require racket/list)
(require racket/file)
(require racket/bool)
(define (load-matrix) (file->lines "matrix83.txt"))

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


(define (fake-matrix-ref v x y)
  (define dim (integer-sqrt (vector-length v)))
  (vector-ref v (+ (* dim x) y)))
  
(define (fake-matrix-set! v x y val)
  (define dim (integer-sqrt (vector-length v)))
  (vector-set! v (+ (* dim x) y) val))  

(define (get-candidates x y matrix)
  (define len (length matrix))
  (filter-not false? 
              (list (if (> x 0) (cons (sub1 x) y) #f)
                    (if (< x (sub1 len)) (cons (add1 x) y) #f)
                    (if (> y 0) (cons x (sub1 y)) #f)
                    (if (< y (sub1 len)) (cons x (add1 y)) #f))))
  


(define distance-vec (make-vector 25 0))
(define visited-vec (make-vector 25 #f))

(define (is-surrounded? x y visited)
  (define len (integer-sqrt (length visited)))
  (define cands 
    (filter-not false? 
              (list (if (> x 0) (cons (sub1 x) y) #f)
                    (if (< x (sub1 len)) (cons (add1 x) y) #f)
                    (if (> y 0) (cons x (sub1 y)) #f)
                    (if (< y (sub1 len)) (cons x (add1 y)) #f))))
  (for/and ([c cands])
    (fake-matrix-ref visited (car c) (cdr c))))

(define (is-visited? x y visited)
  (fake-matrix-ref visited x y))

(define (mark-visited x y visited)
  (fake-matrix-set! visited x y #t))

(define (get-dist x y distance-vec)
  (fake-matrix-ref distance-vec x y))
(define (set-dist! x y value distance-vec)
  (fake-matrix-set! distance-vec x y value))

(define (get-unvisited visited-vec distance-vec len)
  ;;(define len (integer-sqrt (vector-length visited-vec)))
  (filter-not false? (for/list ([i (in-range 0 (vector-length visited-vec))])
    (define x (quotient i len))
    (define y (modulo i len))
    (cond [(and (not (is-visited? x y visited-vec))
               (not (zero? (get-dist x y distance-vec))))
      (cons (cons x y) (get-dist x y distance-vec))]
          [else #f]))))

(define (find-next-possibility visited-vec distance-vec len)
         (car (argmin cdr (get-unvisited visited-vec distance-vec len))))

(define (mark-surrounding-dists x y visited distances matrix)
  (define cur (get-dist x y distances))
  (define cands (get-candidates x y matrix))
  (for ([c cands])
    (define j (car c))
    (define k (cdr c))
    (define dist1 (+ cur (matrix-ref matrix j k)))
    (define dist2 (get-dist j k distances))
    (cond [(= 0 dist2) (set-dist! j k dist1 distances)]
          [(< dist1 dist2) (set-dist! j k dist1 distances)])))

(define (dij-min-walk matrix start finish)
  (define x (car start))
  (define y (cdr start))
  (define len (length matrix))
  (define distance-vec (make-vector (* len len) 0))
  (define visited-vec (make-vector (* len len) #f))
  (define start-distance (matrix-ref matrix x y))
  (set-dist! x y start-distance distance-vec)
  (define (iter x y)
    (cond [(equal? (cons x y) finish)
           (get-dist x y distance-vec)]
          [else
           (mark-surrounding-dists x y visited-vec distance-vec matrix)
           (mark-visited x y visited-vec)
           (define next (find-next-possibility visited-vec distance-vec len))
           (iter (car next) (cdr next))]))
  (iter x y))
    
    
(time (dij-min-walk (process-matrix (load-matrix)) (cons 0 0) (cons 79 79)))  
           
    
          
      
      
    
  
  

  
             



  

	