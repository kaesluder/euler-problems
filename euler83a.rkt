#lang racket/base
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

(define (get-candidates x y matrix)
  (define len (length matrix))
  (filter-not false? 
              (list (if (> x 0) (cons (sub1 x) y) #f)
                    (if (< x (sub1 len)) (cons (add1 x) y) #f)
                    (if (> y 0) (cons x (sub1 y)) #f)
                    (if (< y (sub1 len)) (cons x (add1 y)) #f))))

(define (is-visited? x y visited)
  (hash-ref visited (cons x y) #f))

(define (mark-visited x y visited)
  (hash-set visited (cons x y) #t))

;;use infinity as the default value.
(define (get-dist x y distances)
  (hash-ref distances (cons x y) +inf.f))

(define (set-dist x y value distances)
  (hash-set distances (cons x y) value))

(define (mark-surrounding x y visited distances matrix)
  (define cur (get-dist x y distances))
  (define cands (get-candidates x y matrix))
  (for/fold ([results distances])
    ([c cands])
    (define j (car c))
    (define k (cdr c))
    (define dist1 (+ cur (matrix-ref matrix j k)))
    (define dist2 (get-dist j k distances))
    (cond [(and (< dist1 dist2)
                (not (is-visited? j k visited)))
           (set-dist j k dist1 results)]
          [else results])))

(define (drop-visited visited distances)
  (for/fold ([results distances])
    ([key (hash-keys distances)])
    (cond [(is-visited? (car key) (cdr key) visited)
           (hash-remove results key)]
          [else results])))

(define (get-next distances)
  (car (argmin cdr (hash->list distances))))

(define (dij-walk start finish matrix)
  (define start-dist (matrix-ref matrix (car start) (cdr start)))
  (define start-dist-hash (hash start start-dist))
  (define (iter cell visited distances)
    (cond [(equal? cell finish)
           (get-dist (car cell) (cdr cell) distances)]
          [else 
           (define x (car cell))
           (define y (cdr cell))
           (define new-visited (mark-visited x y visited))
           (define new-distances 
             (drop-visited visited 
                           (mark-surrounding x y visited distances matrix)))
           (define next (get-next new-distances))
           (iter next new-visited new-distances)]))
  (iter start (hash) start-dist-hash))
             

(define (euler83) (dij-walk (cons 0 0) (cons 79 79) (process-matrix (load-matrix))))
(time (euler83))


