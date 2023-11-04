#lang racket/base
(require racket/list)
(require racket/file)
(require racket/math)

(define (roman-char->int char)
  (case char
    [(#\I) 1]
    [(#\V) 5]
    [(#\X) 10]
    [(#\L) 50]
    [(#\C) 100]
    [(#\D) 500]
    [(#\M) 1000]))

(define (recursive-parse-roman prev li)
  (cond [(null? li) prev]
        [else 
         (define current (roman-char->int (car li)))
         (cond [(< prev current) (recursive-parse-roman (- current prev) (cdr li))]
               [else (+ prev (recursive-parse-roman current (cdr li)))])]))

(define (parse-roman-string str)
  (recursive-parse-roman 0 (string->list str)))

(define (roman-nums-for-multiplier m)
  (case m
    [(0) '(#\I #\V #\X)]
    [(1) '(#\X #\L #\C)]
    [(2) '(#\C #\D #\M)]))

(define (digit->number multiplier digit)
  (define rs (roman-nums-for-multiplier multiplier))
  (define one (first rs))
  (define five (second rs))
  (define ten (third rs))
  (cond [(= digit 0) '()]
        [(< digit 4) (build-list digit (lambda (x) one))]
        [(= digit 4) (list one five)]
        [(< digit 9) (append (list five) (build-list (- digit 5) (lambda (x) one)))]
        [(= digit 9) (list one ten)]))

(define (romans-to-thousand int)  
  (cond [(= int 0) '()]
        [else 
         (define m (order-of-magnitude int))
         (define x (quotient int (expt 10 m)))
              (append (digit->number m x) (romans-to-thousand (modulo int (expt 10 m))))]))

(define (int->roman int)
  (cond [(= int 0) '()]
        [else 
         (define ts (quotient int 1000))
         (append (build-list ts (lambda (x) #\M))
                 (romans-to-thousand (modulo int 1000)))]))

(define (load-romans) (file->lines "roman.txt"))

(define (e89-diff) 
  (for/sum ([r (load-romans)])
    (define l1 (string-length r))
    (define l2 (length (int->roman (parse-roman-string r))))
    (- l1 l2)))

(time (e89-diff))
         








  



