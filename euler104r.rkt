#lang racket
(require racket/math)
(require math/base)

(define (number-to-list n)
  (define (loop p n)
    (if (< 9 n)
        (loop (cons (modulo n 10) p) (quotient n 10))
        (cons n p)))
  (loop '() n))




(define (is-pandigital n)
  (define (is-pan-aux p i)
    (cond 
      [(= i 10) #t]
      [(not (= i (car p))) #f]
      [else (is-pan-aux (cdr p) (+ 1 i))]))
  (cond
    [(> n 987654321) #f]
    [(< n 123456789) #f]
    [else (is-pan-aux (sort (number-to-list n) <) 1)]))

(define chopchop 1000000000)

(define (log10 n)
  (/ (log n) (log 10)))

(define log10phi (log10 phi.0))
(define log10sqrt5 (log10 (sqrt 5.0)))

(define (estimate-fib n)
  (define t (- (* log10phi n) log10sqrt5))
  (exact-floor (expt 10 (+ 8 (- t (floor t))))))

(define (euler104fib3 nmax)
  (define (fib a b i)
    (cond
      [(= i nmax) i]
      [(is-pandigital b)
       (if (is-pandigital (estimate-fib i))
           i
           (fib b (modulo (+ a b) chopchop) (+ 1 i)))]
      [else (fib b (modulo (+ a b) chopchop) (+ 1 i))]))
  (fib 1 1 2))


  