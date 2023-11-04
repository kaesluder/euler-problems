#lang racket
(require "primes.rkt")


;;(define (primes) (sieve 1000))

(define (sieve-as-list n) (vector->list (sieve n)))

(define (take-while proc seq)
  (define (iter proc seq2 results)
    (cond [(empty? seq2) (reverse results)]
          [(not (proc (car seq2))) (reverse results)]
          [else (iter proc (cdr seq2) (cons (car seq2) results))]))
  (iter proc seq '()))

(define (divides? p q)
  (zero? (modulo p q)))

(define (prime-factors n primelist)
  (define (iter n ps results)
    (cond
      [(empty? ps) (cons n results)]
      [(< n (* (car ps) (car ps))) (cons n results)]
      [(divides? n (car ps)) 
       (iter (quotient n (car ps)) primelist (cons (car ps) results))]
      [else (iter n (cdr ps) results)]))
  (iter n primelist '()))

(define (multiply-primes-to limit)
  (define primes (sieve-as-list 100))
  (define (iter ps j)
    (define k (* (car ps) j))
    (cond [(> k limit) j]
          [else (iter (cdr ps) k)]))
  (iter primes 1))

;;;this version retunrs 
;;;a reversed list of digits.
(define (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))


(define (totient n divisors)
  (define divs 
    (for/product ([d divisors])
                 (- 1 (/ 1 d))))
  (* n divs))


(define (euler70-test) 
  (for/list ([i (sieve-as-list (integer-sqrt (expt 10 7)))]
             #:when (is-palendrome? (* i i) (- (* i i) 1)))
    (* i i)))


;;this can probably be optimized further. 
(define (is-palendrome? a b)
  (define ax (sort (number-to-list a) <))
  (define bx (sort (number-to-list b) <))
  (equal? ax bx))

;;convenience function
;;returns true if product and totient 
;;are palendromes
(define (builds-totient-palendrome? divisors)
  (define prod (apply * divisors))
  (define tot (totient prod divisors))
  (is-palendrome? prod tot))


(define (euler70-test2)
  (define maximum (expt 10 7))
  (define sqrt-limit (integer-sqrt maximum))
  ;;test primes close to the square root of 
  ;;our maximum: 0.5*sqrt(max) < p < 2*sqrt(max)
  (define limit (* 2 sqrt-limit))
  (define primes (filter (lambda (x) (> x (/ sqrt-limit 2))) (sieve-as-list limit)))
  ;; a quick helper to make the lambda more readable
  ;; for determining the maximum.
  (define (helper x y) (< x (/ maximum y)))
  (for*/list ([i primes]
              [j (take-while (lambda (x) (helper x i)) primes)]
              #:when (< j i)
              ;; a nice and cheap optimization
              #:when (= 1 (modulo (+ i j) 9))
              #:when (builds-totient-palendrome? (list i j)))
    (define prod (* i j))
    (define tot (totient prod (list i j)))
    (list prod tot (exact->inexact (/ prod tot)))))

(define (euler70) (argmin last (euler70-test2)))




