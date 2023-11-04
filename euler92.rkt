#lang racket
(require profile)

;;;this version retunrs 
;;;a reversed list of digits.
(define (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))


(require srfi/1 srfi/26)
(define (digits->list num (base 10))
  (unfold-right zero? (cut remainder <> base) (cut quotient <> base) num))

(define (next-in-chain n)
  (apply + (map (lambda (x) (* x x)) (digits->list n))))

(define (end-of-chain n)
  (cond 
    [(or (= n 1) (= n 89)) n]
    [else (end-of-chain (next-in-chain n))]))

(define (gen-square-sieve limit)
  (define sieve (make-vector (add1 limit) 0))
  (for ([i (in-range 1 (add1 limit))])
    (vector-set! sieve i (end-of-chain i)))
  sieve)

(define (end-chain-with-vector n vec vec-limit)
  (cond 
    [(> n vec-limit) (end-chain-with-vector (next-in-chain n) vec vec-limit)]
    [else (vector-ref vec n)]))

(define (count-89s limit vec-limit)
  (define vec (gen-square-sieve vec-limit))
  (define (iter i count)
    (cond 
      [(> i limit) count] 
      [(= 89 (end-chain-with-vector i vec vec-limit)) (iter (add1 i) (add1 count))]
      [else (iter (add1 i) count)]))
  (iter 1 0))


(define (alternate-count limit)
  (length (filter (lambda (x) (= x 89)) (vector->list (gen-square-sieve limit)))))
