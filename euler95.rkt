#lang racket/base
(require profile)
(require racket/list)

(define (make-sieve limit)
  (define divisor-sieve (make-vector (+ 1 limit)))
  (vector-fill! divisor-sieve 1)
  (for* ([i (in-range 2 (+ 1 limit))]
         [j (in-range 2 (+ 1 (quotient limit i)))])
    (define prev (vector-ref divisor-sieve (* i j)))
    (vector-set! divisor-sieve (* i j) (+ i prev)))
  divisor-sieve)

(define (member-of? target li)
  (cond [(null? li) #f]
        [(= target (car li)) #t]
        [else (member-of? target (cdr li))]))
      
      
(define (follow-seq number limit vec)
  (define (iter this breadcrumbs)
    (cond
      [(= this number) (values breadcrumbs)] ;;easy test for amicable pair
      [(member-of? this breadcrumbs) #f] ;;not amicable but still loops
      [(> this limit) #f] ;;out of bounds
      [else (iter (vector-ref vec this) (cons this breadcrumbs))]))
  (iter (vector-ref vec number) (list number)))

(define (find-longest-seq limit)
  (define divisor-sieve (make-sieve limit))
  (define (iter i best best-length)
    (cond 
      [(> i limit) best]
      [else
       (define this (follow-seq i limit divisor-sieve))
       (cond 
         [(not (list? this)) ;;not an amicable chain
          (iter (+ 1 i) best best-length)]
         [(> (length this) best-length) ;;longer than current best
          (iter (+ 1 i) this (length this))]
         [else (iter (+ 1 i) best best-length)])]))
  (iter 1 (list) 0))


(define (euler95)
  (apply min (find-longest-seq 1000000)))

(display (time (euler95)))
(newline)

    

    
    