(define (make-sieve limit)
  (define divisor-sieve (make-vector (+ 1 limit)))
  (vector-fill! divisor-sieve 1)
  (define (iter1 i)
	(cond [(> i limit) divisor-sieve] ;;exit condition
		  [else (iter2 i 2) (iter1 (+ 1 i))]))
  (define (iter2 i j)
	(cond [(> j (quotient limit i)) ;;exit condition
		   divisor-sieve]
		  [else 
		   (let ((prev (vector-ref divisor-sieve (* i j))))
			  (vector-set! divisor-sieve (* i j) (+ i prev))
			  (iter2 i (+ 1 j)))]))
  (iter1 2))

(define (member-of? target li)
  (cond [(null? li) #f]
        [(= target (car li)) #t]
        [else (member-of? target (cdr li))]))

(define (count-breadcrumbs li target)
  (let iter ((li li)
			 (count 1))
	(if (null? li) #f
		(if (= target (car li)) count
			(iter (cdr li) (+ 1 count))))))

(define (follow-seq number limit vec)
  (define (iter this breadcrumbs)
	(cond 
	 [(= this number) breadcrumbs]
	 [(= this 1) #f]
	 [else
		(cond
		 [(member-of? this breadcrumbs) #f] ;;not amicable but still loops
		 [(> this limit) #f] ;;out of bounds
		 [else (iter (vector-ref vec this) (cons this breadcrumbs))])]))
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

(display (euler95))
(newline)

    

    
    