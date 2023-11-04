(use srfi-69)
;;(#%require srfi/69)

(define test-list '(2 3 1 0 -4 -1))

(define (build-range start finish)
  (define (iter results cur)
	(if (> cur finish) results
		(iter (cons cur results) (+ 1 cur))))
  (iter (list) start))

(define test-list2 (build-range -50 50))

(define test-list3 '(-2 -3 -1 -4))

(define (pairs li)
  (define (pairs-loop-inner i results li)
	(if (null? li) results
		(begin (hash-table-set! results (+ i (car li)) (list i (car li)))
			   (pairs-loop-inner i results (cdr li)))))

  (define (pairs-loop-outer results lj li)
	(if (null? lj) results
		(pairs-loop-outer (pairs-loop-inner (car lj)
											results
											li)
						  (cdr lj)
						  li)))
  (pairs-loop-outer (make-hash-table) li li))

(define (compare-pairs li)
  (define sums-hash (pairs li))
  (define sums-list (hash-table-keys sums-hash))
  (define (iter sums)
	(cond 
	 ((null? sums) #f)
	 (else 
	  (letrec ((cur (car sums))
			   (inverse (* -1 cur)))
		(if (hash-table-ref/default sums-hash 
									inverse
									#f)
			(append (hash-table-ref sums-hash inverse)
					(hash-table-ref sums-hash cur))
			(iter (cdr sums)))))))
  (iter sums-list))

(define (pairs2 li)
  (let ((results (make-hash-table)))
	(call/cc 
	 (lambda (return)
	   (for-each 
		(lambda (x)
		  (for-each 
		   (lambda (y) 
			 (let ((t (test2 results x y)))
			   (if t 
				   (return t)
				   (hash-table-set! results (+ x y) (list x y)))))
		   li))
		li) #f))
	))

(define (test2 sums-hash x y)
  (letrec ((cur (+ x y))
		   (inverse (* -1 cur)))
	(cond 
	 [(hash-table-ref/default sums-hash
							 inverse
							 #f)
		(append (hash-table-ref sums-hash inverse) (list x y))]
	 [(zero? (+ x y)) (list x x y y)]
	 [else #f])))


(display (time (pairs2 test-list2)))
(newline)
 

  
						