(use srfi-1)

;;convenience functions for 
;;sum and product
(define (product li)
  (let loop ((prod 1)
			 (li li))
	(if (null? li) prod
		(loop (* prod (car li)) (cdr li)))))


(define (sum li)
   (let loop ((prod 0)
			 (li li))
	(if (null? li) prod
		(loop (+ prod (car li)) (cdr li)))))

(define (add-digits-item item limit)
  (define (max-n item) 
    (add1 
     (min (car item) 
          (quotient limit (product item)))))
  (list-tabulate (- (max-n item) 2) 
				 (lambda (x) (cons (+ 2 x) item))))

(define (add-digits-list ls limit)
  (apply append 
		 (map (lambda (x) (add-digits-item x limit)) ls)))

(define (add-digits-times ls n limit)
  (let loop ((results (add-digits-list ls limit))
			 (i 1))
	(cond ((= i n) results)
		  (else (loop 
				 (add-digits-list results limit)
				 (add1 i))))))


(define (build-lists limit)
  (define max-factors (inexact->exact (floor (/ (log limit) (log 2)))))
  (define start-list (list-tabulate (- (add1 limit) 2) (lambda (x) (list (+ 2 x)))))
  (let loop ((results (list))
			 (i 1))
	(if (= i max-factors) results
		(loop (append results (add-digits-times start-list i limit)) (add1 i)))))

(define (get-slot factor-list)
  (+ (length factor-list) (- (product factor-list) (sum factor-list))))

(define (fill-vector limit)
  (define vec (make-vector (add1 limit) +inf.0))
  (vector-set! vec 0 0)
  (vector-set! vec 1 0)
  (define lists (build-lists (* 2 limit)))
  (do ((lists lists (cdr lists)))
	   ((not (pair? lists)) vec)
	(let ((slot (get-slot (car lists)))
		  (prod (product (car lists))))
	  (when (<= slot limit)
			(when (< prod (vector-ref vec slot))
				  (vector-set! vec slot prod))))))

(define (sum-vector vec)
  (fold + 0 (delete-duplicates (vector->list vec))))

(define (e88) (sum-vector (fill-vector 1000)))

(display (e88))


		
		
  



 
