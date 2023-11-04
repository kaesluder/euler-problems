#lang racket
(require profile)
(require (planet dherman/memoize:3:1))

(define (factorial n)
  (cond [(= 0 n) 1]
        [else (* n (factorial (- n 1)))]))

(define factorials-cache 
  (vector->immutable-vector 
   (list->vector
    (for/list ([i (in-range 0 10)]) (factorial i)))))

(define (factorial-c digit)
  (cond [(< digit 10) (vector-ref factorials-cache digit)]
        [else (factorial digit)]))

;;;this version retunrs 
;;;a reversed list of digits.
(define/memo (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))


(define (sum li) (apply + li))


(define (count-nonrepeating-factorials n)
  (define (iter n count seq) 
    (define newn (fact-sum n))
    (cond 
      [(= newn n) count]
      [(member newn seq) count]
      [else (iter newn (+ 1 count) (cons newn seq))]))
  (iter n 1 '()))

(define/memo (fact-sum n)
  (apply + (map factorial-c (number-to-list n))))

(define (comb-add li)
  (for*/list ([i li]
              [j (in-range 0 (+ (modulo i 10) 1))])
    (+ (* 10 i) j)))

(define (recursive-comb-add n li results)
  (cond [(= n 0) (remove-duplicates results)]
        [else (define newlist (comb-add li))
              (define newres (append results newlist))
              (recursive-comb-add (- n 1) newlist newres)]))

(define (gen-sequences digits)
  (define start (sequence->list (in-range 0 10)))
  (recursive-comb-add (- digits 1) start start))

             
   
(define (count-chains-matching-length n limit)
  (for/sum ([i (in-range 0 limit)]
             #:when (= n (count-nonrepeating-factorials i)))
    1))

(define (not-car-zero? li)
  (not (= 0 (car li))))

;;lifted from Roestta Code
(define (perm s)
  (cond ((null? s) '())
	((null? (cdr s)) (list s))
	(else ;; extract each item in list in turn and perm the rest
	  (let splice ((l '()) (m (car s)) (r (cdr s)))
	    (append
	      (map (lambda (x) (cons m x)) (perm (append l r)))
	      (if (null? r) '()
		(splice (cons m l) (car r) (cdr r))))))))


(define (chains-matching-length n li)
  (for/list ([i li]
             #:when (= n (count-nonrepeating-factorials i)))
    i))

(define (euler74-loop) 
  (for/sum ([i (chains-matching-length 60 (gen-sequences 6))])
           (length 
            (filter not-car-zero? 
                    (remove-duplicates (perm (number-to-list i)))))))

