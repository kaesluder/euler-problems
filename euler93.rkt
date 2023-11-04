#lang racket
(require racket/set)

(define (combinations k nlst)
  (cond ((zero? k)
         '(()))
        ((null? nlst)
         '())
        (else
         (append (map (lambda (k-1)
                        (cons (car nlst) k-1))
                      (combinations (- k 1) (cdr nlst)))
                 (combinations k (cdr nlst))))))

(define (perm s)
  (cond ((null? s) '())
	((null? (cdr s)) (list s))
	(else ;; extract each item in list in turn and perm the rest
	  (let splice ((l '()) (m (car s)) (r (cdr s)))
	    (append
	      (map (lambda (x) (cons m x)) (perm (append l r)))
	      (if (null? r) '()
		(splice (cons m l) (car r) (cdr r))))))))

(define (f1 numbers functions)
  (define-values (a b c d) (apply values numbers))
  (define-values (fx fy fz) (apply values functions))
  (fz (fy (fx a b) c) d))

(define (f2 numbers functions)
  (define-values (a b c d) (apply values numbers))
  (define-values (fx fy fz) (apply values functions))
  (fy 
   (fx a b)
   (fz c d)))

(define (f3 numbers functions)
  (define-values (a b c d) (apply values numbers))
  (define-values (fx fy fz) (apply values functions))
  (fz (fx a (fy b c)) d))

(define (f4 numbers functions)
  (define-values (a b c d) (apply values numbers))
  (define-values (fx fy fz) (apply values functions))
  (fx a (fz (fy b c) d)))

(define (f5 numbers functions)
  (define-values (a b c d) (apply values numbers))
  (define-values (fx fy fz) (apply values functions))
  (fx a (fy b (fz c d))))

(define (plus a b) ;;error checking
  (if (or (not a) (not b)) #f
      (+ a b)))
(define (minus a b) ;;error checking
  (if (or (not a) (not b)) #f
      (- a b)))

(define (mult a b) ;;error checking
  (if (or (not a) (not b)) #f
      (* a b)))

(define (div a b) ;;error checking
 (cond
   [(or (not a) (not b)) #f]
   [(zero? b) #f]
   [else (/ a b)]))

(define (integer-targets seq)
  (define function-list (list plus minus mult div))
  (define order-list (list f1 f2))
  (remove-duplicates
   (for*/fold
       ([results (list)])
     ([nums (perm seq)]
      [fa function-list]
      [fb function-list]
      [fc function-list]
      [order order-list])
     (define result (order nums (list fa fb fc)))
     (cond
       [(not result) results]
       [(and (positive? result) (integer? result)) (values (cons result results))]
       [else (values results)]))))


(define (count-contiguous li)
  (define (iter seq prev count max-length)
    (cond 
      [(null? seq) (max count max-length)]
      [(= 1 (- (car seq) prev)) (iter (cdr seq) (car seq) (+ 1 count) (max count max-length))]
      [else (iter (cdr seq) (car seq) 1 (max count max-length))]))
  (iter (sort li <) -inf.f 1 1))

(define (euler93)
  (define sets (combinations 4 (build-list 10 values)))
  (define (iter li maximum max-set)
    (cond [(null? li) (cons maximum (list max-set))]
          [else 
           (define cur (car li))
           (define cur-count (count-contiguous (integer-targets cur)))
           (cond 
             [(> cur-count maximum) (iter (cdr li) cur-count cur)]
             [else (iter (cdr li) maximum max-set)])]))
  (iter (cdr sets) 
        (count-contiguous (integer-targets (car sets)))
        (car sets)))

(display (time (euler93)))
(newline)




    















 