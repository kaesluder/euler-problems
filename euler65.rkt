#lang racket


;;;this version retunrs 
;;;a reversed list of digits.
(define (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))


;;each iteration adds (1 2i 1) to the list
(define (e-continued-fraction iterations)
  (for/fold 
      ([result '()]) 
      ([i (in-range 1 (+ 1 iterations))])
    (append result (list 1 (* 2 i) 1)))) 
    
;;generalization of function used for euler66.
;;creates the convergents from a list representing 
;;an extended fraction. 
(define (convergents-with-seq start seq)
  (define list-sequence seq)
  (define sr start)
  (define (iter results ls)
    (if (empty? ls)
        results
        (let* ([a (car ls)]
               [res1 (first results)]
               [res2 (second results)]
               [newn (+ (car res2) (* (car res1) a))]
               [newd (+ (cdr res2) (* (cdr res1) a))]
               [newres (cons (cons newn newd) results)])
          (iter newres (cdr ls)))))
  (cdr (reverse (iter (list (cons sr 1) (cons 1 0)) list-sequence))))

(define (e65-num) 
  (car (last (take 
              (convergents-with-seq 2 (e-continued-fraction 34)) 100))))

(define (e65) 
  (apply + (number-to-list (e65-num))))




