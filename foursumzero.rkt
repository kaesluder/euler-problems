#lang racket/base
(require racket/list racket/sequence)

(define (negatives li)
  (sort (filter (lambda (x) (< x 0)) li) <))

(define (positives li)
  (sort (filter (lambda (x) (> x 0)) li) >))

(define (has-zero? li)
  (cond
    [(empty? li) #f]
    [(= 0 (car li)) #t]
    [else (has-zero? (cdr li))]))

(define (find-match-inner target num li)
  (cond
    [(empty? li) #f]
    [(= target (+ (car li) num)) (list (car li) num)]
    [else (find-match-inner target num (cdr li))]))

(define (find-match target li)
  (for/first ([i li])
    (find-match-inner target i li)))

(define (closer-to-zero cur li)
  (define sum (abs (apply + cur))) ;;calculate the sum
  (filter (lambda (x) (>= sum (abs (+ sum x)))) li))

(define (add-one root li)
  (for/list ([i (closer-to-zero root li)])
    (cons i root)))


(define (pairs li) ;;create a hash from the list
  (define my-pairs (make-hash))
  (for* ([i li]
         [j li])
    (hash-set! my-pairs (+ i j) (list i j)))
  my-pairs) 
    
  

(define (compare-pairs li)
  (define my-pairs (pairs li))
  (define sums-list (hash-keys my-pairs))
  (for*/or ([i sums-list])
    (cond
      [(hash-has-key? my-pairs (* i -1))
       (append (hash-ref my-pairs i) (hash-ref my-pairs (* i -1)))]
      [else #f])))

(define (compare-pairs2 li)
  (define my-pairs (make-hash))
  (for*/or ([i li]
            [j li])
    (let ((sum (+ i j)))
      (cond 
        [(hash-has-key? my-pairs (* sum -1))
         (append (list i j) (hash-ref my-pairs (* sum -1)))]
        [(not (hash-has-key? my-pairs sum)) 
         (begin (hash-set! my-pairs sum (list i j))
                #f)]
        [else #f]))))

    

(time (compare-pairs2 (sequence->list (in-range -50 50))))


















