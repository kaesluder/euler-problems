#lang racket
(define (cube x) (* x x x))

;;;this version retunrs 
;;;a reversed list of digits.
(define (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))

;;;use a sorted list of digits a key to a 
;;;hash structure.
(define (sort-key x) (sort (number-to-list x) <))

(define (find-perms n limit)
  ;;;set up variables including a blank immutable hash
  (let ([list (map cube (stream->list (in-range 1 limit)))]
        [results (hash)])
    ;;;inner recursion function
    ;;;takes the list of cubes and a 
    ;;;hash accumulator.
    (define (iter li r)
      (if (empty? li)
          ;;;if the list is exhausted return 
          ;;;just the values
          (hash-values r)
          
          ;;;cons our value onto the appropriate 
          ;;;slot in our hash table. Then 
          ;;;Recusively call iter with a shortened list 
          ;;;and the new hash table.
          (let* ([x (car li)]
                 [key (sort-key x)]
                 [prev (hash-ref r key '())]
                 [new-result (hash-set r key (cons x prev))])
                (iter (cdr li) new-result))))
    ;;;filter the results for sequences of length n.
    (filter (lambda (x) (= n (length x))) (iter list results))))

(define (e62) (last (argmin last (find-perms 5 10000))))
