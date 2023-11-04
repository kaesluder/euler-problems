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

(define (combinations-as-sets k list)
  (map list->set (combinations k list)))

(define (disjoint? a b)
  (set-empty? (set-intersect (set 1 2) (set 3 4))))

(define (set-sum a)
  (apply + (set->list a)))

(define (test-sets-rule1 a b)
  ;;check to see if it satisfies rule 1
  ;;S(B) != S(A) and disjoint sets
  (cond
    [(disjoint? a b) 
     (not (= (set-sum a) (set-sum b)))]
    [else #t]))

(define (test-all-subsets-rule1? li)
  (for/and ([i (in-range 2 (+ 1 (quotient (length li) 2)))])
    (test-list test-sets-rule1 (combinations-as-sets i li))))

(define (test-list fn li)
  ;;recursively test all members of a list against each other
  (define (loop head tail)
    (cond
      [(empty? tail) #t]
      [else 
       (define tested
         (for/and ([x tail])
           (fn head x)))
       (cond
         ;; loop if test returns true for all 
         ;; members of tail
         [tested (loop (car tail) (cdr tail))]
         ;;exit if false
         [else #f])]))
  (loop (car li) (cdr li)))

(define (set-of-n? n li)
  (= n (set-count (list->set li))))

(define (test-rule2? li)
  ;;define the limit on sets
  ;;as 1/2 length rounding up.
  (define limit
    (cond 
      [(odd? (length li))
       (+ 1 (quotient (length li) 2))]
      [else
       (quotient (length li) 2)]))
  (define sorted (sort li <))
  (define (sum l)
    (apply + l))
  (for/and ([i (in-range 2 (+ 1 limit))])
    (< (sum (take-right sorted (- i 1)))
       (sum (take sorted i)))))

(define near-optimum-test-case6 '(11 17 20 22 23 24))
(define optimum-test-case6 '(11 18 19 20 22 25))

(define (apply-tests fnli li)
  ;;apply tests in order of efficiency
  (for/and ([fn fnli])
    (fn li)))

(define (test-list6 near-optimum range)
  (define r (in-range (* -1 range) range))
  ;;currying the size test.
  (define (size-test? li) (set-of-n? 6 li))
  (for*/fold 
      ([output near-optimum])
    ([a r]
     [b r]
     [c r]
     [d r]
     [e r]
     [f r])
    (define mod (list a b c d e f))
    (define candidate (map + near-optimum mod))
    (test-min output candidate)))

(define (test-list7 near-optimum range)
  (define r (in-range (* -1 range) range))
  ;;currying the size test.
  (define (size-test? li) (set-of-n? 6 li))
  (for*/fold 
      ([output near-optimum])
    ([a r]
     [b r]
     [c r]
     [d r]
     [e r]
     [f r]
     [g r])
    (define mod (list a b c d e f g))
    (define candidate (map + near-optimum mod))
    (test-min output candidate)))


(define (test-min old new)
   (define (size-test? li) (set-of-n? (length li) li))
   (cond
     ;;cheap minimum test
      [(< (apply + old) (apply + new))
       old]
      [else
       (cond
         ;;apply tests in increasing complexity
         [(apply-tests (list
                        size-test?
                        test-rule2? 
                        test-all-subsets-rule1?) new)
          new]
         [else
          old])]))

(define (generate-new-near-op old)
  (define b (list-ref old (/ (length old) 2)))
  (define (+b a) (+ b a))
  (cons b (map +b old)))

(define (euler103)
  (apply string-append (map number->string (sort (test-list7 (generate-new-near-op optimum-test-case6) 4) <))))

(time (euler103))

  
