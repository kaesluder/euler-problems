#lang racket/base


(define test-pairs 
  '((0 . 1) (0 . 4) (0 . 9) (1 . 6) (2 . 5) (3 . 6) (4 . 9) (6 . 4) (8 . 1))) 


;;not tail recursive, and probably
;;would blow up for large values of n
(define (combinations n prev seed)
  (cond [(zero? n) prev]
        [else
         (for*/fold ([results '()])
           ([p prev]
            [s seed])
           (append results (combinations (sub1 n) (list (cons s p)) (filter (lambda (x) (< s x)) seed))))]))


;;builds unique combinations 
;;of n items from list li
(define (build-combinations n li)
  (for/fold ([results '()])
    ([x li])
    (append results (combinations (sub1 n) (list (list x)) (filter (lambda (y) (< y x)) li)))))

;;test a combo with an individual pair
(define (test-combo-with-pair x y pair)
  (define a (car pair))
  (define b (cdr pair))
  ;;use list? to only return a boolean
  (list? (or (and (member a x) (member b y))
      (and (member b x) (member a y)))))

;;since 6/9 can be turned upside down
;;add 6 to all sets containing 9
;;and add 9 to all sets containing 6
(define (add-six-nine li)
  (cond [(member 6 li) (cons 9 li)]
        [(member 9 li) (cons 6 li)]
        [else li]))

;;test combo with all pairs, taking into account 
;;6/9 reversal
(define (test-combo-with-pairs x y pairs)
  (define modx (add-six-nine x))
  (define mody (add-six-nine y))
  (for/and ([p pairs])
    (test-combo-with-pair modx mody p)))

;;test all of the combos.
(define (test-all-combos)
  ;;we know one dice must have a 0. 
  ;;this cuts the list down by %40
  (define list1 (filter (lambda (x) (member 0 x)) (build-combinations 6 (build-list 10 values))))
  (define list2 (build-combinations 6 (build-list 10 values)))
  (for*/list
      ([x list1]
       [y list2]
       #:unless (equal? x y)
       #:when (test-combo-with-pairs x y test-pairs))
    (cons x (list y))))


;;filter combos to find unique combos
;;eliminting cases where the dice
;;are swapped
(define (count-unique-combos)
  (define combo-list (test-all-combos))
  (hash-count 
   (for/fold ([h (hash)])
    ([c combo-list])
    (cond [(hash-has-key? h (reverse c)) h]
          [else (hash-set h c 1)]))))


(time (count-unique-combos))


