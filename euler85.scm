

;;this works because (/ 1 4) returns an exact rational
;;fraction
(define (combinations x y) (* x (+ 1 x) y (+ 1 y) (/ 1 4)))

(define (iter x y target bestpair)
  (define newvalue (combinations x y))
  (define best (combinations (car bestpair) (cdr bestpair)))
  (define newbest
    ;;replace best if newvalue is closer
    (cond [(> (abs (- best target)) (abs (- newvalue target)))
           (cons x y)]
          [else bestpair]))
  ;;return results if y greater than x
  (cond [(> y x) newbest]
        ;;increment y if we're below target
        [(< newvalue target) (iter x (+ 1 y) target newbest)]
        ;;decrement x if we're above target
        [else (iter (- x 1) y target newbest)]))


(define bestpair (iter 2000 1 2000000 (cons 0 0)))

(define (e85) 
  (define bestpair (iter 2000 1 2000000 (cons 0 0)))
  (* (car bestpair) (cdr bestpair)))

 (display (e85))