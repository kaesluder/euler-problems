#lang racket/base
(require racket/file racket/list racket/set racket/vector)

(define (load-puzzles) 
  (file->lines "sudoku.txt"))

(define (char->number c)
  (- (char->integer c) 48))

(define (separate-puzzles lines)
  (define (iter lines results)
    (cond 
      [(null? lines)
       (reverse results)]
      [else 
       (iter (drop lines 10) (cons (drop (take lines 10) 1) results))]))
  (iter lines (list)))

(define (parse-puzzle puz)
  (define (parse-line line)
    (map char->number (string->list line)))
  (map parse-line puz))

(define (make-puzzle-vector text-puz)
  (list->vector (apply append (parse-puzzle text-puz))))

(define (make-all-puzzle-vectors)
  (map make-puzzle-vector (separate-puzzles (load-puzzles))))

(define all-puzzles (make-all-puzzle-vectors))
(define test-case (make-puzzle-vector (car (separate-puzzles (load-puzzles)))))
(define test-case2 (make-puzzle-vector (car (cdr (separate-puzzles (load-puzzles))))))

;(define bad-test-case
;  (let ((v (vector-append test-case))) ;;copy the test-case vector
;    (vector-set! v 0 5) ;;set a wrong value
;    v))
  

(define (index->row idx)
  (quotient idx 9))

(define (index->column idx)
  (modulo idx 9))

(define (coord->idex row column)
  (+ (* row 9) column))

(define (index->block-row idx)
  (quotient idx 27))

(define (index->block-column idx)
  (quotient (modulo idx 9) 3))

(define (index->block idx)
  (+ (* 3 (index->block-row idx)) (index->block-column idx)))
  
;;accessor functions 
(define (get-row puzz row-num)
  (define start (* row-num 9))
  (define (iter i results)
    (if (> i 8) results
        (iter (+ 1 i) (cons (vector-ref puzz (+ i start)) results) )))
  (reverse (iter 0 (list))))

(define (get-column puzz col-num)
  (define (iter i results)
    (if (> i 8) results
        (iter (+ 1 i)
              (cons (vector-ref puzz (+ (* i 9) col-num)) results))))
  (reverse (iter 0 (list))))

(define (get-block puzz block-num)
  (define vertical-offset (* 27 (quotient block-num 3)))
  (define horizontal-offset (* 3 (modulo block-num 3)))
  (for*/list ([i (in-range 3)]
             [j (in-range 3)])
    (vector-ref puzz
     (+ (* 9 i) vertical-offset horizontal-offset j))))


;;a default set with digits from 1-9.
(define digits (list->set (build-list 9 (lambda (x) (+ 1 x)))))

;;remove zeros from a set. 
(define (list->solved-set li)
  (set-remove (list->set li) 0))

;;create solution-sets using 
;;an accessor function. 
(define (get-solutions proc puzz)
  (for/vector ([i (in-range 9)])
    (list->solved-set (proc puzz i))))

;;cut down the solutions for a given cell
;;by cross-checking rows, columns, and blocks.
;;Takes the puzzle and sets for rows, columns an blocks.
(define (narrow-solutions puzz rows columns blocks idx)
  (define cur-value (vector-ref puzz idx))
  (cond
    [(> 0 cur-value) (set cur-value)]
    [else (set-subtract
           digits
           (vector-ref rows (index->row idx))
           (vector-ref columns (index->column idx))
           (vector-ref blocks (index->block idx)))]))



;;;try to solve by cross-checking
;;;numbers in rows and columns
;;;returns a solve puzzle vector
(define (rule1 puzz)
  (define rows (get-solutions get-row puzz))
  (define columns (get-solutions get-column puzz))
  (define blocks (get-solutions get-block puzz))
  (for/vector ([i (in-range 81)])
    (define cur (vector-ref puzz i))
    ;;save some work by 
    ;;creating the sets for 
    ;;rows columns and blocks
    ;;before iterating
    ;;over individual cells. 
    (cond [(> cur 0) cur]
          [else 
           (define solutions (narrow-solutions puzz rows columns blocks i))
           (cond 
             [(= 1 (set-count solutions))
              (car (set->list solutions))]
             [else 0])])))

;;a pretty-print function
;;to avoid staring at raw 
;;vectors.
(define (pretty-print puzz)
  (for ([i (in-range 81)])
    (define cur (vector-ref puzz i))
    (cond [(= 0 (modulo i 27))
           (newline) (newline)
           (display cur)]
      
          [(= 0 (modulo i 9)) 
           (newline)
           (display cur)]
          [(= 0 (modulo i 3))
           (display ":")
           (display cur)]
          [else
           (display cur)])))

       

(define (row-indices row-num)
  (build-list 9 (lambda (x) (+ (* row-num 9) x))))

(define (column-indices col-num)
  (build-list 9 (lambda (x) (+ (* 9 x) col-num))))

(define (block-indices block-num)
  (define vertical-offset (* 27 (quotient block-num 3)))
  (define horizontal-offset (* 3 (modulo block-num 3)))
  (for*/list ([i (in-range 3)]
             [j (in-range 3)])
     (+ (* 9 i) vertical-offset horizontal-offset j)))



;;;Rule2: if 
;;; x and y are unsolved cells.
;;; Possible solutions for x = (a b c)
;;; and possible solutions for y = (b c)
;;; x = a.

(define (get-multiple-from-vector vec li)
  (map (lambda (x) (vector-ref vec x)) li))

(define (rule2-solutions puzz)
  (define rows (get-solutions get-row puzz))
  (define columns (get-solutions get-column puzz))
  (define blocks (get-solutions get-block puzz)) 
  (for/vector ([i (in-range 81)])
      (define cur (vector-ref puzz i))
      (cond [(> cur 0)
             (set cur)]
            [else (narrow-solutions puzz rows columns blocks i)])))

(define (rule2-reduce solutions index-proc group-proc i)
    (define cur (vector-ref solutions i))
    (define others
      (get-multiple-from-vector solutions (remove i (group-proc (index-proc i)))))
  (set-subtract cur (set-union-list others)))

(define (rule2 puzz)
  (define solutions (rule2-solutions puzz))
  (for/vector ([i (in-range 81)])
    (cond [(> (vector-ref puzz i) 0)
           (vector-ref puzz i)]
          [else
           (define a (rule2-reduce solutions index->row row-indices i))
           (define b (rule2-reduce solutions index->column column-indices i))
           (define c (rule2-reduce solutions index->block block-indices i))
           (cond
             [(= 1 (set-count a)) 
              ;(display i) (display ":") 
              (car (set->list a))]
             [(= 1 (set-count b)) 
              ;(display i) (display ":") 
              (car (set->list b))]
             [(= 1 (set-count c)) 
              ;(display i) (display ":")
              (car (set->list c))]
             [else 0])])))


(define (solve-rule rule puzz)
  (define (iter puzz)
    (cond
      [(solved? puzz) ;;has the puzzle been solved?
       (values puzz #t)]
      [else
         (define next (rule puzz))
         (cond [(equal? puzz next) ;;failed to create a solution
                (values puzz #f)]
               [else (iter next)])]))
  (iter puzz))

(define (solve-and-pretty-print rule puzz)
  (define-values (final solved) (solve-rule rule puzz))
  (pretty-print final)
  (newline)
  (display solved))

           

(define (set-union-list li)
  (apply set-union li))


  
  
  
;;;Create a list of solutions 
(define (solutions puzz)
  (define rows (get-solutions get-row puzz))
  (define columns (get-solutions get-column puzz))
  (define blocks (get-solutions get-block puzz))
  (for/vector ([i (in-range 81)])
    (define cur (vector-ref puzz i))
    (define sol 
      (if (= cur 0)
          (narrow-solutions puzz rows columns blocks i)
          (set cur)))
    sol))

(define (solved? puzz)
  ;;a puzzle vector is solved if it has no zeros
  (zero? (vector-count zero? puzz)))

(define (solvable? puzz)
  ;;a puzzle can't be solved if there are no possible solutions
  (zero? (vector-count (lambda (solution-set) 
                         (> 1 (set-count solution-set))) (solutions puzz))))

(define (peers idx)
  (remove idx 
          (remove-duplicates
           (sort (append (row-indices (index->row idx))
                         (column-indices (index->column idx))
                         (block-indices (index->block idx))) < ))))

;(define peers-vector
;  (for/vector ([i (in-range 81)])
;    (peers i)))

(define (eliminate val-vector idx n)
  (define ps (peers idx))
  (for ([p ps])
    (define v (vector-ref val-vector p))
    (vector-set! val-vector p (set-remove v n)))
  (vector-set! val-vector idx (set n))
  
  val-vector)

(define test-case-solved
  (let-values ([(a b) (solve-rule rule1 test-case)]) a))

(define (val-solved? val-vector)
  ;;;count as solved if there's one and only one 
  ;;;solution for each cell
  (for/and ([v val-vector])
    (= 1 (set-count v))))

(define (val-fail? val-vector)
  (for/or ([v val-vector])
    (set-empty? v)))

(define (find-min val-vector)
  (define (loop min min-ref i)
    (cond 
      [(> i 80) min-ref]
      
      [else
       (define new-val (set-count (vector-ref val-vector i)))
       (cond
      
         [(and (> new-val 1) (< new-val min))
          (loop new-val i (+ 1 i))]
         [else (loop min min-ref (+ 1 i))])]))
  (loop 10 81 0))
        

(define (val-vector->list val-vector)
  (vector->list (vector-map set->list val-vector)))

(define (create-child gene-vector)
  ;;;Create a random mutant from the given solution vector.
  (for/vector ([v gene-vector])
    (cond
      [(= 1 (length v)) (first v)] ;;if only one solution possible, pass that solution
      [else 
       (define c (length v))
       (define vs v)
       (list-ref vs (random c)) ;;pick a solution from the set
       ])))

(define (score-puzzle original solution)
  ;;return the number of errors
  (for/sum 
   ([i (in-range 81)])
   (cond 
     [(not (zero? (vector-ref original i))) 
      0]
     [else
      (define foo (vector-ref solution i))
      (for/sum ([j (peers i)])
               (define bar (vector-ref solution j))
               (cond
                 [(= foo bar)
                  (eprintf "~a:~a:~a:~a\n" i j foo bar)
                   1]
                 [else 0]))])))
                 
   

(define (generate-n-children val-vector n)
  (for/list ([i (in-range n)])
    (create-child val-vector)))

(define (create-gene-pool children)
  (for/vector ([i (in-range 81)])
    (for/list ([c children])
      (vector-ref c i))))

(define (genetic-solve orig children generation-size mutant-number children-size)
  ;;create a mutant
  (define mutants (generate-n-children (vector-map set->list (solutions orig)) mutant-number))
  ;;append mutant to parents
  (define parents (append mutants children))
  ;;create the gene-bank
  (define genes (create-gene-pool parents))
  ;(print genes)
  ;;create the full generation
  (define generation (generate-n-children genes generation-size))
  ;;score
  (define (score-curry puzz) 
    (cons (score-puzzle orig puzz) puzz))
  (define scored-generation (map score-curry generation))
  (define (sort-car x y) (< (car x) (car y)))
  (define sorted-generation (sort scored-generation sort-car))
  (map cdr (take sorted-generation children-size)))

(define (recursive-gen-solve orig children generation-size mutant-number children-size generation-limit)
  (cond
    ;;return the best solution after n generations
    [(= 0 generation-limit) (car children)]
    ;;exit if the lowest score is 0
    [(= 0 (score-puzzle orig (car children))) (car children)]
    [else
     (define new-children (genetic-solve orig children generation-size mutant-number children-size))
     (eprintf "generation: ~a best: ~a\n" generation-limit (score-puzzle orig (car children)))
     (recursive-gen-solve orig new-children generation-size mutant-number children-size (- generation-limit 1))]))




(define (val-contradiction? val-vec)
  (for*/or ([i (in-range 81)]
             [j (peers i)])
    (define foo (vector-ref val-vec i))
    (define bar (vector-ref val-vec j))
    (and (= 1 (set-count foo))
         (= 1 (set-count bar))
         (equal? foo bar))))
         

(define (backtracking puzz)
  (define sols (solutions puzz))
  (define min (find-min sols))
  (cond 
    [(val-solved? sols)
     ;(print sols)
     (vector-map (lambda (s) (car (set->list s))) sols)]
    [(or (val-fail? sols) (val-contradiction? sols)) 
     ;(eprintf "fail1:~a\n" (vector-ref sols 20))
     #f]
    [else
     (for/or 
         ([i (vector-ref sols min)])
       ;(eprintf "~a:~a:~a\n" min i (vector-ref sols 20))
       (define newvector (vector-copy puzz))
       (vector-set! newvector min i)
       (backtracking (rule1 newvector))
       )]))

(define (euler96)
  (for/sum ([puzz all-puzzles])
    (define foo (backtracking puzz))
    (+ 
     (* (vector-ref foo 0) 100)
     (* (vector-ref foo 1) 10)
     (* (vector-ref foo 2) 1))))


(define (euler96-check)
  (for/list ([puzz all-puzzles])
    (define foo (backtracking puzz))
    (score-puzzle puzz foo)))


(define (euler96-pretty-print)
  (for ([puzz all-puzzles])
    (define foo (backtracking puzz))
    (eprintf "\n----\n")
    (pretty-print foo)))

(time (euler96))







       
    
    
  
  


  
  

  
  
  
            
  




           
   
           
           
      
