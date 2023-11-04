#lang racket/base
(require racket/list)
(require racket/string)

;;produce a list of possible continuations
;;from a sequence and a list of possibilities
;;for the inner and outer ring.
(define (extend1 seq inner-list outer-list)
  (define pivot (last (car seq)))
  (cond [(empty? inner-list) seq]
        [(empty? outer-list) seq]
        [else (for*/list ([i outer-list]
                          [j inner-list]
                          #:when (= 14 (+ i j pivot)))
                (cons (list i pivot j) seq))]))

;;probably a better way to code this
;;remove known numbers on the inner
;;ring from a list of possibilities.
(define (reduce-inners seq)
  (define known-inners
    (remove-duplicates 
     (apply append (for/list ([triplet seq])
                      (list (second triplet) (last triplet))))))
  (define unknowns '(1 2 3 4 5))
  (remove* known-inners unknowns))

;;probably a better way to code this
;;remove known numbers on the outer
;;ring from the list of possibilities.
(define (reduce-outers seq)
  (define known-outers (map car seq))
  (define unknowns '(6 7 8 9 10))
  (remove* known-outers unknowns))

;;identify all possible continuations
;;and append to the list of sequences
(define (extend-sequences seqs)
  (apply append (for/list ([s seqs])
    (extend1 s (reduce-inners s) (reduce-outers s)))))

;;recurisively apply extend-sequences to a list
;;of possibilities
(define (recurse-apply seqs)
  (for/fold ([results seqs])
    ([i (in-range 0 3)])
    (extend-sequences results)))

;;produce all starting sequences.
;;outer-ring numbers are in the range
;;6-10. 
;;First inner-ring number is range
;;1-5. 
;;Second inner-ring number is 
;; 14 - (inner + outer).
;;also filter out duplicates.
(define (start-seqs)
  (map list (for*/list ([i (in-range 6 11)]
              [j (in-range 1 6)]
              #:when (and (> 14 (+ i j))
                          (not (= i (- 14 i j)))
                          (not (= j (- 14 i j)))))
    (list i j (- 14 i j)))))

;;complete the last triplet.
(define (complete-sequence seq)
  (define pivot (last (first seq)))
  (define bottom (second (last seq)))
  (define top (car (reduce-outers seq)))
  (cons (list top pivot bottom) seq))

;;produce a full list of all possible
;;completions
(define (full-seqs) 
  (for/list ([seq (recurse-apply (start-seqs))])
    (reverse (complete-sequence seq))))
    
;;a simple function to rotate a sequence
;;until a predicate matches. Returns the 
;;unrotated sequence of the predicate will
;;never match.
(define (rotatef proc seq)
  (cond [(not (findf proc seq)) seq]
        [(proc (car seq)) seq]
        [else (rotatef proc (append (cdr seq) (list (car seq))))]))


;;rotate and remove duplicates
(define (seqs-rotated)
  (remove-duplicates 
   (for/list ([seq (full-seqs)])
    (rotatef (lambda (x) (= 6 (car x))) seq))))

;;reduce the set down to a list of strings
(define (seqs-reduced)
  (for/list ([seq (seqs-rotated)])
   (apply string-append 
          (map number->string (apply append seq)))))

;;sort the string and report the maximum.
(define (e68) 
  (car (sort (seqs-reduced) string>?)))

(time (e68))













                                        






             
             