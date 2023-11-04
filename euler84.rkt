#lang racket
(require racket/generator)

(define (roll-die) (add1 (random 4))) 

(define (jail n) 10)
(define (go n) 0)
(define (c1 n) 11)
(define (e3 n) 24)
(define (h2 n) 39)
(define (r1 n) 5)
(define (pass n) n)
(define (nextrr n)
  (cond [(= n 7) 15]
        [(= n 22) 25]
        [(= n 36) 5]))
(define (back3 n) (- n 3))
(define (next-utility n) 
  (cond [(or (= n 36) (= n 7)) 12]
        [(= n 22) 28]))

(define community-chest-seq (append (list jail go) (for/list ([i (in-range 0 14)]) pass)))
(define (community-chest) (sequence->repeated-generator (shuffle community-chest-seq)))

(define chance-seq (append 
                    (list go jail c1 e3 h2 r1 nextrr nextrr next-utility back3) 
                    (for/list ([i (in-range 0 6)]) pass)))

(define (chance) (sequence->repeated-generator (shuffle chance-seq)))

(define (turn start doubles cc ch)
  (define roll1 (roll-die))
  (define roll2 (roll-die))
  (define rollsum (+ roll1 roll2))
  (cond [(and (= roll1 roll2)
              (= 2 doubles)) (values 10 0)]
        [(= roll1 roll2) (move start rollsum (add1 doubles) cc ch)]
        [else (move start rollsum 0 cc ch)]))

(define (move start rollsum doubles cc ch)
  (define ns (+ rollsum start))
  ;;loop around
  (define next-square (cond [(> ns 39) (- ns 40)]
                            [else ns]))
  (cond 
    ;;community chest
    [(member next-square (list 2 17 33)) ;;(print "community chest")
                                         (values ((cc) next-square) doubles)]
    ;;chance
    [(member next-square (list 7 22 36)) ;;(print "chance")
                                         (values ((ch) next-square) doubles)]
    ;;go to jail
    [(= 30 next-square) (values 10 0)]
    ;;default 
    [else (values next-square doubles)]))

(define (count-turns iterations results-hash)
  ;;reshuffle chest and chance
  (define chest (community-chest))
  (define cha (chance))
  (define-values (res s d) (for/fold ([results results-hash]
             [square 0]
             [doubles 0])
    ([i (in-range 0 iterations)])
    (let-values ([(s d) (turn square doubles chest cha)])
      (values 
       (hash-update results s add1 0)
       s
       d))))
  res)

(define (count-games x y)
  (for/fold ([results (hash)])
    ([z (in-range 0 x)])
    (count-turns y results)))
    
(define (sort-games x y) (sort (hash->list (count-games x y)) (lambda (a b) (> (cdr a) (cdr b)))))

  
        
         