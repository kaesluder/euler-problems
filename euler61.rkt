#lang racket/base
(require racket/stream)
(require racket/list)

(define (square x) (* x x))
(define (triangle x) (* x (+ x 1) (/ 1 2)))
(define (pentagonal x) (* x (- (* 3 x) 1) (/ 1 2)))
(define (hexagonal x) (* x (- (* 2 x) 1)))
(define (heptagonal x) (* x (- (* 5 x) 3) (/ 1 2)))
(define (octagonal x) (* x (- (* 3 x) 2)))
(define squares (map square (stream->list(in-range 32 100))))
(define triangles (map triangle (stream->list(in-range 45 141))))
(define pentagonals (map pentagonal (stream->list(in-range 26 82))))
(define hexagonals (map hexagonal (stream->list(in-range 23 71))))
(define heptagonals (map heptagonal (stream->list(in-range 21 64))))
(define octagonals (map octagonal (stream->list(in-range 19 59))))

(define test-set (list octagonals heptagonals hexagonals pentagonals triangles squares))
(define (first-two n) (quotient n 100))
(define (second-two n) (modulo n 100))
(define (front-cyclic? a b) (= (second-two a) (first-two b)))
(define (back-cyclic? a b) (= (first-two a) (second-two b)))

(define (filter-cyclics n li)
  (filter (lambda (x) (front-cyclic? n x)) li))

(define (find-next-continuation n li set1 set2 results)
  (if (empty? set1)
      results
      (if (empty? li)
          (find-next-continuation n (car set1) (cdr set1) set2 results)
          (if (front-cyclic? n (car li))
              (let ([x (car li)]
                    [nextset1 (remove li set2)])
                (find-next-continuation 
                 x (car nextset1) (cdr nextset1) (cdr nextset1) (cons x results)))
              (find-next-continuation n (cdr li) set1 set2 results)))))

(define (foo seq li)
   (for/list ([i (filter-cyclics (car seq) li)])
     (cons i seq)))

(define (extend-sequences seqs li)
  (apply append (for/list ([s seqs])
    (foo s li))))

(define (recur-extend seqs sets)
  (if (empty? sets)
      seqs
      (let ([full-sets sets])
        (for/fold ([results '()])
          ([s sets])
          (append results (extend-sequences 
                           (recur-extend seqs (remove s full-sets)) s))))))

(define (filter-first-last seqs)
  (filter (lambda (seq) (front-cyclic? (car seq) (last seq))) seqs))

(define (build-list) 
  (recur-extend (map list (car test-set)) (cdr test-set)))

(define (e61) (apply + (car (filter-first-last (build-list)))))

(e61)
          
    
    
;(trace recur-extend)
;(trace extend-sequences)       






   
  
  
  
  





