#lang racket/base
(require racket/list)
(require racket/math)
(require racket/generator)

(define (pells-equation D y)
  (sqrt (+ 1 (* D (* y y)))))

(define (pells-equation2 D x y)
  (- (* x x ) (* D y y)))

(define (pells? D x y)
  (= 1 (pells-equation2 D x y)))

(define (is-square? s)
  (define sr (sqrt s))
  (= sr (floor sr)))


(define (continued-fraction s)
  (define m 0)
  (define d 1)
  (define a (integer-sqrt s))
  (define (iter a m d results)
    (define m1 (- (* d a) m))
    (define d1 (quotient (- s (* m1 m1)) d))
    (define a1 (inexact->exact (floor (/ (+ (sqrt s) m1) d1))))
    (if (member (list a1 m1 d1) results)
        results
        (iter a1 m1 d1 (cons (list a1 m1 d1) results))))
  (iter a m d '()))

(define (repeat li count)
  (for/fold ([result '()])
    ([i (in-range 0 count)])
    (append li result)))

(define (convergents s iterations)
  (define list-notation (reverse (map car (continued-fraction s))))
  (define list-sequence (repeat list-notation (+ 1 (quotient iterations (length list-notation)))))
  (define sr (integer-sqrt s))
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
  (drop (reverse (iter (list (cons sr 1) (cons 1 0)) list-sequence)) 2))



(define (reducable? a b c d)
  (cond [(zero? b) #f]
        [(= (quotient a b) (quotient c d)) #t]
        [else #f]))

;;;this version retunrs 
;;;a reversed list of digits.
(define (number-to-list n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (number-to-list (quotient n 10)))))


(define (decimal-notation s len)
  (define list-notation (reverse (map car (continued-fraction s))))
  (define list-generator (sequence->repeated-generator list-notation))
  (define sr (integer-sqrt s))
  ;;a little hack to make it work for numbers > 100
  (define start-results (cdr (number-to-list sr)))
  (define start (car (number-to-list sr)))
  (define (iter results n1 d1 n2 d2 ls)
    (cond [(= (length results) len) results]
          [else 
           (define e (ls))
           (cond [(reducable? n1 d1 n2 d2)
                  (define newres (quotient n1 d1))
                  (define n3 (* 10 (- n1 (* (quotient n1 d1) d1))))
                  (define n4 (* 10 (- n2 (* (quotient n2 d2) d2))))
                  (define newn (+ n3 (* n4 e)))
                  (define newd (+ d1 (* d2 e)))
                  (iter (cons newres results) n4 d2 newn newd ls)]
                 [else 
                  (define newn (+ n1 (* n2 e)))
                  (define newd (+ d1 (* d2 e)))
                  (iter results n2 d2 newn newd ls)])]))
  (reverse (iter start-results 1 0 start 1 list-generator)))
                 
                  
         
(define (find-min-x maximum-d max-iterations)
  (for/list ([D (in-range 2 maximum-d)]
             #:when (not (is-square? D)))
    (for/first ([c (convergents D max-iterations)]
                #:when (pells? D (car c) (cdr c)))
      (list D (car c) (cdr c))))) 

(define (e66) (time (argmax second (find-min-x 1000 50))))

(define (e80) 
  (for/sum ([i (in-range 1 100)]
            #:when (not (is-square? i)))
           (apply + (decimal-notation i 100))))

(time (e80))
 
    
  

