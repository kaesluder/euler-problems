#lang racket
(require racket/vector)
(provide sieve)


(define (sieve n)  
  (let*
      ((limit (round (/ (+ 1 (inexact->exact (round (sqrt n))))
                        2)))
       (m (round (/ (+ 1 n) 2)))
       (ar (make-vector m 1)))

    ;; element 0 is really for number 1, so we do not 
    ;; want to drop its multiples.
    (for ([i (in-range 1 limit)])
         (when (= (vector-ref ar i) 1)
           (let
               ((p (+ (* 2 i) 1)))
             (for ([j (in-range (* (+ p 1) i) m p)])
                  (vector-set! ar j 0)))))

    ;; Collect all of the non-zero elements
    (let ((result 
           (for/vector
            ([(x i) (in-indexed (in-vector ar))]
             #:when (> x 0))
            (+ 1 (* 2 i)))))
      ;; now, set the first element to 2, since it is
      ;; currently holding 1.
      (vector-set! result 0 2)
      result)))

(define (find-first-prime n primeset)
  (or (for/first ([i primeset]
              #:unless (> (* i i) n)
              #:when (= (modulo n i) 0)) 
    i) n))

(define (trial-division n primeset)
  (define (iter n primeset results)
    (if (= n 1) 
        results
        (let ([i (find-first-prime n primeset)])
          (iter (/ n i) primeset (cons i results)))))
  (iter n primeset '()))

(define (is-prime? n primeset)
           (= n (find-first-prime n primeset)))



       