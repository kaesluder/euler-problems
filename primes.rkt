#lang racket/base
(require racket/vector)
;;(require (planet dherman/memoize:3:1))


(provide sieve trial-division is-prime? is-prime2? is-prime-mr? find-first-prime)

;;vector-based seive of Eratosthenes. Produces all primes less than 
;;n as a fector
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

(define a (sieve 1000))

;;a faster algoritm using recursion and multiple
;;exit points. Primeset is a list of primes.
;;The largets prime in the set must be less
;;than the square root of n.
(define (is-prime2? n primeset)
  (let ([maxi (vector-length primeset)]
        [maxprime (floor (sqrt n))])
    (define (iter n index)
      (if (>= index maxi)
          #f
          (let ([x (vector-ref primeset index)])
            (if (> x maxprime) 
                #t
                (if (= 0 (modulo n x)) 
                    #f
                    (iter n (+ 1 index)))))))
    (iter n 0)))



;;determine s in the Miller-Rabin algorithm
;;is is the largest integer such that 2^s is a 
;;factor of n
(define (get-mr-2s n)
  (let ([a (- n 1)])
    (define (iter d s)
      (if (odd? (quotient d (expt 2 s)))
          s
          (iter d (+ s 1))))
    (iter a 1)))

;;detrmine d in the Miller-Rabin algorithm
;;d*2^s = n-1
(define (get-mr-2d n s)
  (quotient (- n 1) (expt 2 s)))

;;test individual candidate primes n against a witness.
;;a candiate is possibly prime if 
;;mod(n) witness^d = 1 or mod(n) witness^(d*2^s) = n-1 (where s < r)
;;returns #t if it's a strong candidate prime.
(define (mr-test-candidate n witness)
  ;;don't test 2 or 3.
  (if (< n 4) 
      #t
      ;;fitler out evens from the start
      (if (even? n) 
          #f 
          (let* ([s (get-mr-2s n)]
                 [d (get-mr-2d n s)])
            
            ;;if all the tests are false, then n is composite.
            ;;cacluate test values for range 0 s-1
            (for/or ([r (in-range 0 s)])
              (let* ([test (modular-pow witness (* d (expt 2 r)) n)])
                ;;first test only applies if r=0
                (or (and (= 0 r ) (= test 1)) (= test (- n 1)))))))))

;;deterministic test primes up to 3.42*10^14
;;returns true if the number is prime.
(define (is-prime-mr? n)
  ;;witnesslists pulled from wikipedia. Should be sufficient to 
  ;;identify all primes below a certain number.
  (let ([witnesslist (cond [(< n 1373653) '(2 3)]
                               [(< n 9080191) '(31 73)]
                               [(< n 475912341) '(2 7 61)]
                               [else '(2 3 5 7 11 13 17)])])
        
        ;;invert the test becayse we want to know if 
        ;;any of the tests return composite.
        (not (for/or ([i witnesslist]
                      #:when (> n i))
               (not (mr-test-candidate n i))))))
  
  

;;modular power function.
;;modular power using fast exponentiation
;;and calculating the modulus of intervening steps. 
(define (modular-pow base exponent modulus)
  (define (iter base exponent m result)
    (if (<= exponent 0)
        result
        (let ([newbase (modulo (* base base) m)]
              [newresult (if (= 1 (bitwise-and 1 exponent))
                             (modulo (* result base) m)
                             result)]
              [newxp (arithmetic-shift exponent -1)])
          (iter newbase newxp m newresult))))
  (iter base exponent modulus 1))