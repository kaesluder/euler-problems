#lang racket/base
(require racket/string)
;;(require racket/regexp)
(require racket/list)
(require racket/math racket/file)


(define (char->number c)
  (- (char->integer c) 48))

(define (number->list n)
  (define (ntl n)
    (if (zero? n)
        '()
        (cons (remainder n 10) (ntl (quotient n 10)))))
  (reverse (ntl n)))

(define (list->number numlist)
  (for/fold ([num 0])
    ([i numlist])
    (+ i (* 10 num))))
  

(define (list-represent-number x)
  (sort (map char->number (string->list (number->string x))) <))

(define (list-represent-string str)
  (sort (map char->integer (string->list str)) <))

(define (number-anagram? x y)
  (equal? (list-represent-number x) (list-represent-number y)))

(define (string-anagram? x y)
  (equal? (list-represent-string x) (list-represent-string y)))

(define (load-words) 
  (file->lines "words.txt"))


(define (string-remove-char str c)
  (list->string (remove* (list c) (string->list str))))

(define (string-trim-quote str)
  (list->string (drop (drop-right (string->list str) 1) 1)))

(define (parse-words)
  (map string-trim-quote (regexp-split #rx"," (car (load-words)))))

(define (filter-by-string-length li len)
  (define (right-length? str)
    (= len (string-length str)))
  (filter right-length? li))

(define (max-str-length li)
  (argmax string-length li))

;;search a list for a 
;;matching string anagram
(define (string-anagram-match str li)
  (define lr (list-represent-string str))
  (for/or ([i li])
    (if (equal? (list-represent-string i) lr) i #f)))

(define (string-anagram-matches li)
  (define (iter li-inner results)
    (cond 
      [(null? li-inner) results]
      [else
       (define this (car li-inner))
       (define that (string-anagram-match this (cdr li-inner)))
       (cond 
         [that (iter (cdr li-inner) (cons (list this that) results))]
         [else (iter (cdr li-inner) results)])]))
  (iter li (list)))

(define (length-of-anagrams li)
  (for/fold ([results (list)])
    ([i (in-range 1 (+ 1 (string-length (argmax string-length li))))])
    (define this (length (string-anagram-matches (filter-by-string-length li i))))
    (if (> this 0)
        (cons i results)
        results)))

;;;inefficient but does the trick
;;;frequencies for a lit
(define (frequencies li)
  (sort (for/list ([i (remove-duplicates li)])
    (count (lambda (x) (equal? i x)) li)) <))


(define (square-anagrams min max)
  (define start (integer-sqrt max))
  (define end (integer-sqrt min))
  (define results (make-hash))
  (define (iter i)
    (cond 
      [(< i end) (filter (lambda (x) (< 1 (length x))) (hash-values results))]
      [else 
       (define s (* i i))
       (define lr (list-represent-number s))
       ;;push our result onto the hash
       (hash-set! results lr (cons s (hash-ref results lr (list))))
       (iter (- i 1))]))
  (iter start))

;;append a value to a list in a hash 
;;cell, creating a new cell with an empty
;;list as a default
(define (hash-append! hs key value)
  (hash-set! hs key (cons value (hash-ref hs key (list)))))


(define (word-anagram-hash n word-list)
  (define words (string-anagram-matches (filter-by-string-length word-list n)))
  (define word-freqs (make-hash))
  (for* ([w words])
    (define wf (frequencies (list-represent-string (car w))))
    (hash-append! word-freqs wf w))
  word-freqs)

(define (number-anagram-hash n word-list)
  (define words (string-anagram-matches (filter-by-string-length word-list n)))
  (define numbers (square-anagrams (expt 10 (- n 1)) (expt 10 n)))
  (define word-freqs (word-anagram-hash n word-list))
  (define number-freqs (make-hash))
  (for* ([ns numbers])
    (define nf (frequencies (list-represent-number (car ns))))
    (define test (hash-ref word-freqs nf #f))
    (when test (hash-append! number-freqs nf ns)))
  number-freqs)
    
    

(define (caesar-table2 w n)
  (define table (make-hash))
  (for ([c (string->list w)]
        [d (number->list n)])
    (hash-set! table c d))
  table)

(define (caesar-table w n)
  (make-immutable-hash (map cons (string->list w) (number->list n))))

(define (caesar-encrypt table word)
  ;;curry
  (define (lookup c) (hash-ref table c))
  (list->number (map lookup (string->list word))))

(define (square-anagram-test? word1 word2 n)
  (define new-val (caesar-encrypt (caesar-table word1 n) word2))
  (cond 
    ;;catch leading zeroes
    [(< (order-of-magnitude new-val) (order-of-magnitude n))
     #f]
    [(integer? (sqrt (caesar-encrypt (caesar-table word1 n) word2)))
     #t]
    [else #f]))

(define (compare-anagrams words numbers)
  (for/fold ([results (list)])
    ([n numbers])
    (define w1 (first words))
    (define w2 (second words))
    (cond 
     [(square-anagram-test? w1 w2 n)
      (cons (cons n words) results)]
     [(square-anagram-test? w2 w1 n)
      (cons (cons n words) results)]
     
     [else results])))

(define (compare-sets word-pairs number-sets)
  (apply append (for*/fold ([results (list)])
    ([pair word-pairs]
     [ns number-sets])
    (define result (compare-anagrams pair ns))
    (if (> (length result) 0)
        (cons result results)
        results))))

(define (compare-hashes word-hash number-hash)
  (for*/fold ([results (list)])
    ([key (hash-keys word-hash)])
    (define words (hash-ref word-hash key))
    (define numbers (hash-ref number-hash key))
    (define result (compare-sets words numbers))
    (if (> (length result) 0)
        (append result results)
        results)))

(define (e98-loop n)
  (define all-words (parse-words))
  (for*/fold ([results (list)])
    ([i (in-range 3 (+ 1 n))])
    (define word-hash (word-anagram-hash i all-words))
    (define number-hash (number-anagram-hash i all-words))
    (define result (compare-hashes word-hash number-hash))
    (append result results)))

(define (e98-loop2 n)
  (define all-words (parse-words))
  (define (iter i)
    (cond 
      [(= i 1)
       #f]
      [else
       (define word-hash (word-anagram-hash i all-words))
       (define number-hash (number-anagram-hash i all-words))
       (define result (compare-hashes word-hash number-hash))
       (cond 
         [(> (length result) 0) result]
         [else (iter (- i 1))])]))
  (iter n))
       
      

(define (e98)
  (car (argmax car (e98-loop2 9))))

(display (time (e98)))
(newline)
  






      








  
  






    

    
  
























