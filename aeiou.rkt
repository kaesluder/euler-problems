#lang racket

(define vowels (string->list "aeiou"))

;;recursive function for searching the word
(define (search-word-iter search-list word-list)
  (cond
    [(null? search-list) #t]
    [else 
     ;;search the word-list for the first item on 
     ;;search-list. member returns the tail 
     ;;starting from the first occurance of 
     ;;of the search item
     (define result (member (car search-list) word-list))
     (cond
       ;;exit if current search item isn't found
       [(not result) #f] 
       ;;recursively test the next item against the 
       ;;tail of the word. 
       [else (search-word-iter (cdr search-list) result)])]))

;;curry and search for vowels
(define (search-word-vowels word)
  (search-word-iter vowels (string->list word)))

;;this isn't memory-safe but good 
;;enough for this dictionary
(define (aeiou) (filter search-word-vowels (file->lines "english-words.95.txt")))

(define (sorted? proc lst)
  (for/and ([a lst]
            [b (cdr lst)])
    (proc a b)))

;;curry and unpack the word 
;;string
(define (word-sorted? word)
  (sorted? char<=? (string->list word)))

;;run the test only on words
;;longer than six letters
(define (word-test? word)
  (if (< 5 (string-length word))
      (word-sorted? word)
      #f))

(define (alphabetical-order) (filter word-test? (file->lines "english-words.95.txt")))

;;(time (aeiou))