#lang racket

(require racket/file)
(require racket/list)
(define poker-lines (file->lines "poker.txt"))

;;;split into hands
(define (player1-hand str) (substring str 0 14))
(define (player2-hand str) (substring str 15))

;;;get suit values
(define (get-suits str) 
  (for/list ([i (in-range 1 15 3)])
    (string-ref str i)))

;;;get number values

(define value-map (hash 
                   #\2 2
                   #\3 3
                   #\4 4
                   #\5 5
                   #\6 6
                   #\7 7
                   #\8 8
                   #\9 9
                   #\T 10
                   #\J 11
                   #\Q 12
                   #\K 13
                   #\A 14))

;;;create a sorted list of card values
(define (get-values str)
  (sort (for/list ([i (in-range 0 15 3)])
    (hash-ref value-map (string-ref str i))) >))

;;;sort frequencies by number of 
;;;cards, then by card values if 
;;;the number of cards is equal.
;;;This pushes n-of-a-kind
;;;to the top. 
(define (sort-helper x y) 
  (if (= (cdr x) (cdr y))
      (> (car x) (car y))
      (> (cdr x) (cdr y))))

;;;create a sorted list of (card . frequency) 
;;; values
(define (frequencies values)
   (sort (remove-duplicates 
    (for/list ([i values])
      (cons i (count (lambda (x) (= x i)) values)))) sort-helper))

;;;identify full house
;;;three cards in the first slot, two in the second
(define (is-full-house? freq)
  (if (and (= 3 (cdr (first freq)))
           (= 2 (cdr (second freq))))
      #t
      #f))

;;;identify two pair
;;;two cards in the first two slots
(define (is-two-pair? freq)
  (if (and (= 2 (cdr (first freq)))
           (= 2 (cdr (second freq))))
      #t
      #f))

;;;identify n of a kind
;;;n of a kind is 
;;;pushed to the head of the list
;;;by the sort function
(define (is-n-kind? n freq)
  (if (= n (cdr (first freq)))
      #t
      #f))

;;;identify straight
;;;standard zip-map and test the differences
(define (is-straight? values)
  (for/and ([i (map - (drop values 1) (drop-right values 1))])
    (= i -1)))

;;;identify flush
;;;just test if all suits
;;; match the first one
(define (is-flush? str)
  (let ([suits (get-suits str)])    
    (for/and ([c suits])
      (eq? c (car suits)))))


;;;adds the big-score (full-house, straight-flush, etc)
;;;to a number representing cards in decreasing order
(define (score-cards big-score freqs)
  (+ (* big-score (expt 16 5)) 
     (for/sum ([i '(4 3 2 1 0)]
            [j freqs])
           (* (car j) (expt 16 i)))))

;;;score an individual hand.
(define (score-hand str)
  (let* ([vals (get-values str)]
        [freq (frequencies vals)])
    ;;;this determines the most significant digit 
    ;;; in the score
    (let ([big-score (cond
                       [(and (is-straight? vals) (is-flush? str)) 10] 
                       ;;royal flush is just an ace-high straight-flush
                       [(is-n-kind? 4 freq) 8]
                       [(is-full-house? freq) 7]
                       [(is-flush? str) 6]
                       [(is-straight? vals) 5]
                       [(is-n-kind? 3 freq) 4]
                       [(is-two-pair? freq) 3]
                       [(is-n-kind? 2 freq) 2]
                       [else 0])])
      ;;;compute the score by adding individual cards to the big-score
      (score-cards big-score freq))))

;;returns #t if player1 has the higher score.
(define (compare-hands str) 
  (> (score-hand (player1-hand str)) (score-hand (player2-hand str))))

;;standard filter/length reporting idiom
(define (euler54) (length (filter compare-hands poker-lines))) 

