#lang racket/base
(require racket/file)
(require racket/list)
(require racket/set)

;;load the file into a list of strings
(define keylog-lines (file->lines "keylog.txt"))

;;convert each character to a number. 
;;more convenience than anything else
(define keylog-list 
  (for/list ([line keylog-lines])
    (for/list ([i (in-range 0 3)])
      (string->number (string (string-ref line i))))))

;;identify the followers for each number
(define (followers)
  (for/fold ([results (hash)])
    ([item keylog-list])
    (define a (first item))
    (define b (second item))
    (define aset (hash-ref results a (set)))
    (define bset (hash-ref results b (set)))
    (define newa (set-union (set b (third item)) aset))
    (define newb (set-add bset (third item)))
    (hash-set (hash-set results b newb) a newa)))

;;identify the priors for each number
;;the numbers. 
(define (priors)
  (for/fold ([results (hash)])
    ([item keylog-list])
    (define a (third item))
    (define b (second item))
    (define aset (hash-ref results a (set)))
    (define bset (hash-ref results b (set)))
    (define newa (set-union (set b (first item)) aset))
    (define newb (set-add bset (first item)))
    (hash-set (hash-set results b newb) a newa)))

;;list any doubles
(define (doubles)
  (define p (priors))
  (define f (followers))
  (for/list ([i (in-range 0 10)])
    (set-intersect (hash-ref f i (set)) (hash-ref p i (set)))))

;;a test function. tests if the triplet
;;can be derived from the target 
;;passphrase
(define (test-triplet triplet target)
  (define lengths 
    (for/list ([t triplet])
      (length (member t target))))
  (apply > lengths))

;;tests a password by testing
;;all triplets. Returns a list
;;of triplets that are inconsistent
;;with the password
(define (validate-password target)
  (for/list ([item keylog-list]
             #:when (not (test-triplet item target)))
    item))

;;remove a single edge from 
;;a hash
(define (remove-edge n p-hash)
  (define (updater p-set) 
    (set-remove p-set n))
  (for/fold ([results p-hash])
    ([key (hash-keys p-hash)])
    (hash-update results key updater)))

;;find all keys where the set
;;is empty.
(define (find-empty p-hash)
  (for/list ([key (hash-keys p-hash)]
             #:when (set-empty? (hash-ref p-hash key)))
    key))


;;topological sort using a hash of priors and followers
(define (topological-sort p f)
  ;the start is a number with no priors, only followers
  (define start (car (remove* (hash-keys (priors)) (hash-keys (followers)))))
  ;;entrypoint to our recursive function
  (define (iter ps results)
    (cond [(= 0 (hash-count ps))
           results]
          [else 
           ;the last number is pulled from results
           (define last (car results))
           ;remove the edges from the last number
           (define edges-removed (remove-edge last ps))
           ;the next number in the sequence is the one without 
           ;any incoming edges. (All previous edges have been removed.)
           (define next (car (find-empty edges-removed)))
           (define new-results (cons next results))
           ;remove empties from our list of priors. 
           (define new-priors (hash-remove edges-removed next))
           (iter new-priors new-results)]))
  (reverse (iter p (list start))))

(define (list-to-number numlist)
  (for/fold ([num 0])
    ([i numlist])
    (+ i (* 10 num))))

(define (euler79) (list-to-number (topological-sort (priors) (followers))))
(time (euler79))
    

  
  
  
    



