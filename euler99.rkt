#lang racket/base
(require racket/list)
(require racket/file)

;;calculate log(a^b) using the 
;;forumla log(a^b) = b*log(a)
(define (log-expt base power) 
  (* power (log base)))  

;;load the file
(define (load-exponents) (file->lines "base_exp.txt"))

(define (parse-exponents line) ;;split each line and convert to numbers
  (map string->number (regexp-split "," line)))

(define (log-expt-line line) ;;calculate the natural log of each line
  (apply log-expt (parse-exponents line)))

(define (number-lines li) ;;attach line numbers to our values
  (map cons li (build-list (length li) (lambda (x) (+ 1 x)))))

;;find the maximum and return the line number
(define (euler99) 
  (cdr (argmax 
        (lambda (line) (log-expt-line (car line))) 
        (number-lines (load-exponents)))))


(display (time (euler99)))
(newline)

  