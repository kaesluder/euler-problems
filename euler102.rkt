#lang racket/base
(require racket/string racket/list racket/file)

(define (load-triangles) (map parse-line (file->lines "triangles.txt")))
(define (parse-line line)
  (map string->number (regexp-split "," line)))

(define triangles (load-triangles))

;;individual conversion functions
;;to determine the baryocentric coordinates
;;of the origin in relationship to 
;;a triangle.
(define (bary1 x1 y1 x2 y2 x3 y3)
  (define top (+ (* (- y2 y3)(- 0 x3)) (* (- x3 x2)(- 0 y3))))
  (define bottom (+ (* (- y2 y3)(- x1 x3)) (* (- x3 x2)(- y1 y3))))
  (/ top bottom))

(define (bary2 x1 y1 x2 y2 x3 y3)
  (define top (+ (* (- y3 y1)(- 0 x1)) (* (- x1 x3)(- 0 y1))))
  (define bottom (+ (* (- y2 y3)(- x1 x3)) (* (- x3 x2)(- y1 y3))))
  (/ top bottom))

(define (bary3 x1 y1 x2 y2 x3 y3)
  (define top (+ (* (- y1 y2)(- 0 x2)) (* (- x2 x1)(- 0 y2))))
  (define bottom (+ (* (- y2 y3)(- x1 x3)) (* (- x3 x2)(- y1 y3))))
  (/ top bottom))


;;this doesn't seem to actually save any time
(define (bary-triplet x1 y1 x2 y2 x3 y3)
  ;;return the baryocentric triplet for the origin. 
  (define bottom (+ (* (- y2 y3)(- x1 x3)) (* (- x3 x2)(- y1 y3))))
  (define b1 (+ (* (- y2 y3)(- 0 x3)) (* (- x3 x2)(- 0 y3))))
  (define b2 (+ (* (- y3 y1)(- 0 x1)) (* (- x1 x3)(- 0 y1))))
  (define b3 (+ (* (- y1 y2)(- 0 x2)) (* (- x2 x1)(- 0 y2))))
  (list (/ b1 bottom) (/ b2 bottom) (/ b3 bottom)))
  
  

(define (test-triangle tri)
  ;;test to see if the origin falls within the triangle
  (for/and ([test (list bary1 bary2 bary3)])
    (define x (apply test tri))
    (and (< 0 x)
         (< x 1))))

(define (test-triangle2 tri)
  ;;test to see if the origin falls within the triangle
  ;;refactored version
  ;;origin falls inside the triangle 
  ;;if 0 < x < 1 for any x in the triplet
  (for/and ([x (apply bary-triplet tri)])
    (and (< 0 x)
         (< x 1))))

(define (euler102)
  (for/sum ([t triangles])
           (if (test-triangle t)
               1
               0)))

(time (euler102))
    



