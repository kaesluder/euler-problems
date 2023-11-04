#lang racket
(define (totients n)
  (let ((tots (make-vector (+ n 1))))
    (do ((i 0 (+ i 1))) ((< n i))
      (vector-set! tots i i))
    (do ((i 2 (+ i 1))) ((< n i) tots)
      (when (= i (vector-ref tots i))
        (vector-set! tots i (- i 1))
        (do ((j (+ i i) (+ i j))) ((< n j))
          (vector-set! tots j
            (* (vector-ref tots j) (- 1 (/ i)))))))))