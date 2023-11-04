(defun square (x) (* x x))
(defun triangle (x) (* x (+ x 1) (/ 1 2)))
(defun pentagonal (x) (* x (- (* 3 x) 1) (/ 1 2)))
(defun hexagonal (x) (* x (- (* 2 x) 1)))
(defun heptagonal (x) (* x (- (* 5 x) 3) (/ 1 2)))
(defun octagonal (x) (* x (- (* 3 x) 2)))

(defun in-range (a b) 
  (loop for i from a to b collect i))

(defvar squares (map 'list #'square (in-range 32 99)))
(defvar triangles (map 'list #'triangle (in-range 45 140)))
(defvar pentagonals (map 'list #'pentagonal (in-range 26 81)))
(defvar hexagonals (map 'list #'hexagonal (in-range 23 70)))
(defvar heptagonals (map 'list #'heptagonal (in-range 21 63)))
(defvar octagonals (map 'list #'octagonal (in-range 19 58)))

(defvar test-set (list octagonals heptagonals hexagonals pentagonals triangles squares))
(defvar test-set2 (list triangles squares pentagonals))
(defun first-two (n) (floor n 100))
(defun second-two (n) (mod n 100))
(defun front-cyclic-p (a b) (eq (second-two a) (first-two b)))
(defun back-cyclic-p (a b) (eq (first-two a) (second-two b)))

(defun recursive-collect (n sets results)
  (if (not sets)
      (list results)
    (loop for set in sets
          append (loop for i in set
                       when (front-cyclic-p n i)
                       append (recursive-collect 
                               i 
                               (remove set sets :test #'equal)
                               (cons i results))))))

(defun outer-outer (sets)
  (outer-loop (car sets) (remove (car sets) sets :test #'equal)))

(defun outer-loop (seta sets)
  (loop for i in seta
	   for result = (recursive-collect i sets (list i))
	   append result))


(defun filter-first-last (results)
  (loop for result in results
	   when (front-cyclic-p (car result) (car (last result)))
	   collect result))

(defun e61 () 
  (reduce #'+ (car (filter-first-last (outer-outer test-set)))))