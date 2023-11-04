(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))  
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun get-mr-2s (n)
  (let ((a (- n 1)))
    (loop 
      for s = 1 then (+ 1 s)
      while (evenp (/ a (expt 2 s)))
      finally (return s))))

(defun get-mr-2d (n s)
  (values (floor (- n 1) (expt 2 s))))
 
(defun mr-test-candidate (n witness)
  (if (< n 4)
    T
    (if (evenp n)
      NIL
      (let* ((s (get-mr-2s n))
             (d (get-mr-2d n s)))
        (loop for r from 0 to (- s 1)
          for test = (expt-mod witness (* d (expt 2 r)) n)
          do (if (or (and (= test 1)(= 0 r)) (= test (- n 1)))
               (return T))
          finally (return NIL))))))


(defun mr-get-witnesses (n)
  (cond ((< n 1373653) '(2 3))
        ((< n 9080191) '(31 73))
        ((< n 475912341) '(2 7 61))
        ((< n 2152302898747) '(2 3 5 7 11))
        (T '(2 3 5 7 11 13 17))))

(defun mr-is-primep (n)
  (loop for witness in (mr-get-witnesses n)
    do (if (not (mr-test-candidate n witness))
         (return NIL))
    finally (return T)))

(defun sieve (maximum) 
 (cons 2 
  (let ((maxi (floor (/ (- maximum 1) 2))))
   (let ((sieve (make-array (1+ maxi) :element-type 'bit 
                                        :initial-element 0)))
    (loop for i from 1 to maxi
          when (zerop (bit sieve i))
            collect (+ (* 2 i) 1)
            and do (loop for j from (* 2 i (+ i 1)) 
                                 to maxi by (+ (* 2 i) 1)
                          do (setf (bit sieve j) 1)))))))


(defvar combine-hash (make-hash-table :test 'equal :rehash-size 100000))
												
				 			 
(defun gen-hash-values (maximum)
  (let ((pri (sieve maximum)))
	(loop for i in pri
		 do (loop for j in pri
				 when (< i j)
				 when (is-mutual-concat-prime-p i j)
				 do (setf (gethash (cons i j) combine-hash) 1)))))
				 
				 
(defun mcp-hash-p (i j) 
  (gethash (cons i j) combine-hash))

(defun mcp-hash-list-p (j listi)
  (loop for i in listi
	   do (if (not (mcp-hash-p i j))
			  (return NIL))
	   finally (return T)))
	   

(defvar primes (sieve 20000))

(defun is-prime-p (n)
  (let ((maxi (ceiling (sqrt n))))
	(loop 
	 for i in primes
	 while (<= i maxi)
	 do (if (eq 0 (mod n i))
			(return nil))
	 finally (return T))))

(defun side-length (n) (+ 1 (* 2 n)))

(defun gen-sides (n) 
  (let ((y (side-length n)))
    (loop 
	 for i from 1 to 3
	 counting (mr-is-primep (+ (* y y) (* y i -1) i)))))


(defun number-to-list (n) 
  (reverse (loop for i = n then (floor i 10) 
	   while (> i 0)
       collect (mod i 10))))

(defun list-to-number (numlist)
  (loop for i in numlist
	   for a = i then (+ i (* 10 a))
	   finally (return a)))





(defun mcp-list-p (a numlist)
  (loop for i in numlist
	   do (if (not (is-mutual-concat-prime-p a i))
			  (return NIL))
	   finally (return T)))


												   

(defun concat-numbers (a b) 
  (+ (* a (expt 10 (+ 1 (floor (log b 10))))) b))
												   
(defun is-mutual-concat-prime-p (a b)
  (let ((ab (concat-numbers a b)) (ba (concat-numbers b a)))
	(and (mr-is-primep ab)
		 (mr-is-primep ba))))
												   
(defun collect-concats (n candidates)
  (loop for i in candidates
    when (> i n)
    when (is-mutual-concat-prime-p n i)
    collect i))

(defun recursive-collect (n limit candidates results)
  (let ((newcands (collect-concats n candidates)))
    (if (eq limit 0)
      (list results)
      (loop for i in newcands
        ;;kill the branch if there's not enough candidates
        ;;left to complete it
        when (> (length newcands) (- limit 1))
        append (recursive-collect i (- limit 1) newcands (cons i results))))))

(defun euler60-loop (maxprime series-length)
  (let ((candidates (cdr (sieve maxprime)))
        (limit (- series-length 1))) 
    (loop for i in candidates
      for result = (recursive-collect i limit candidates (list i))
      append result)))



