(defun read-cipher-text-file (filename)
  (with-open-file (fh filename)
	(read-line fh nil)))

(defun comma-split (string)
  "Split a comma-separate string."
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defvar cipher-text-string (read-cipher-text-file "cipher1.txt"))

(defvar cipher-num-list (map 'list #'parse-integer (comma-split cipher-text-string)))

;;; this could probably be generalized
(defun take-by-three (offset li) 
  (loop for elem in (nthcdr offset li) by #'cdddr collect elem))

;;;
(defun build-frequencies (li)
  "build an assoc list of frequencies of items in list"
  (let ((symlist (remove-duplicates li)))
	(sort (loop for sym in symlist 
		 for freq = (length (remove-if-not #'(lambda (x) (eq sym x)) li))
		 collecting (cons sym freq)) #'cdr-gt)))

(defun cdr-gt (a b)
  "helper function returns true if (cdr a) > (cdr b)"
  (> (cdr a) (cdr b)))

(defun get-trial-keys (freqs)
  "gets trial keys based on the assumption that e (101) is common"
  (loop for i = 1 then (+ i 1)
  for freq in freqs
  while (< i  6)
  collect (logxor 101 (car freq))))

;;used for testing
(defvar segment-a (take-by-three 0 cipher-num-list))
(defvar segment-b (take-by-three 1 cipher-num-list))
(defvar segment-c (take-by-three 2 cipher-num-list))

(defun xor-with (key num-list)
  "maps xor (key value) over a numeric list"
  (map 'list #'(lambda (x) (logxor key x)) num-list))

(defun numlist-to-string (num-list)
  "convert a numlist to string"
  (concatenate 'string (map 'list #'code-char num-list))) 

(defun try-keys-string (num-list)
  "'decrypts' a num-list attempting to derive keys from item frequencies. text output"
  (let ((keys (get-trial-keys (build-frequencies num-list))))
	(loop for key in keys 
		 collect (cons key (numlist-to-string (xor-with key num-list))))))

(defun test-keys (num-list)
  "Outputs keys derived from frequency analysis. Tests keys assuming common space, e, t n." 
  (let ((keys (get-trial-keys (build-frequencies num-list))))
	(loop for key in keys
		 for freqs = (map 'list #'car 
						  (subseq (build-frequencies 
										(xor-with key num-list)) 0 10))
		 for score-common-letters = (loop for code in '(32 101 116 110)
									  sum (count code freqs))
		 when (> score-common-letters 2) do (return key))))

(defun get-plain-numlist (keys num-list)
  "decrypts a numlist using a set of numeric keys"
  (let ((key-sequence 
		 (loop repeat (+ 1 (floor (length num-list) (length keys)))
			  append keys)))
 (loop for num in num-list
	   for key in key-sequence
	   collect (logxor num key))))

(defun get-plain-text (keys numlist)
  "outputs plaintext as text"
  (numlist-to-string (get-plain-numlist keys numlist)))
  
(defun get-keys (n encrypted-num-list)
  "gets a set of keys using frequency analysis"
  (let ((splits (loop for i from 0 to (- n 1)
					 collect (take-by-three i encrypted-num-list))))
	(loop for s in splits
		 collect (test-keys s))))

(defun euler59 ()
  (reduce '+ (get-plain-numlist (get-keys 3 cipher-num-list) cipher-num-list)))
	   



