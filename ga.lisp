(defun evolve (population fitness mate mutate mutation-rate)
  (let* ((new-population)
	 (pop-type (type-of population))
	 (fitnes-popu (map 'cons  #'(lambda(x) (funcall fitness x)) population))
	 (mating-pool (map 'cons  #'list population (cummulativeD fitness-popu)))
	 (total-fitness (apply #'+ fitness-popu)))
    (do ((i 0 (+ i 1)))
	((= i (length population)))
      (let ((child (apply mate (couple mating-pool (+ 1 total-fitness))))
	    (r (random 1.0)))
	(if (< r mutation-rate)
	    (setf child (funcall mutate child)))
	(push child new-population)))
    (coerce new-population pop-type)))

(defun evolve-generate (&key (gens 0) population fitness mate mutate mutation-rate)
  (let (pops)
    (do ((i 0 (+ i 1)))
	((= i gens))
      (setf pops (append pops (list (sort (mapcar #'(lambda(x)
						 (list x (funcall fitness x)))
					     population)
					  #'> :key #'cadr))))
      (setf population (evolve population fitness mate mutate mutation-rate)))
    pops))

(defun cummulativeD (l)
  (let ((run-sum 0))
    (mapcar #'(lambda(x)
		(setf run-sum (+ run-sum x))
		run-sum)
	    l)))
		     
(defun couple (pool total)
  (do ((x (random-parent pool total) (random-parent pool total))
       (y (random-parent pool total) (random-parent pool total)))
      ((not (eq x y)) 
       (list x y))))

(defun random-parent (pool total)
  (let ((r (random total)))
      ;((r (random (coerce total 'float))))
    (car (find-if #'(lambda(x) (>= (cadr x) r)) pool))))
 

;string making problem

(defun char-list (start end)
	   (if (> start end)
	       nil
	       (cons (code-char start) (char-list (+ start 1) end))))

(defvar eng-chars (append (char-list 65 (+ 65 25))
			  (char-list 97 (+ 97 25))
			  '(#\space #\, #\. #\" #\' #\( #\))))

(defun rand-ele (lis)
  (nth (random (length lis)) lis))

(defun rand-string (lis n) ; n is length
  (if (= n 0)
      nil
      (concatenate 'string (string (rand-ele lis)) (rand-string lis (- n 1)))))

(defvar target "To be or not to be")

(defun fitness1 (s)
  (let ((n 0))
    (do ((i 0 (+ i 1)))
	((= i (length s)))
      (if (char= (char s i) (char target i))
	  (incf n)))
    n))

(defun fitness2 (s)
  (let ((f 0))
    (do ((i 0 (+ i 1)))
	((= i (length s)))
      (incf f
	    (/ 1.0 (+ 1
		      (abs (- (char-code (char s i))
			      (char-code (char target i))))))))
    f))

(defun mutate (s)
  (setf (char s (random (length s))) (rand-ele eng-chars))
  s)

(defun mate (s1 s2)
  (concatenate 'string s1 s2))
