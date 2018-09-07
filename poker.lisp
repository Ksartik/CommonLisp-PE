(defun delimit (new-line &optional (ch #\ ))
  (let ((run-str nil)
	(lis nil))
    (do ((i 0 (+ i 1)))
	((= i (length new-line)))
      (if (char= (char new-line i) ch)
	  (progn (setf lis (append lis
				   (list (coerce run-str 'string))))
		 (setf run-str nil))
	  (setf run-str (append run-str (list (char new-line i))))))
    (if run-str
	(setf lis
	      (append lis
		      (list (coerce run-str 'string)))))
    lis))

(defmacro delimit (str &optional (ele #\ ))
  (with-gensyms (final-lis run-eles)
    `(let ((,final-lis nil)
	   (,run-eles nil))
       ,@(loop for x across str
	    collect (if (equal x ele)
			`(progn (push (coerce ,run-eles 'string) ,final-lis)
				(setf ,run-eles nil))
			`(setf ,run-eles
			       (append ,run-eles '(,x)))))
       (if ,run-eles
	   (push (coerce ,run-eles 'string) ,final-lis))
       (nreverse ,final-lis))))

(defmacro sublist-p (lis1 lis2)
  (with-gensyms (n1 n2)
    `(let ((,n1 (length ,lis1))
	   (,n2 (length ,lis2)))
       (do ((i 0 (1+ i )))
	   ((= i (- ,n1 ,n2)))
	 (

(defun card-suit (c)
  (char c 1))
(defun card-value (c)
  (char c 0))

(defvar cards-rank '((#\2 0) (#\3 1) (#\4 2) (#\5 3) (#\6 4) (#\7 5)
		     (#\8 6) (#\9 7) (#\T 8) (#\J 9) (#\Q 10) (#\K 11)
		     (#\A 12)))
(defun rank-card (c)
  (cadr (assoc (card-value c) cards-rank :test 'char=)))

(defun royal-flush-p (h)
  (and (flush-p h)
	(let ((cvalues (mapcar #'card-value h)))
	  (subsetp '(#\T #\J #\Q #\K #\A) cvalues)))))

(defun straight-flush-p (h)
  (and (flush-p h)
       (straight-p h)))

(defun count-card-values (h)
  (let ((cv (mapcar #'card-value h)))
    (mapcar #'(lambda(x) (count x cv)) (mapcar #'car cards-rank))))
   
(defun four-of-a-kind-p (h)
  (position 4 (count-card-values h)))

(defun full-house-p (h)
  (let ((ccv (count-card-values h)))
    (let ((p2 (position 2 ccv))
	  (p3 (position 3 ccv)))
      (if (and p2 p3)
	  p3))))

(defun flush-p (h)
  (let ((s1 (card-suit (car h))))
    (every #'(lambda(x) (equal (card-suit x) s1))
	   (cdr h))))

(defun straight-p (h)
  (let ((cvalues (mapcar #'card-value h))
	(cards-vals (mapcar #'car cards-rank)))
    (do ((i 0 (1+ i)))
	((= i 9) nil)
      (if (subsetp (subseq cards-vals i (+ i 5))
		   cvalues :test 'equal)
	  (return-from straight-p i)))))

(defun three-of-a-kind-p (h)
  (position 3 (count-card-values h)))

(defun two-pairs-p (h)
  (let ((ccv (count-card-values h)))
    (if (= (count 2 ccv) 2)
	(let ((p (position 2 ccv)))
	  (+ p
	     (position 2 (subseq ccv (+ p 1))))))))

(defun one-pair-p (h)
  (position 2 (count-card-values h)))

(defun high-card (h)
  (apply #'max (mapcar #'rank-card h)))

(defun rank-hand (h)
  (cond ((royal-flush-p h) 105)
	((straight-flush-p h) (+ (straight-flush-p h) 92))
	((four-of-a-kind-p h) (+ (four-of-a-kind-p h) 79))
	((full-house-p h)(+ (full-house-p h) 66))
	((flush-p h) 65)
	((straight-p h) (+ (straight-p h) 52))
	((three-of-a-kind-p h) (+ (three-of-a-kind-p h) 39))
	((two-pairs-p h) (+ (two-pairs-p h) 26))
	((one-pair-p h)  (+ (one-pair-p h) 13))
	(t (high-card h))))

(defun rank-hand (h)
  (cond-value ((royal-flush-p h) 105)
	      ((straight-flush-p h) => #'(lambda(x) (+ x 92)))
	      ((four-of-a-kind-p h) => #'(lambda(x) (+ x 79)))
	      ((full-house-p h) => #'(lambda(x) (+ x 66)))
	      ((flush-p h) 65)
	      ((straight-p h) => #'(lambda(x) (+ x 52)))
	      ((three-of-a-kind-p h) => #'(lambda(x) (+ x 39)))
	      ((two-pairs-p h) => #'(lambda(x) (+ x 26)))
	      ((one-pair-p h) => #'(lambda(x) (+ x 13)))
	      (t (high-card h))))

(defun high-compare (h1 h2)
  (let ((hh1 (high-card h1))
	(hh2 (high-card h2)))
    (cond ((> hh1 hh2) 1)
	  ((< hh1 hh2) 2)
	  (t
	   (let ((hc (car (nth hh1 cards-rank))))
	     (high-compare (remove hc h1 :key #'card-value)
			   (remove hc h2 :key #'card-value)))))))

(defun read-poker-line (str)
  (let ((de-str (delimit str)))
    (rank-compare (subseq de-str 0 5) 
		  (subseq de-str 5)))))

(defvar a (make-array 3))
(with-open-file (in "\poker.txt")
  (do ((s (read-line in nil nil) (read-line in nil nil)))
      ((null s))
    (let ((sr (delimit (subseq s 0 29))))
      (incf (aref a (rank-compare (subseq sr 0 5)
				  (subseq sr 5)))))))

(defun rank-compare (h1 h2)
  (let ((r1 (rank-hand h1))
	(r2 (rank-hand h2)))
    (cond ((> r1 r2) 1)
	  ((< r1 r2) 2)
	  (t (high-compare h1 h2)))))


(defmacro cond-value (&rest clauses)
  `(cond
     ,@(loop for c in clauses
	       for p = (car c)
	       if (eq (cadr c) '=>)
	       collect `((not (null ,p)) (funcall ,(caddr c) ,p))
	       else
	  collect `((not (null ,p)) ,@(cdr c)))))

(defmacro cond-value (&rest clauses)
  (with-gensyms (p)
    `(,@(loop for c in clauses
	   if (eq (cadr c) '=>)
	   collect `(let ((,p ,(car c)))
		      (if ,p
			  (return-from cond-value (funcall ,(caddr c) ,p))))
	   else
	   collect `(if ,(car c)
			(return-from cond-value (progn ,(cdr c))))))))
