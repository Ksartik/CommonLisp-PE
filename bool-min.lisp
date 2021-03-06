(defun lsb-dec2bin (n nterms)
  (multiple-value-bind (q r) (truncate n 2)
    (cond ((= nterms 1) (cons r nil))
	  ((= q 0) (if (= nterms 1)
		       (cons r nil)
		       (append (list r) (loop for i from 1 to (- nterms 1)
					   collect 0))))
	  (t (cons r (lsb-dec2bin q (- nterms 1)))))))
    (if (= q 0)
	(if (= nterms 1)
	    (cons r nil)
	    (append (list r) (loop for i from 1 to (- nterms 1)
			   collect 0)))
	(cons r (lsb-dec2bin q (- nterms 1))))

(defun implicant-join (p1 p2)
  (let ((n (length p1))
	(lp nil)
	(c 0))
    (if (= n (length p2))
	(do ((lp1 p1 (cdr lp1))
	     (lp2 p2 (cdr lp2)))
	    ((null lp1) (nreverse lp))
	  (let ((x (car lp1))
		(y (car lp2)))
	    (if (eq x y)
		(push x lp)
		(if (>= c 1)
		    (return-from implicant-join nil)
		    (progn (push 'x lp) (incf c))))))
	nil)))

(defun minimize-exp (nvars minterms dont-cares)
  (let ((bmin-terms (mapcar #'(lambda(x) (lsb-dec2bin x nvars))
			    minterms))
	(dc-terms (mapcar #'(lambda(x) (lsb-dec2bin x nvars))
			  dont-cares)))
    (minimize-bool bmin-terms dc-terms)))
