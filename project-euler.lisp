(defun enumerate-interval (lower upper &optional (inc 1))
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval (+ lower inc) upper inc))))

(defun sieve (upper)
  (let ((l (enumerate-interval 2 upper)))
    (labels ((f (x)
	       (if (null x)
		   nil
		   (cons (car x)
			 (f (remove-if
			     #'(lambda(y) (= (rem y (car x)) 0))
			     x))))))
      (f l))))

(defun primep (n)
  (when (> n 1)
    (loop for i from 2 to (isqrt n)
       never (zerop (mod n i)))))

(defun next-prime (n)
  (loop for i from (+ n 1) when (primep i)
     return i))

(defun nth-prime (n)
  (do ((i 2 (1+ i)))
      ((= n 0) (- i 1))
    (if (primep i) (decf n))))

(defun prime-list (n)
  (do ((i 2 (+ i 1))
       (lis nil))
      ((= n 0) (nreverse lis))
    (if (primep i)
	(progn (decf n) (push i lis)))))

(defun crt-x (prem x m p)
  (do ((i 0 (+ i 1)))
      ((= (rem (+ x (* m i)) p) prem) (+ x (* m i)))))

(defun A (n)
  (let ((m 1)
	(x 0)
	(r 1))
    (dolist (p (prime-list n))
      (setf x (crt-x r x m p))
      (setf m (* m p))
      (incf r))
    x))

(defun A-clever (n)
  (if (= n 2)
      5
      (crt-x n (A-clever (- n 1))
	     (apply #'* (prime-list n))
	     (car (reverse
		   (prime-list n))))))

(defun S (n)
  (let ((
	 (dolist
	     )))))

;;;;

(defun su (n)
  (+ 1 (* 4 (loop for i from 1 to (/ (- n 1) 2) 
	       summing (- (expt (+ (* 2 i) 1)
				2)
			  (* 3 i))))))


;;
(defun digits (n)
  (let ((q nil)
	(r nil))
    (setf (values q r) (truncate n 10))
    (if (= q 0)
	(cons r nil)
	(cons r (digits q)))))

(defun digit-cancelling ()
  (let ((lis nil))
    (do ((numer 11 (1+ numer)))
	((> numer 99))
      (do ((denom (1+ numer) (1+ denom)))
	  ((> denom 99))
	(if (digit-cancelling-p numer denom)
	    (push `(,numer ,denom) lis))))
    lis))

(defun digit-cancelling-p (numer denom)
  (if (and (not (zerop
		 (rem numer 10)))
	   (not (zerop
		 (rem denom 10))))
      (let* ((n-digits
	      (digits numer))
	     (d-digits
	      (digits denom))
	     (ints (intersection n-digits d-digits)))
	(if ints
	    (let ((nf (set-difference n-digits ints))
		  (df (set-difference d-digits ints)))
	      (if (and nf
		       df
		       (= (/ numer denom)
			  (/ (first nf)
			     (first df))))
		  t))))))

;;

(defun 1to9-pandigital-p (n)
  (let ((x (loop for i from 1 to 2  collecting (* n i))))
    (if (equal
	 (sort (apply #'append (mapcar #'digits x))
	       #'<)
	 '(1 2 3 4 5 6 7 8 9))
	x)))

;;

(defun square-p (n)
  (let ((s (sqrt n)))
    (if (= (floor s) s)
	s)))

(defun pentagonal-p (x)
  (let ((s (square-p (+ (* 24 x) 1))))
    (if s
	(= (rem (+ s 1) 6) 0))))

(defun pentagonal (n)
  (/ (* n (- (* 3 n) 1)) 2))

(defun sum-penta-p (j k)
  (pentagonal-p
   (+ (pentagonal j)
      (pentagonal k))))

(defun diff-penta-p (j k)
  (pentagonal-p
   (abs (- (pentagonal j)
	   (pentagonal k)))))

(defun pcheck (N)
  (let ((l nil))
    (do ((k 1 (1+ k)))
	((> k N))
      (do ((j (+ k 1) (1+ j)))
	  ((> j N))
	(if (and (sum-penta-p j k)
		 (diff-penta-p j k))
					;(format t "~a ~a~%" k j)))))
	    (push (- (pentagonal j)
		     (pentagonal k))
		  l))))
    (if l
	(apply #'min l))))

;;

(defun digits->num (dl)
  (let ((r (reverse dl)))
    (labels ((d->n (l)
	       (if (null l)
		   0
		   (+ (car l) (* 10 (d->n (cdr l)))))))
      (d->n r))))

(defun perms-num (n)
  (let ((q nil)
	(r nil))
    (setf (values q r) (truncate n 10))
    (if (= q 0)
	(list (list r))
	(let ((s (perms-num q)))
	  ;(mapcar #'digits->num
		  (remove-duplicates 
		   (apply #'append
			  (mapcar #'(lambda(x) (in-list x r))
				  s))
		   :test #'equal)))))

(defun num-perms (n)
  (mapcar #'digits->num (perms-num n)))

(defun in-list (l n)
  (do ((i 0 (1+ i))
       (lis nil))
      ((> i (length l)) (nreverse lis))
    (push (append (subseq l 0 i)
		  (list n)
		  (subseq l i))
	  lis)))

(defun diff-n (l)
  (do ((i 0 (1+ i))
       (lis nil))
      ((= i (length l)) (nreverse lis))
    (push (mapcar #'(lambda(x) (- (nth i l) x))
		  (subseq l 0 i))
	  lis)))

(defun ap-subseq (l)  
  (do ((i 0 (1+ i)))
      ((= i (length l)))
    (do ((j (1+ i) (1+ j)))
	((= j (length l)))
      (do ((k (1+ j) (1+ k)))
	  ((= k (length l)))
	(if (= (+ (nth i l) (nth k l)) (* 2 (nth j l)))
	    (return-from ap-subseq (list
				    (nth i l)
				    (nth j l)
				    (nth k l))))))))

;;

(defun k-prime-factors (n k &optional (sie (sieve n)))
  (= (loop for x in sie
	counting (= (rem n x) 0))
     k))

(defun consec-pf (i k m)
  (let* ((upper (+ i (- m 1)))
	 (sie (sieve upper)))
    (every #'identity
	   (mapcar #'(lambda(x) (k-prime-factors x k sie))
		   (enumerate-interval i upper)))))

(defun consec-k-prime-factors (m k)
  (do ((i 1))
      ((every #'identity
	      (mapcar #'(lambda(x) (k-prime-factors x k))
		      (enumerate-interval i (+ i (- m 1)))))
       i)
    (incf i)))

(let ((i 1))
  (loop while (not (every #'identity
	      (mapcar #'(lambda(x) (k-prime-factors x k))
		      (enumerate-interval i (+ i (- m 1))))))
       (incf i))
  i)

(defun sieve-k (upper)
  (let ((a (make-array (+ upper 1)))
	(l (enumerate-interval 2 upper)))
    (let ((s l))
      (dolist (x l)
	(if (zerop (aref a x))
	    (dolist (y (cdr s))
	      (if (= (rem y x) 0)
		  (incf (aref a y)))))
	(setf s (cdr s))))
    a))

(defun dist-pf (n &optional (k 2))
  (cond ((= n 1) 0)
	((= (rem n k) 0)
	 (loop until (not (= (rem n k) 0))
	    do
	      (setf n (/ n k)))
	 (+ 1 (dist-pf n (+ k 1))))
	(t (dist-pf n (+ k 1)))))

(defun consec-dist-pf (n m k)
  (if (= m 0)
      t
      (and (= (dist-pf n) k)
	   (consec-dist-pf (+ n 1) (- m 1) k))))
;;

(defun anagram-p (a &rest br)
  (let ((da (sort (digits a) #'<)))
    (every #'(lambda(x)
	       (equal x da))
	   (mapcar #'(lambda(x) (sort
				 (digits x)
				 #'<))
		   br))))

(defun multiple-anagram-p (x k)
  (labels ((multiples (a upper)
	     (loop for i from 2 to upper
		collecting
		  (* a i))))
    (apply #'anagram-p x
	   (multiples x k))))

;;
(defun ndigits (n)
  (let ((q nil)
	(r nil))
    (setf (values q r) (truncate n 10))
    (if (= q 0)
	1
	(+ 1 (ndigits q)))))

(defun continued-fraction (partial-numer partial-denom terms &optional (i 0))
  (if (= terms 0)
      (funcall partial-denom i)
      (+ (funcall partial-denom i) (/ (funcall partial-numer (+ i 1))
				      (continued-fraction partial-numer
							  partial-denom
							  (- terms 1)
							  (+ i 1))))))

(defun /2-numer (i)
  1)

(defun /2-denom (i)
  (if (= i 0)
      1
      2))

;;

(defvar dp-array (make-array 100000))
(defun sdiag-prime (i)
  (let ((a (expt i 2))
	(s 0))
      (if (primep (+ a (+ i 1)))
	  (incf s))
      (if (primep (+ a (* (+ i 1) 2)))
	  (incf s))
      (if (primep (+ a (* (+ i 1) 3)))
	  (incf s))
      s))

(do ((i 1 (1+ i))
	      (j 1 (+ 2 j)))
	     ((= i 100000))
	   (setf (aref dp-array i) (+ (aref dp-array (- i 1)) (sdiag-prime j)))
	   (if (< (/ (aref dp-array i) (+ (* 4 i) 1)) 0.1)
	       (return (+ (* 2 i) 1))))

;;

(defun xor-crypt (lis pass)
  (let ((curr-pass pass))
    (mapcar #'(lambda(x) (let ((cpass (car curr-pass)))
			   (if (not (null curr-pass))
			     (progn (setf curr-pass (cdr curr-pass))
				    (logxor x cpass))
			     (progn (setf curr-pass (cdr pass))
				    (logxor x (car pass))))))
	    lis)))

(do ((i 97 (1+ i))
     (s nil))
    ((> i 122) s)
  (do ((j 97 (1+ j)))
      ((> j 122))
    (do ((k 97 (1+ k)))
	((> k 122))
      (if (every #'(lambda(x) (or (and (>= x 65) (<= x 90))
				  (and (>= x 97) (<= x 122))))
		 (xor-crypt enc (list i j k)))
	  (push (list i j k) s)))))

(loop for i from 97 to 122
	      for j from 97 to 122
	      for k from 97 to 122
	      if (every #'(lambda(x) (or (and (>= x 65) (<= x 90))
					 (= x 32) (= x 33) (= x 44)
					 (and (>= x 97) (<= x 122))))
		(xor-crypt enc (list i j k)))
   collect (list i j k))

;;

(defun digit-count (n)
  (let ((q nil) (r nil))
    (setf (values q r) (truncate n 10))
    (if (= q 0)
	1
	(1+ (digit-count q)))))

(defun dcn-p (a n)
  (= (digit-count (expt a n)) n ))

;;

(defun seq-repeat (s)
  (let ((n (length s)))
    (if (evenp n)
	(let ((mid (/ n 2)))
	  (dotimes (i mid)
	    (if (not (= (aref s i) (aref s (+ mid i))))
		(return-from seq-repeat nil)))
	  mid))))

(defun root-period (x)
  (let ((n 1)
	(seq (make-array 1 :adjustable t
			 :initial-contents (list (floor x))))
	(temp 1)
	(m x))
    (loop
       (if (evenp temp)
	   (progn (setf (aref seq n) (floor m))
		  (setf m (/ 1 (- a (floor m)))))
	   (let ((seq-rep (seq-repeat seq)))
	     (if seq-rep
		 (return-from root-period seq-rep)
		 (progn (setf (aref seq n) (floor m))
			(setf m (/ 1 (- a (floor m))))))))
       (incf n)
       (incf temp))))

(defun cont-frac (x N)
  (let ((seq (make-array (+ N 1)))
	(m x))
    (dotimes (n N)
      (setf (aref seq n) (floor m))
      (setf m (/ 1 (- m (floor m)))))
    seq))

(defun con-frac (x &optional (ms nil))
  (if (member (floor (* 100 x)) ms)
      nil
      (let ((f (floor x)))
	(cons f (con-frac (coerce (/ 1 (- x f)) 'long-float)
			  (push (floor (* 100 x)) ms))))))

(defun cont-frac (x)
  (let ((cfx (con-frac x)))
    (list (car cfx) (cdr cfx))))

(defun period-cf (x)
  (length (cadr (cont-frac x))))

;;

(defun e-cf (n)
  (continued-fraction #'(lambda(n) 1)
		      #'(lambda(n) 
			  (cond ((= n 0) 2)
				((= (rem (- n 2) 3) 0)
				 (+ (* (/ 2 3) (- n 2)) 2))
				(t 1)))
		      (- n 1)))

;;

(defun maxs-ending-at (a i j)
  (cond ((= i 0) (aref a i j))
	((= j 0) (loop for k from 0 to i
		    summing (aref a k j)))
	((= j i) (loop for k from 0 to i
		    summing (aref a k k)))
	(t (+ (aref a i j) 
	      (max (maxs-ending-at a (- i 1) j)
		   (maxs-ending-at a (- i 1) (- j 1)))))))

(defun max-sum-down (a)
  (let ((r (array-dimension a 0))
	(c (array-dimension a 1)))
    (loop for j from 0 to (- c 1)
       maximizing (maxs-ending-at a (- r 1) j))))

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

(defun triangle-input (filename)
  (let ((a (make-array '(100 100))))
    (with-open-file (in filename)
      (do ((s (read-line in nil nil) (read-line in nil nil))
	   (i 0 (1+ i)))
	  ((null s) a)
	(let ((ls (mapcar #'parse-integer (delimit s)))
	      (j 0))
	  (dolist (x ls)
	    (setf (aref a i j) x)
	    (incf j)))))))

(defun max-sum-down (a)  ; moving up !
  (let ((r (- (array-dimension a 0) 1)))
    (do ((i r (- i 1)))
	((= i 0) (aref a 0 0))
      (dotimes (j i)
	(incf (aref a (- i 1) j)
	      (max (aref a i j) (aref a i (+ j 1))))))))

;;

(defun gcd-sum (N)
  (loop for i from 1 to N
     summing (gcd i N)))

(defvar G-gcd (make-array (expt 10 6)))

(loop for i from 1 to 10
     (setf (aref G-gcd i)
	   (+ (aref G-gcd (- i 1))
	      (gcd-sum i))))
;;
(defun euler-phi (n)
  (do ((i 2 (1+ i))
       (m n))
      ((> i n) m)
    (if (= (mod n i) 0)
	(setf m (* m (/ (- i 1) i))))))

(defun euler-phi (n)
  (let ((m n))
    (dolist (p (sieve n))
      (if (= (mod n p) 0)
	  (setf m (* m (/ (- p 1) p)))))
    m))

(defun euler-phi (n &optional (p 2) (m n))
  (cond ((= n 1) m)
	((= (mod n p) 0)
	 (loop until (not (= (mod n p) 0))
	      do
	      (setf n (/ n p)))
	 (euler-phi n (+ p 1) (* m (/ (- p 1) p))))
	(t (euler-phi n (+ p 1) m))))

(defun totient-perm-p (n)
  (equal (sort (digits (euler-phi n)) #'<)
	 (sort (digits n) #'<)))


(defun perms-p (n1 n2)
  (let ((a1 (make-array 10))
	(a2 (make-array 10)))
    (loop until (= n1 0)
       do
	 (incf (aref a1 (mod n1 10)))
	 (incf (aref a2 (mod n2 10)))
	 (setf n1 (truncate n1 10))
	 (setf n2 (truncate n2 10)))
    (dotimes (i 10)
      (if (not (= (aref a1 i)
		  (aref a2 i)))
	  (return-from perms-p nil)))
    t))

(defun phi-array (n)
  (let ((euler-phi (make-array (+ n 1))))
    (setf (aref euler-phi 1) 1)
    (loop for i from 2 to n
       if (primep i)
       do
	 (do ((j i (* j i)))
	     ((> j n))
	   (setf (aref euler-phi j) (* (/ j i) (- i 1)))))
    (loop for i from 2 to n
       if (zerop (aref euler-phi i))
       do
	 (do  ((j 2 (1+ j)))
	      ((and (= (mod i j) 0) (= (gcd j (/ i j)) 1))
	       (setf (aref euler-phi i) (* (aref euler-phi j)
					   (aref euler-phi (/ i j)))))))
    euler-phi))

(defun totient-perms (N)
  (let ((a-phi (phi-array N)))
    (do ((i 2 (1+ i))
	 (k 0) (m 4))
	((> i N) (list k (float m)))
      (let ((phi-i (aref a-phi i)))
	(if (perms-p i phi-i)
	    (if (< (/ i phi-i) m)
		(progn (setf m (/ i phi-i)) (setf k i))))))))
;     (loop for i from 100000 to N
;	 if (perms-p i (aref a-phi i))
;        collect (list i (float (/ i (aref a-phi i)))))))

;;

(defmacro magic-3gon-p (outer inner) ;cyclic arrays of length 3
  `(= ,@(loop for i from 0 to 2
	   collect
	     `(+ (aref ,outer ,i) (aref ,inner ,i)
		 (aref ,inner (mod (+ ,i 1) 3))))))

(defun 3gon->string (outer inner)
  (let ((min-i 0)
	(m (aref outer 0))
	(n 0))
    (do ((i 1 (1+ i)))
	((= i 3))
      (if (< (aref outer i) m)
	  (progn (setf m (aref outer i))
		 (setf min-i i))))
    (do ((j min-i (mod (+ j 1) 3))
	 (k 0 (1+ k)))
	((= k 3))
      (setf n (+ (* 10 n) (aref outer j)))
      (setf n (+ (* 10 n) (aref inner j)))
      (setf n (+ (* 10 n) (aref inner (mod (+ j 1) 3)))))
    n))

(defun perms (l)
  (if (null (cdr l))
      `((,(car l)))
      (apply #'append
	     (mapcar #'(lambda(x)
			 (put-at-all-pos x (car l)))
		     (perms (cdr l))))))

(dolist (x (perms '(1 2 3 4 5 6)))
  (let ((outer (make-array 3 :initial-contents (subseq x 0 3)))
	(inner (make-array 3 :initial-contents (subseq x 3))))
    (if (magic-3gon-p outer inner)
	(format t "~a~%" (3gon->string outer inner)))))

(defun put-at-all-pos (l a &optional (l-prev nil))
  (if (null l)
      (cons (append l-prev `(,a)) nil)
      (cons (append l-prev `(,a) l)
	    (put-at-all-pos (cdr l) a (append l-prev `(,(car l)))))))


(defmacro magic-5gon-p (outer inner) ;cyclic arrays of length 5
  `(= ,@(loop for i from 0 to 4
	   collect
	     `(+ (aref ,outer ,i) (aref ,inner ,i)
		 (aref ,inner (mod (+ ,i 1) 5))))))

(defun 5gon->string (outer inner)
  (let ((min-i 0)
	(m (aref outer 0))
	(n ""))
    (do ((i 1 (1+ i)))
	((= i 5))
      (if (< (aref outer i) m)
	  (progn (setf m (aref outer i))
		 (setf min-i i))))
    (do ((j min-i (mod (+ j 1) 5))
	 (k 0 (1+ k)))
	((= k 5))
      (setf n (concatenate 'string n (write-to-string (aref outer j))))
      (setf n (concatenate 'string n (write-to-string (aref inner j))))
      (setf n (concatenate 'string n
			   (write-to-string (aref inner (mod (+ j 1) 5))))))
    n))
;;

(defun left-farey-nbr (order r first-nbr)
  (let ((next-nbr (/ (+ (numerator r) (numerator first-nbr))
		     (+ (denominator r) (denominator first-nbr)))))
    (if (> (denominator next-nbr) order)
	first-nbr
	(left-farey-nbr order r next-nbr))))

(defun farey-length (order)
  (let ((a (phi-array order)))
    (1- (loop for x across a
	   summing x))))

(defun farey-between (n r1 r2)
  (let ((rmid (/ (+ (numerator r1) (numerator r2))
		 (+ (denominator r1) (denominator r2)))))
    (if (> (denominator rmid) n)
	0
	(+ 1
	   (farey-between n r1 rmid)
	   (farey-between n rmid r2)))))

;;
(defun factorial (n)
  (cond ((< n 0) nil)
	((< n 2) 1)
	(t (* n (factorial (- n 1))))))


(defun digit-fact-sum (n)
  (multiple-value-bind
	(q r) (truncate n 10)
    (if (= q 0)
	(factorial r)
	(+ (factorial r)
	   (digit-fact-sum q)))))

(defun dfact-chain-len (n &optional
			    (lis nil))
  (if (member n lis)
      0
      (1+ (dfact-chain-len
	   (digit-fact-sum n)
	   (push n lis)))))

;;
(defun form-tria-p (p q r)
  (
(defun factors (n)
  (loop for i from 1 to n
     if (zerop (mod n i))
     collect i))

(defun factorize (n)
  (if (= n 1)
      (cons 1 nil)
      (do ((i 2 (1+ i)))
	  ((= (mod n i) 0) (cons i (factorize (/ n i)))))))


;;
(defun sum-n-by-max (n m) ; a1 + a2 + ... + ak = n : a1 <= a2 <= ... <= ak = m
  (if (= m 1)
      1
      (labels ((rec-sum (a b)
		 (cond ((< a 0) 0)
		       ((= a 0) 1)
		       (t (loop for i from 1 to b
			     summing (rec-sum (- a i) i))))))
	(rec-sum (- n m) m))))

(defun partitions (n)
  (loop for i from 1 to n
     summing (sum-n-by-max n i)))

;;
(defun psum-n-by-max (n m) ; a1 + a2 + ... + ak = n : a1 <= a2 <= ... <= ak = m
  (labels ((rec-sum (a b)
	     (cond ((< a 0) 0)
		   ((= a 0) 1)
		   (t (loop for i from 1 to b
			 if (primep i)
			 summing (rec-sum (- a i) i))))))
    (rec-sum (- n m) m)))

(defun ppartitions (n)
  (loop for i from 1 to (- n 1)
       if (primep i)
	   summing (psum-n-by-max n i))))

;;
(defvar l '(7 3 1 6 2 8 9 0))
(defun read-file-as-list (filename)
  (with-open-file (in filename)
    (do ((s (read-line in nil nil)))
	((null s))
      (let ((dig (nreverse (digits (parse-integer s :junk-allowed t)))))
	(if (not (< (position (first dig) l)
		    (position (second dig) l)
		    (position (third dig) l)))
	    (return-from read-file-as-list nil))))))

(defun digital-sum (n)
  (multiple-value-bind (q r) (truncate n 10)
    (if (= q 0)
	r
	(+ r (digital-sum q)))))

(defun sqrt-prec (n &optional (precision 1.0e-7) (prev-approx (isqrt n)))
  (let ((new-approx (/ (+ prev-approx  (/ n prev-approx)) 2)))
    (if (< (abs (- (expt prev-approx 2) n)) precision)
	prev-approx
	(sqrt-prec n precision new-approx))))

(defun sqrt-decimal-digits (n digits)
  (let* ((prev-approx (isqrt n))
	 (digs (- digits (1+ (floor (log prev-approx 10))))))
    (floor (* (expt 10 digs)
	      (sqrt-prec n (expt 10 (- digs)) prev-approx)))))

(defun sqrt-100-digital-sum (n)
  (digital-sum (sqrt-decimal-digits n 100)))


;;

(defun 3-right-factors (n)
  (remove-duplicates
   (loop for p from 1 to n
      if (zerop (mod n p))
      append
	(loop for q from 2 to (/ n p)
	   if (zerop (mod (/ n p) q))
	   append
	     (loop for r from (+ q 1) to (- (* 2 q) 1)
		if (= (* p q r) n)
					;collect `(,p ,q ,(- r q))))))
		collect (let ((n (- r q)))
			  (list (* p (- (expt q 2) (expt n 2)))
				(* p (+ (expt q 2) (expt n 2)))
				(* p 2 q n))))))
   :test #'subsetp))

(defun right-tria-len (L)
  (length (3-right-factors L)))

(defun pqr->kmn (p q r)
  (let ((n (- r q)))
    (list
     (* p (- (expt q 2) (expt n 2)))
     (* p (+ (expt q 2) (expt n 2)))
     (* p 2 q n))))

(defun 1-right-tria-p (n)
  (let ((s 0)
	(l nil))
    (loop for p from 1 to n
       if (zerop (mod n p))
       append
	 (loop for q from 2 to (/ n p)
	    if (zerop (mod (/ n p) q))
	    append
	      (loop for r from (+ q 1) to (- (* 2 q) 1)
		 if (and (= (* p q r) n) (not (subsetp (pqr->kmn p q r) l)))
		 do
		   (if (= s 1)
		       (return-from 1-right-tria-p nil)
		       (progn (incf s) (setf l (pqr->kmn p q r)))))))
    (= s 1)))

(defun right-tria-sum-sieve (k)
  (let ((1lis nil)
	(g1lis nil)
	(s 0))
    (do* ((m 2 (1+ m))
	 (y (* 2 m (+ m 1)) (* 2 m (+ m 1))))
	((> y k) (progn (loop for x in 1lis
			   if (< x y)
			   do
			     (incf s))
			s))
	(setf 1lis (delete-if #'(lambda(x) (and (< x y) (incf s))) 1lis))
	(do ((n 1 (1+ n)))
	    ((= m n))
	  (let ((z (* 2 m n (+ m n))))
	    (if (not (find z g1lis))
		(if (find z 1lis)
		    (setf 1lis (delete z 1lis))
		    (push (* 2 m n (+ m n)) 1lis))))))))

;;

(defun min-sum-tl-br (a)
  (let ((r (array-dimension a 0))
	(c (array-dimension a 1)))
    (loop for j from 1 to (- c 1)
       do
	 (incf (aref a 0 j) (aref a 0 (- j 1))))
    (loop for i from 1 to (- r 1)
       do
	 (incf (aref a i 0) (aref a (- i 1) 0)))
    (do ((i 1 (1+ i)))
	((= i r))
      (do ((j 1 (1+ j)))
	  ((= j c))
	(incf (aref a i j) (min (aref a (- i 1) j)
				(aref a i (- j 1))))))
    (aref a (- r 1) (- c 1))))

;;

(defun 1-right-tria-sum (L)
  (let ((h (make-hash-table)))
    (do ((m 2 (1+ m)))
	((> (* 2 m (+ m 1)) L)
	 (loop for x being the hash-keys in h using (hash-value v)
	    count (listp v)));collect `(,x ,v)))
      (do ((n 1 (1+ n)))
	  ((= n m))
	(let ((z (* 2 m (+ m n))))
	  (do* ((k 1 (1+ k))
		(u (* k z) (* k z)))
	      ((> u L))
	    (if (null (gethash u h))
		(setf (gethash u h) `(,m ,n))
		(let ((v (gethash u h)))
		  (if (and (not (= v 0))
			   (not (= (+ (expt (car v) 2) (expt (cadr v) 2))
				   (+ (expt m 2) (expt n 2)))))
		      (setf (gethash u h) 0))))))))))

;;

(defun prime-sieve (N)
  (let ((lis (loop for i from 2 to N
		  collect i)))
    (maplist #'(lambda(x)
		 (setf lis (delete-if #'(lambda(y) (= (mod y (car x)) 0))
				      (cdr x)))
		 (car x))
	     lis)))

(defun PS (N)
  (let ((ps (prime-sieve N)))
    (mapcan #'(lambda(x)
		(mapcar #'(lambda(y) (+ x y))
			(mapcan #'(lambda(x) (mapcar #'(lambda(y) (+ x y))
						     (mapcar #'(lambda(x) (expt x 4)) ps)))
				(mapcar #'(lambda(x) (expt x 3)) ps))))
	    (mapcar #'(lambda(x) (expt x 2)) ps))))


;;

(defun exp-comp (b1 e1 b2 e2)
	   (> (* e1 (log b1)) 
	      (* e2 (log b2))))

(let ((bmax 1) (emax 1) (kmax 0))
	   (with-open-file (in "/home/progs/base_exp.txt")
	     (do ((s (read-line in nil nil) (read-line in nil nil))
		  (k 1 (1+ k)))
		 ((null s) kmax)
	       (let ((be (mapcar #'parse-integer (delimit s #\,))))
		 (if (exp-comp (car be) (cadr be) bmax emax)
		     (setf bmax (car be) emax (cadr be)
			   kmax k))))))

;;

(defun min-sum-lr (a)
  (let ((r (array-dimension a 0))
	(c (array-dimension a 1))
	(curr-min-row 0))
    (do ((i 1 (1+ i))
	 (m (aref a 0 0)))
	((= i r))
      (if (< (aref a i 0) m)
	  (setf curr-min-row i m (aref a i 0))))
    (do ((j 1 (1+ j)))
	((= j c))
      (let ((m 0))
	(
	(do ((i curr-min-row (- i 1)))
	    ((< i 0))
	  (setf (aref a i j) (+
			      ))))))))

(defun min-sum-cc (a)
  (let ((r (array-dimension a 0))
	(c (array-dimension a 1)))
    (let ((tempa (make-array r)))
      (do ((j 1 (1+ j)))
	  ((= j c))
	(do ((i 0 (1+ i)))
	    ((= i r))
	  (let ((s 0)
		(m (+ (aref a i j) (aref a i (- j 1)))))
	    (do ((k i (1- k)))
		((= k 0))
	      (incf s (aref a k j))
	      (let ((msum (+ s (aref a k (- j 1)))))
		(if (> m msum)
		    (setf m msum))))
	    (setf s 0)
	    (do ((k i (1+ k)))
		((= k r))
	      (incf s (aref a k j))
	      (let ((msum (+ s (aref a k (- j 1)))))
		(if (> m msum)
		    (setf m msum))))
	    (setf (aref tempa i) m)))
	(loop for i from 0 to (- r 1)
	   do
	     (setf (aref a i j) (aref tempa i)))))))

(defun min-sum-lc-rc (a)
  (let ((r (array-dimension a 0))
	(c (array-dimension a 1)))
    (let ((tempa (make-array r)))
      (do ((j 1 (1+ j)))
	  ((= j c))
	(loop for i from 0 to (- r 1)
	   do
	     (setf (aref tempa i) (bator a i j)))
	(loop for i from 0 to (- r 1)
	   do
	     (setf (aref a i j) (aref tempa i))))
      (loop for i from 0 to (- r 1)
	 minimizing (aref a i (- c 1))))))

(defun bator (a i j)
  (let ((r (array-dimension a 0))
	(s 0)
	(m (+ (aref a i j) (aref a i (- j 1)))))
    (do ((k i (1- k)))
	((< k 0))
      (incf s (aref a k j))
      (let ((msum (+ s (aref a k (- j 1)))))
	(if (> m msum)
	    (setf m msum))))
    (setf s 0)
    (do ((k i (1+ k)))
	((= k r))
      (incf s (aref a k j))
      (let ((msum (+ s (aref a k (- j 1)))))
	(if (> m msum)
	    (setf m msum))))
    m))
;;
(defun sq-digit-sum (n)
  (multiple-value-bind (q r) (truncate n 10)
    (if (= q 0)
	(expt r 2)
	(+ (expt r 2) (sq-digit-sum q)))))

(defun sq-digit-chain (n)
  (if (or (= n 1) (= n 89))
      n
      (sq-digit-chain (sq-digit-sum n))))

;;
(defun cont-frac (x n)
  (if (= n 0)
      (floor x)
      (+ (floor x) (/ 1 (cont-frac (/ 1 (- x (floor x))) (- n 1))))))

(defun sqrt-convergent (n i)
  (cont-frac (sqrt n) i))

(defun pell-sol (n)
  (do* ((i 0 (1+ i))
	(x (sqrt-convergent n i) (sqrt-convergent n i)))
       ((= (- (expt (numerator x) 2) (* n (expt (denominator x) 2))) 1)
	(numerator x))))

(defun max-pell-x (N)
  (do ((i 2 (1+ i))
       (max-x 0) (k -1))
      ((= i N) max-x)
    (if (not (= (sqrt i) (isqrt i)))
	(let ((px (pell-sol i)))
	  (if (> px max-x)
	      (setf max-x px k i))))))

;;
(defun div-sum (N)
  (loop for i from 1 to (- N 1)
     if (zerop (mod N i))
     summing i))

(defun divisor-list (N)
  (if (= N 1)
      '(1)
      (let ((i 2))
	(loop while (not (= (mod N i) 0))
	   do
	     (incf i))
	(mapcan #'(lambda(x) (list x (* x i)))
		(divisor-list (/ N i))))))

(defun divisor-sum (N)
  (- (reduce #'+ (remove-duplicates
		  (divisor-list N)))))

(defun amicable-chain-len (k)
  (labels ((chain (n)
	     (if (= n k)
		 nil
		 (cons n (chain (divisor-sum n))))))
    (cons k (chain (divisor-sum k)))))

;;

(defun consec-mul (N)
  (let* ((dis (+ 1 (* 4 N)))
	 (is (floor (sqrt dis))))
    (if (and (= (expt is 2) dis)
	     (oddp is))
	(/ (+ 1 is) 2))))

(defun first-half (low)
  (do* ((d low (1+ d))
	(y (consec-mul (/ (* d (- d 1) 2)))
	   (consec-mul (/ (* d (- d 1) 2)))))
       (y y)))

;;
(defun bouncy-p (n)
  (multiple-value-bind (q r) (truncate n 10)
    (bouncy-rec q r)))

(defun bouncy-rec (n first &optional (order 0))
  (multiple-value-bind (q r) (truncate n 10)
    (if (and (= q 0) (= r 0))
	nil
	(cond ((> r first)
	       (if (= order -1)
		   t
		   (bouncy-rec q r 1)))
	      ((< r first)
	       (if (= order 1)
		   t
		   (bouncy-rec q r -1)))
	      (t (bouncy-rec q r order))))))

(defun non-bouncy-count (low high)
  (loop for i from low to (- high 1)
     counting (not (bouncy-p i))))

;;

(defun fibo (n)
  (labels ((f (k a b)
	     (if (= k n)
		 a
		 (f (+ k 1) b (+ a b)))))
    (f 0 0 1)))

(defun fibolast9 (n)
  (let ((ten (expt 10 9)))
    (labels ((f (k a b)
	       (if (= k n)
		   a
		   (f (+ k 1) b (mod (+ a b) ten)))))
      (f 0 0 1))))

(defun fibofirst9 (n)
  (let ((f (fibo n)))
    (if (>= (digit-len f) 9)
	(parse-integer (subseq (write-to-string f) 0 9))
	f)))

(defun pandigital-p (n &optional (lis nil)) ; n is necessarily a 9 digit number
  (multiple-value-bind (q r) (truncate n 10)
    (if (and (= q 0) (= r 0))
	t
	(if (= r 0)
	    nil
	    (if (find r lis)
		nil
		(pandigital-p q (cons r lis)))))))

(defun digit-len (N)
  (multiple-value-bind (q r) (truncate n 10)
    (if (= q 0)
	1
	(1+ (digit-len q)))))

;;

(defun rmax (a)
  (let ((sq (expt a 2)))
    (loop for n from 1 to (- a 1)
       maximizing (mod (* 2 n a) sq))))

;;
(defun radical-array (N)
  (let ((rad-arr (make-array (+ N 1))))
    (setf (aref rad-arr 1) 1)
    (labels ((radical (n &optional (curr 2))
	       (if (= (mod n curr) 0)
		   (progn (loop while (= (mod n curr) 0)
			     do
			       (setf n (/ n curr)))
			  (* curr (aref rad-arr n)))
		   (radical n (1+ curr)))))
      (loop for i from 2 to N
	 do
	   (setf (aref rad-arr i) (radical i)))
      rad-arr)))

(defun sorted-radical (N)
  (let ((ra (radical-array N)))
    (sort (enumerate-interval 1 N)
	  #'<
	  :key #'(lambda(x) (aref ra x)))))

(defvar *radical-array* (make-array 100000))
(setf (aref *radical-array*  1) 1)
			 
(defun radical (n &optional (curr 2))
  (cond ((= n 1) 1)
	((= (mod n curr) 0)
	 (loop while (= (mod n curr) 0)
	    do
	      (setf n (/ n curr)))
	 (* curr (aref *radical-array* n)))
	(t (radical n (1+ curr)))))

(loop for i from 2 to 99999
   do
     (setf (aref *radical-array* i) (radical i)))

(defun radical-array (N)
  (let ((arr (make-array (+ N 1))))
    (setf (aref arr 1) 1)
    (loop for i from 2 to N
       do
	 (loop for j from 2
	    when (= (mod i j) 0)
	    do
	      (let ((n i))
		(loop while (= (mod n j) 0)
		   do
		     (setf n (/ n j)))
		(setf (aref arr i) (* j (aref arr n))))))
    arr))

;;

(defun sum-squares (N)
  (let ((lis nil))
    (do ((i 1 (1+ i)))
	((>= (+ (expt i 2) (expt (+ i 1) 2)) N))
      (let ((s (expt i 2)))
	(do ((j (+ i 1) (1+ j)))
	    ((>= s N))
	  (incf s (expt j 2))
	  (if (< s N)
	      (push s lis)))))
    lis))

(defun palindrome-p (s)
  (let ((l (length s)))
    (if (or (= l 0) (= l 1))
	t
	(and (char= (char s 0) (char s (- l 1)))
	     (palindrome-p (subseq s 1 (- l 1)))))))

(defun npalind-p (n)
  (palindrome-p (write-to-string n)))

;;

(defun digit-sum (n)
  (multiple-value-bind (q r) (truncate n 10)
    (if (= q 0)
	r
	(+ r (digit-sum q)))))

(defun digit-power-sum-p (n)
  (let ((d (digit-sum n)))
    (if (not (= d 1))
	(let ((x (log n d)))
	  (and (= x (floor x))
	       (= (expt d x) n))))))

;;
(defun reversen (n)
  (let ((nrev 0))
    (do* ((q n (floor q 10))
	  (r (rem q 10) (rem q 10)))
	 ((= q 0) nrev)
      (setf nrev (+ (* 10 nrev) r)))))

(defun reversible-p (n)
  (let* ((l (+ (floor (log n 10)) 1))
	 (a (make-array l)))
    ;((a (make-array 1 :fill-pointer 0 :adjustable t)))
    (do ((k n (floor k 10))
	 (i 0 (1+ i)))
	((= k 0))
      (setf (aref a i) (mod k 10)))

    (let ((carry 0))
      (do ((j 0 (1+ j)))
	  ((= j (floor l 2)) (if (evenp l)
				 t
				 (oddp carry)))
	(let ((s (+ (aref a j) (aref a (- l j 1)) carry)))
	  (setf carry (floor s 10))
	  (if (evenp (mod s 10))
	      (return-from reversible-p nil)))))))

(defun reversible-num-list (N)
  (let ((a (make-array (+ N 1))))
    ))

;;
(defvar pn (make-array (+ N 1)))

(do ((i 2 (1+ i)))
    ((= i N))
  (setf (aref pn i) (next-prime (- i 1))))

(defun rem-prob (p n)
  (mod (* 2 n p) (* p p)))

(defun rem-exceed (k) ; k > 2
  (let ((pn (make-array 1 :fill-pointer 1 :adjustable t)))
    (do ((i 1 (1+ i)))
	((and (oddp i) (> (rem-prob (next-prime (aref pn (- i 1))) i) k)) i)
      (vector-push-extend (next-prime (aref pn (- i 1))) pn))))

;;
(defun lcm1 (&rest integers)
  (if (null integers)
      1
      (let ((c (car integers))
	    (m (apply #'lcm1 (cdr integers))))
	(if (= (mod m c) 0)
	    m
	    (* m (/ c (gcd m c)))))))

;;

(defun sigma (n &optional (start 2))
  (cond ((= n 1) 1)
	((= (mod n start) 0)
	 (let ((k 1))
	   (loop while (= (mod n start) 0)
	      do
		(setf n (/ n start))
		(setf k (* k start)))
	   (* (/ (- (* k start) 1) (- start 1))
	      (sigma n (next-prime start)))))
	(t (sigma n (next-prime start)))))

(defun proper-sigma (n)
  (- (sigma n) n))

(defun amicable-chain-length (n up-bound &optional (lis nil))
  (cond ((> n up-bound) nil)
	((find n lis) 0)
	(t (let ((ac (amicable-chain-length (proper-sigma n)
					    up-bound (cons n lis))))
	     (if (null ac)
		 nil
		 (1+ ac))))))

;;
;???



;???
;;

(defun tau-list (n)
  (let ((a (make-array (+ n 1))))
    (setf (aref a 1) 1)
    (do ((k 2 (1+ k)))
	((= k (+ n 1)))
      (if (primep k)
	  (setf (aref a k) 2)
	  (do ((i 2 (1+ i)))
	      ((= (mod k i) 0) (let ((alpha 0)
				     (m k))
				 (progn (loop while (= (mod m i) 0)
					   do
					     (incf alpha)
					     (setf m (/ m i)))
					(setf (aref a k)
					      (* (+ 1 alpha) (aref a m)))))))))
    a))

;;

(defun simple-sieve (n v)
  (let ((a (make-array (+ n 1) :initial-element t))
	;(v (make-array 1 :fill-pointer 0 :adjustable t))
	(is (isqrt n)))
    (do ((i 2 (1+ i)))
	((> i is))
      (if (not (null (aref a i)))
	  (do ((j (+ i 1) (1+ j)))
	      ((> j n))
	    (if (zerop (mod j i))
		(setf (aref a j) nil)))))
    (do ((i 2 (1+ i)))
	((> i n))
      (if (not (null (aref a i)))
	  (vector-push-extend i v)))))


(defun segmented-sieve (n)
  (let* ((d (isqrt n))
	 (primes (make-array 1 :fill-pointer 0 :adjustable t)))
    (simple-sieve d primes)
    (let ((k (length primes)))
	 ;(seg-primes primes))
      (do ((low (+ d 1) (+ low d))
	   (m 2 (1+ m)))
	  ((>= low n))
	(let ((b (make-array d :initial-element t))
	      (high (min n (* m d))))
	  (do ((p 0 (1+ p)))
	      ((= p k))
	    (let* ((x (elt primes p))
		   (st (* x (ceiling low x))))
	      (do ((j st (+ j x)))
		  ((> j high))
		(if (zerop (mod j x))
		    (setf (aref b (- j low)) nil)))))
	  (do ((i 0 (1+ i)))
	      ((= i d))
	    (if (not (null (aref b i)))
		(vector-push-extend (+ i low) primes))))))
    primes))

;;
	   
(defun hyperexpt-rem (a k n)
  (if (= k 1)
      (mod a n)
      (let ((x (hyperexpt-rem a (- k 1) (euler-phi n)))
	    (m 1))
	(dotimes (i x)
	  (setf m (mod (* m a) n)))
	m)))

;;

(defun prize-strs (n k)
  (cond ((= n 0) 1)
	((= k 0) 0)
	((= n 1) 3)
	(t (+ (prize-strs (- n 1) k)
	      (prize-strs (- n 2) (- k 1))
	      (prize-strs (- n 2) k)
	      (prize-strs (- n 1) (- k 1))))))

;;
(defun no-muls-end (n end)
  (cond ((or (= n 1) (= n end)) 0)
	((evenp n) (1+ (no-muls-end (/ n 2) end)))
	(t (1+ (no-muls-end (- n 1) end)))))

(defun min-no-muls1 (n)
  (cond ((= n 1) 0)
	((evenp n) (1+ (min-no-muls1 (/ n 2))))
	(t (loop for i from 1 to (floor n 2)
	      minimizing (+ 1 (min-no-muls1 i)
			      (no-muls-end (- n i) i))))))

(defun min-expt-muls-list (n)
  (let ((a (make-array (list (+ n 1) (+ n 1)) :initial-element nil)))
    (loop for i from 1 to 
    (labels ((min-no-muls2 (n &optional (ends '(1)))
	       (cond ((find n ends) 0)
		     ((evenp n) (let ((m2 (aref a (/ n 2) )))
				  (if (not m2)
				      (1+ (min-no-muls2 (/ n 2) ends))
				      (1+ m2))))
		     (t (loop for i from 1 to (floor n 2)
			     minimizing (+ 1
(defun min-no-muls2 (n &optional (ends '(1)))
  (cond ((find n ends) 0)
	((evenp n) (1+ (min-no-muls2 (/ n 2) ends)))
	(t
	 (loop for i from 1 to (floor n 2)
	    minimizing (+ 1 (min-no-muls2 i ends) (min-no-muls2 (- n i) (cons i ends)))))))

(defun min-no-muls2 (n &optional (ends '(1)) (this-sum 0) (curr-min 0))
  (cond ((find n ends) this-sum)
	((> this-sum curr-min) this-sum)
	((evenp n) (1+ (min-no-muls2 (/ n 2) ends)))
	(t
	 (do ((i 1 (1+ i))
	      (m n))
	     ((> i (floor n 2)) m)
	   (let ((x (+ 1 (min-no-muls2 i ends 0 m) (min-no-muls2 (- n i) (cons i ends) 0 m))))
	     (if (< x m)
		 (setf m x)))))))

(defun factors (n)
  (loop for i from 2 to (isqrt n)
     if (zerop (mod n i))
     nconc `((,i ,(/ n i)) (,(/ n i) ,i))))

(defun min-no-muls (n)
  (cond ((= n 1) 0)
	((primep n) (1+ (min-no-muls (- n 1))))
	(t (loop for x in (factors n)
	      minimizing (+ (- (car x) 1) (min-no-muls (cadr x)))))))

;;

(defun hamming-num (n type)
  (let ((a (make-array (+ n 1) :initial-element nil)))
    (setf (aref a 0) t (aref a 1) t)
    (do ((i 2 (1+ i)))
	((> i n))
      (setf (aref a i) (do ((k 2 (1+ k)))
			   ((> k type) nil)
			 (if (zerop (mod i k))
			     (return (elt a (/ i k)))))))
    (1- (loop for x across a
	   count x))))

;;
(defun sudoku-possibs (a i j)
  (if (not (= (aref a i j) 0))
      nil
      (let ((lis nil)
	    (x (* 3 (floor i 3)))
	    (y (* 3 (floor j 3))))
	(loop for k from 1 to 9
	   do
	     (if (not (loop for r from 0 to 8
			 if (= (aref a r j) k)
			 do
			   (return t)))
		 (if (not (loop for c from 0 to 8
			     if (= (aref a i c) k)
			     do
			       (return t)))
		     (if (not (let ((temp nil))
				(loop for r from x to (+ x 2)
				   do
				     (loop for c from y to (+ y 2)
					if (= (aref a r c) k)
					do
					  (setf temp t)))
				temp))
			 (push k lis)))))
	lis)))

(defun sudoku-solve (a)
  (let ((pos (loop for i from 0 to 8
		nconc
		  (loop for j from 0 to 8
		     if (= (aref a i j) 0)
		     collect `(,i ,j)))))
      (sudoku-iterate a pos)))

(defun sudoku-iterate (a pos)
  (if (null pos)
      (return-from sudoku-iterate a)
      (let* ((i (caar pos))
	     (j (cadar pos))
	     (b (sudoku-possibs a i j)))
	(if (null b)
	    nil
	    (progn (dolist (x b)
		     (setf (aref a i j) x)
		     (let ((y (sudoku-iterate a (cdr pos))))
		       (if y
			   (return-from sudoku-iterate y))))
		   (setf (aref a i j) 0)
		   nil)))))

(with-open-file (in "sudoku.txt")
  (do ((s (read-line in nil nil)
	  (read-line in nil nil))
       (sum 0))
      ((null s) sum)
    (let ((a (make-array '(9 9) 
			 :initial-contents 
			 (append (list (mapcar #'(lambda(x) 
						   (parse-integer 
						    (string x)
						    :junk-allowed t))
					       (char-list
						(subseq s 0 9))))
				 (loop for i from 1 to 8
				    collect (mapcar #'(lambda(x) 
							(parse-integer 
							 (string x)
							 :junk-allowed t))
						    (char-list 
						     (subseq
						      (read-line in nil nil)
						      0 9))))))))
      (let ((solved (sudoku-solve a)))
	(format t "~a ~a ~a ~%" (aref solved 0 0) (aref solved 0 1) (aref solved 0 2))
	(incf sum (+ (* (aref solved 0 0) 100)
		     (* (aref solved 0 1) 10)
		     (aref solved 0 2)))))
    (read-line in nil nil)))
;;
;DPize it
(defun 1-late-prize (n)
  (cond ((= n 1) 3)
	((= n 2) 8)
	((= n 3) 19)
	(t (+ (1-late-prize (- n 1))
	      (no-late-prize (- n 1))
	      (1-late-prize (- n 2))
	      (no-late-prize (- n 2))
	      (1-late-prize (- n 3))
	      (no-late-prize (- n 3))))))

(defun no-late-prize (n)
  (cond ((= n 1) 2)
	((= n 2) 4)
	((= n 3) 7)
	(t (+ (no-late-prize (- n 1))
	      (no-late-prize (- n 2))
	      (no-late-prize (- n 3))))))


;;

(defun nth-pascal-row (n)
  (if (= n 0)
      (make-array 1 :initial-element 1)
      (let ((prev (nth-pascal-row (- n 1)))
	    (next (make-array (+ n 1))))
	(let ((fprev (concatenate 'vector #(0) prev))
	      (sprev (concatenate 'vector prev #(0))))
	  (loop for i from 0 to n
	     do
	       (setf (aref next i) (+ (aref fprev i) (aref sprev i))))
	  next))))

(defun n-pascal-rows (n)
  (let ((a (make-array n)))
    (dotimes (i n)
      (if (= i 0)
	  (setf (aref a i) #(1))
	  (let ((prev (aref a (- i 1)))
		(next (make-array (+ i 1))))
	    (let ((fprev (concatenate 'vector #(0) prev))
		  (sprev (concatenate 'vector prev #(0))))
	      (progn (loop for j from 0 to i
			do
			  (setf (aref next j) (+ (aref fprev j) (aref sprev j))))
		     (setf (aref a i) next))))))
    a))


(defun squarefree-p (n)
  (if (= n 1)
      t
      (do ((i 2 (1+ i)))
	  ((= (mod n i) 0) (and (not (= (mod (/ n i) i) 0))
				(squarefree-p (/ n i)))))))

(defun npascal-rows (n)
  (let ((a (make-array n)))
    (dotimes (i n)
      (if (= i 0)
	  (setf (aref a i) '(1))
	  (let ((prev (aref a (- i 1))))
	    (setf (aref a i)
		  (mapcar #'+ (append '(0) prev) (append prev '(0)))))))
    a))

(defun distinct-pascal (rows)
  (let ((lis nil)
	(a (npascal-rows rows)))
    (loop for x across a
       do
	 (setf lis (remove-duplicates (append x lis))))
    lis))

;;

(defun n-follows-string-p (n str)
  (n-follows-lis-p n (loop for x across str
			  collect x)))

(defun n-follows-lis-p (n charlis)
  (if (null charlis)
      t
      (case (car charlis)
	(#\D (and (= (mod n 3) 0) (n-follows-lis-p (/ n 3) (cdr charlis))))
	(#\U (and (= (mod n 3) 1) (n-follows-lis-p (/ (+ (* 4 n) 2) 3)
						      (cdr charlis))))
	(#\d (and (= (mod n 3) 2) (n-follows-lis-p (/ (- (* 2 n) 1) 3)
						      (cdr charlis)))))))

(defun string->fun (str)
  (let ((n (length str)))
    (labels ((f (i x)
	       (if (= i n)
		   x
		   (case (char str i)
		     (#\D (* 3 (f (+ i 1) x)))
		     (#\U (/ (- (* 3 (f (+ i 1) x)) 2) 4))
		     (#\d (/ (+ (* 3 (f (+ i 1) x)) 1) 2))))))
      (lambda(x) (f 0 x)))))

(defun f3 (x)
  (funcall (string->fun "UDDDUdddDDUDDddDdDddDDUDDdUUDd") x))

;;

(defun make-gf (fn &oprional (N 100))
  (let ((gf (make-array (+ N 1))))
    (loop for i from 0 to N
       do
	 (setf (aref gf i) (funcall fn i)))))

(defvar *peter* (make-array 5 :initial-contents '(0 1 1 1 1)))

(defun coeff (gf i)
  (aref gf i))

(defun add-gfs (gf1 gf2)
  (let ((m1 nil) (m2 nil) (mgf nil))
    (if (> (length gf1) (length gf2))
	(setf m1 (length gf2) m2 (length gf1) mgf gf1)
	(setf m1 (length gf1) m2 (length gf2) mgf gf2))
    (let (((gf+ (make-array m2)))
	  (loop for i from 0 to m1
	     do
	       (setf (coeff gf+ i) (+ (coeff gf1 i) (coeff gf2 i))))
	  (loop for i from (+ m1 1) to m2
	     do
	       (setf (coeff gf+ i) (coeff mgf i)))
	  gf+))))

(defun mul-gfs (gf1 gf2)
  (let ((n (- (+ (length gf1) (length gf2)) 1)))
    (let ((ngf1 (make-array n))
	  (ngf2 (make-array n))
	  (m-gf (make-array n)))
      (dotimes (i (length gf1))
	(setf (aref ngf1 i) (aref gf1 i)))
      (dotimes (i (length gf2))
	(setf (aref ngf2 i) (aref gf2 i)))
      (dotimes (i n)
	(setf (aref m-gf i)
	      (loop for j from 0 to i
		 summing (* (coeff ngf1 j) (coeff ngf2 (- i j))))))
      m-gf)))


(defun expt-gfs (gf n)
  (if (= n 1)
      gf
      (mul-gfs gf (expt-gfs gf (- n 1)))))

(defvar *peter-9-dice* (expt-gfs *peter* 9))

(defun peter-total-sum (s) ; nine 4-sided dices
  (coeff *peter-9-dice* s))

(defvar *colin* (make-array 7 :initial-contents '(0 1 1 1 1 1 1)))

(defvar *colin-6-dice* (expt-gfs *colin* 6))

(defun colin-total-sum (s)
  (coeff *colin-6-dice* s))

(/ (loop for i from 1 to 36
      summing
	(* (peter-total-sum i) (loop for j from 1 to (- i 1)
				  summing (colin-total-sum j))))
   (* (expt 4 9) (expt 6 6)))

;;

(defun connected-p (v1 v2 a)
  (do ((i v1 (1+ i))
       (l nil))
      ((= i (array-dimension a 0)) (if (null (some #'(lambda(x)
						       (connected-p x v2 a))
						   l))
				       (connected-p-n v2 v1 a)
				       t))
				       
    (if (not (= (aref a v1 i) 0))
	(if (= i v2)
	    (return-from connected-p t)
	    (push i l)))))

(defun connected-p-n (v1 v2 a)
  (do ((i v1 (1+ i))
       (l nil))
      ((= i (array-dimension a 0)) (if (null (some #'(lambda(x)
						       (connected-p x v2 a))
						   l))
				       nil
				       t))
				       
    (if (not (= (aref a v1 i) 0))
	(if (= i v2)
	    (return-from connected-p-n t)
	    (push i l)))))

(defun graph-connected-p (a)
  (let ((l (enum-int 0 (- (array-dimension a 0) 1))))
    (every #'(lambda(x) (if (= x 0)
			    t
			    (connected-p 0 x a)))
	   l)))

(defun enum-int (start end)
  (if (> start end)
      nil
      (cons start (enum-int (1+ start) end))))

(defun graph-fill-pos (a)
  (loop for i from 0 to (1- (array-dimension a 0))
     nconc
       (loop for j from i to (1- (array-dimension a 1))
	  if (not (= (aref a i j) 0))
	  collect `(,i ,j))))

(defun copy-array2 (a)
  (let ((m (array-dimension a 0))
	(n (array-dimension a 1)))
    (let ((b (make-array `(,m ,n))))
      (loop for i from 0 to (- m 1)
	 do
	   (loop for j from 0 to (- n 1)
	      do
		(setf (aref b i j) (aref a i j))))
      b)))

(defun weight (a)
  (loop for i from 0 to (1- (array-dimension a 0))
     summing
       (loop for j from (+ i 1) to (1- (array-dimension a 1))
	  summing (aref a i j))))

(defun graph-iter (a pos &optional (removed-pos nil))
  (cond ((not (graph-connected-p a)) nil)
	((null pos) (list (list (weight a) removed-pos)))
	(t (append (graph-iter a (cdr pos) removed-pos)
		   (let ((b (copy-array2 a)))
		     (progn (setf (aref b (caar pos) (cadar pos)) 0
				  (aref b (cadar pos) (caar pos)) 0)
			    (graph-iter b (cdr pos) (cons (car pos)
							  removed-pos))))))))

(defun graph-iter2 (a pos &optional (curr-min-weight (weight a)))
  (cond ((not (graph-connected-p a)) nil)
	((> (weight a) curr-min-weight) nil)
	((null pos) (list (weight a)))
	(t (append (graph-iter2 a (cdr pos) (weight a))
		   (let ((b (copy-array2 a)))
		     (progn (setf (aref b (caar pos) (cadar pos)) 0
				  (aref b (cadar pos) (caar pos)) 0)
			    (graph-iter2 b (cdr pos) (weight a))))))))
(defun min-car (num &rest nums)
  (if (null nums)
      num
      (if (> (car num) (caar nums))
	  (apply #'min-car (car nums) (cdr nums))
	  (apply #'min-car num (cdr nums)))))

(defun min-weight (graph)
  (apply #'min (graph-iter2 graph (graph-fill-pos graph))))
   
(defun min-conct-weight (graph)
  (apply #'min-car (graph-iter graph (graph-fill-pos graph))))

(with-open-file (in "network.txt")
  (do ((s (read-line in nil nil) (read-line in nil nil))
       (i 0 (1+ i)))
      ((null s))
    (let ((ds (delimit s #\,))
	  (j 0))
      (dolist (x ds)
	(if (not (string= x "-"))
	    (setf (aref *gra* i j) (let ((p (parse-integer x :junk-allowed t)))
				     (if (null p)
					 0
					 p))))
	(incf j)))))



;;

(defun nCk (n k)
  (if (= k 0)
      1
      (* (/ n k) (nCk (- n 1) (- k 1)))))

(defun nck-mod-p (n k p)
  (do ((m (mod n p) (mod n p))
       (i (mod k p) (mod k p))
       (mul 1))
      ((= n 0) (mod mul p))
    (setf mul (* mul (nck m i)))
    (setf n (floor n p))
    (setf k (floor k p))))

(defun fact-p-pow (n p)
  (if (= n 0)
      0
      (let ((f (floor n p)))
	(+ f (fact-p-pow f p)))))

(defun nck-p-pow (n k p)
  (- (fact-p-pow n p) (+ (fact-p-pow k p) (fact-p-pow (- n k) p))))


;;

(defun int-part-prop-list (n)
  (let ((perfect (make-array (+ n 1)))
	(total (make-array (+ n 1)))
	(prop (make-array (+ n 1))))
    (do* ((i 1 (1+ i))
	  (x (/ (+ 1 (sqrt (+ 1 (* 4 i)))) 2) (/ (+ 1 (sqrt (+ 1 (* 4 i)))) 2)))
	 ((> i n))
      (if (= x (floor x))
	  (if (= (log x 2) (floor (log x 2)))
	     (setf (aref perfect i) (1+ (aref perfect (- i 1)))
		   (aref total i) (1+ (aref total (- i 1))))
	     (setf (aref perfect i) (aref perfect (- i 1))
		   (aref total i) (1+ (aref total (- i 1)))))
	  (setf (aref total i) (aref total (- i 1))
		(aref perfect i) (aref perfect (- i 1)))))
    ;perfect))
    (do ((i 1 (1+ i)))
	((> i n))
      (if (not (= (aref total i) 0))
	  (setf (aref prop i) (/ (aref perfect i) (aref total i)))))
    prop))

(defun smallest-part-prop (r)
  (let ((perfect (make-array 1 :initial-element 1 :fill-pointer 1 :adjustable t))
	(total (make-array 1 :initial-element 1 :fill-pointer 1 :adjustable t)))
	;(m 2))
    (do* ((i 1 (1+ i))
	  (m 3 (1+ m))
	  (x (/ (+ 1 (sqrt (+ 1 (* 4 m)))) 2) (/ (+ 1 (sqrt (+ 1 (* 4 m)))) 2)))
	 ((< (/ (aref perfect (- i 1)) (aref total (- i 1))) r) (- m 1))
      (if (= x (floor x))
	  (if (= (log x 2) (floor (log x 2)))
	     (progn (vector-push-extend (1+ (aref perfect (- i 1))) perfect)
		    (vector-push-extend (1+ (aref total (- i 1))) total))
	     (progn (vector-push-extend (aref perfect (- i 1)) perfect)
		    (vector-push-extend (1+ (aref total (- i 1))) total)))
	  (progn (vector-push-extend (aref total (- i 1)) total)
		 (vector-push-extend (aref perfect (- i 1)) perfect))))))

(defun smallest-part-prop (r)
  (let ((perfect-last 1)
	(total-last 1))
	;(m 2))
    (do* ((m 3 (1+ m))
	  (x (isqrt (+ 1 (* 4 m))) (isqrt (+ 1 (* 4 m)))))
	 ((< (/ perfect-last total-last) r) (list (- m 1) perfect-last total-last))
      (if (= (expt x 2) (+ 1 (* 4 m)))
	  (let ((y (/ (+ 1 x) 2)))
	    (if (= (log y 2) (floor (log y 2)))
		(setf perfect-last (1+ perfect-last)
		      total-last (1+ total-last))
		(setf total-last (1+ total-last))))))))
