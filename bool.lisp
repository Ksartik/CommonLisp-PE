(defmacro binexp-read (&rest exp)
  `(loop for x in '(,exp)
      collect x))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun infixexp->prefixfn (exp)
  (cond ((atom exp) exp)
	((atom (car exp)) (list (case (cadr exp)
				  (* 'and)
				  (+ 'or)
				  (otherwise (cadr exp)))
				(car exp)
				(infixexp->prefixfn (caddr exp))))
	(t (list (case (cadr exp)
		   (* 'and)
		   (+ 'or)
		   (otherwise (cadr exp)))
		 (infixexp->prefixfn (car exp))
		 (infixexp->prefixfn (caddr exp))))))

(defun all-possibs (n)
  (if (= n 0)
      (list nil)
      (mapcan #'(lambda(x) (list (cons 0 x) (cons 1 x)))
	      (all-possibs (- n 1)))))

(defmacro let-bind (vars vals &body body)  
  `(let ,(loop for x in vars
	    for y in vals
	    collect `(,x ,y))
     ,@body))

(defmacro exp->tt ((&rest vars) exp)
  (let ((bin-dom (all-possibs (length vars))))
;    (with-gensyms (vars)
    `(let ((x nil))
       ,@(dolist (ls bin-dom)
	   (let-bind vars ls
	     (push `(,ls ,@(eval (infixexp->prefixfn exp)) x)))
       x))))

(defun exp->tt (vars exp)
  (let ((bin-dom (all-possibs (length vars)))
	(x nil))
    (dolist (ls bin-dom)
      (let-bind vars ls
	(push `(,ls ,@(eval (infixexp->prefixfn exp)) x))))
    x))

(defun implicant-merge (t1 t2)
  (let ((s 0))
    (dotimes (i (array-dimension t1 0))
	(if (> s 1)
	    (return-from implicant-merge nil))
	(if (not (eq (aref t1 i) (aref t2 i)))
	    (progn (setf (aref t1 i) nil)
		   (incf s)))))
  t1)

(defun all-eval (pred seq)
  (if (null seq)
      nil
      (let ((p (funcall pred (car seq))))
	(if p
	    (cons p (all-eval pred (cdr seq)))
	    (all-eval pred (cdr seq))))))
    
(defun implicant-reduce (minterms &optional (final-lis nil))
  (if (null minterms)
      final-lis
      (implicant-reduce (mapcon #'(lambda(pair)
				    (let ((imp (all-eval #'(lambda(x)
							     (implicant-merge (car pair) x))
							 (cdr pair))))
				      (if imp
					  imp
					  (progn (push (car pair) final-lis) nil))))
				minterms)
			final-lis)))

(defun 1-diff-p (t1 t2)
  (let ((s 0))
    (dotimes (i (array-dimension t1 0))
	(if (> s 1)
	    (return-from 1-diff-p nil))
	(if (not (eq (aref t1 i) (aref t2 i)))
		   (incf s))))
  t)

(defun size-implicant (curr-term rest-terms size)
  (cond ((or (null rest-terms) (= size 0)) nil)
	((1-diff-p curr-term (car rest-terms))
	 (cons (cons (car rest-terms)
		     (size-implicant (car rest-terms)
				     (cdr rest-terms)
				     (- size 2)))
	       (size-implicant curr-term (cdr rest-terms) size)))
	(t
	 (size-implicant curr-term (cdr rest-terms) size))))

(defun size-implicant (
(defun size-implicant-reduce (minterms size)
  
