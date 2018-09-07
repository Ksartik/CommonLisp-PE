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

(defun sudoku-solve (a)
  (let ((pos (loop for i from 0 to 8
		nconc
		  (loop for j from 0 to 8
		     if (= (aref a i j) 0)
		     collect `(,i ,j)))))
    (sudoku-iterate a pos)))
