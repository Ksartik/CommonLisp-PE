(defvar *db* nil)

(defun add-record (title artist rating ripped)
  (push (make-cd title artist rating ripped) *db*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun get-cd ()
  (add-record 
   (prompt-read "Title")
   (prompt-read "Artist")
   (parse-integer (prompt-read "Rating"))
   (y-or-n-p "Ripped(y/n)")))

(defun cd-database ()
  (loop 
     (get-cd)
     (if (not (y-or-n-p "Another(y/n)"))
	 (return))))
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))


(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax 
      (format t (read in)))))

(defun select (f)
  (remove-if-not f *db*))


(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda(cd)
      (and 
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar #'(lambda(cd)
		    (when (funcall selector-fn cd)
		      (if title (setf (getf cd :title) title))
		      (if artist (setf (getf cd :artist) artist))
		      (if rating (setf (getf cd :rating) rating))
		      (if ripped-p (setf (getf cd :ripped) ripped)))
		    cd)
		*db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun where2move (cd indicators)
  (if (null indicators)
      t
      (and (equal (getf cd (car indicators)) (cadr indicators))
	   (where2move cd (cddr indicators)))))

(defun where2 (&rest indicators)
  #'(lambda(cd) 
      (where2move cd indicators)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields 
     collecting (make-comparison-expr (pop fields)
				      (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda(cd) (and ,@(make-comparisons-list clauses))))
