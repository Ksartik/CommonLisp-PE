(defun hamming-satisfy (k index remainder &optional (base 2))
  (let ((q nil)
	(r nil))
    (setf (values q r) (truncate k base))
    (if (= index 0)
	(= remainder r)
	(hamming-satisfy q (- index 1) remainder base))))

(defun enum-interval (low high)
  (if (> low high)
      nil
      (cons low (enum-interval (+ low 1) high))))

(defun hamming-list (N index remainder &optional (base 2))
  (remove-if-not
   #'(lambda(x) (hamming-satisfy x index remainder base))
   (enum-interval 1 N)))

;---- BINARY SPECIFIC ONLY ----;
(defun hamming-syndrome (S)
  (let ((l (length S)))
    (let ((x (floor (log l 2))))
      (mapcar
       #'(lambda(a)
	   (apply #'plusmod 2
		  (mapcar #'(lambda(y) (- (nth (- y 1) S)))
			  (hamming-list l a 1))))
       (enum-interval 0 x)))))

(defun plusmod (div &rest n)
    (if (null n)
	0
	(mod (+ (car n) (apply #'plusmod div (cdr n))) div)))

(defun hamming-encode (S)
  (let ((zS (insert-if-pos S nil #'power2-1)))
    (replace-if-pos zS (hamming-syndrome zS) #'power2-1)))

(defun insert-if-pos (S replS positionp &optional (index 0))
  (cond ((null S) nil)
	((funcall positionp index)
	 (if (null replS)
	     (cons 0 (insert-if-pos S (cdr replS) positionp (+ index 1)))
	     (cons (car replS)
		   (insert-if-pos S (cdr replS)
				   positionp
				   (+ index 1)))))
	(t (cons (car S) (insert-if-pos (cdr S) replS
					 positionp
					 (+ index 1))))))

(defun power2-1 (n)
  (= (mod (log (+ n 1) 2) 1) 0))

(defun replace-if-pos (S replS positionp &optional (index 0))
  (cond ((null S) nil)
	((funcall positionp index)
	 (if (null replS)
	     (cons 0 (replace-if-pos (cdr S) (cdr replS)
				     positionp (+ index 1)))
	     (cons (car replS)
		   (replace-if-pos (cdr S) (cdr replS)
				   positionp (+ index 1)))))
	(t (cons (car S) (replace-if-pos (cdr S) replS
					 positionp
					 (+ index 1))))))

;; HAMMING-DECODE Find error position- -
(defun hamming-check (S)
  (let ((l (length S)))
    (let ((x (floor (log l 2))))
      (reverse
       (mapcar
	#'(lambda(a)
	    (apply #'plusmod 2
		   (mapcar #'(lambda(y) (nth (- y 1) S))
			   (hamming-list l a 1))))
	(enum-interval 0 x))))))
