(defun join-small2 (s)
  (let ((ns (sort s #'< :key #'cadr)))
    (cons (list (list (caar ns) (caadr ns))
		(+ (cadar ns) (cadadr ns)))
	  (cddr ns))))

(defun huffman-join (s)
  (if (= (length s) 1)
      s
      (huffman-join (join-small2 s))))

(defun huffman-codes-pre (tree)
  (if (atom tree)
      (list (list tree))
      (append (mapcar #'(lambda(x) (cons 1 x))
		      (huffman-codes-pre (cadr tree)))
	      (mapcar #'(lambda(x) (cons 0 x))
		      (huffman-codes-pre (car tree))))))

(defun huffman-codes (tree)
  (mapcar #'(lambda(x) (let ((l (length x)))
			 (cons (nth (- l 1) x)
			       (subseq x 0 (- l 1)))))
	  (huffman-codes-pre tree)))

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defun mapcode (str codes &optional (current-code nil))
  (if (null str)
      (if (null current-code)
	  nil
	  (let ((pcode (assoc current-code codes)))
	    (if pcode
		(cdr pcode)
		nil)))
      (if (null current-code)
	  (mapcode (cdr str) codes (car str))
	  (let ((pcode (assoc current-code codes)))
	    (if pcode
		(append (cdr pcode)
			(mapcode (cdr str) codes (car str)))
		(mapcode (cdr str) codes (symbol-append current-code
							(car str))))))))

(defun huffman-encode (S table)
  (let ((codes (huffman-codes (caar (huffman-join table)))))
    (mapcode S codes)))


(defun huffman-decode (S 
