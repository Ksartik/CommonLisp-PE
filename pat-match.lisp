(defun some-val (p l)
  (if (null l)
      nil
      (let ((pc (funcall p (first l))))
	(if pc
	    pc
	    (some-val p (rest l))))))

(defun simple-equal (x y)
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  (if (variable-p pattern)
      t
      (if (or (atom pattern) (atom input))
	  (eql pattern input)
	  (and (pat-match (first pattern) (first input))
	       (pat-match (rest pattern) (rest input))))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun pat-binds (pattern input)
  (if (variable-p pattern)
      (list (cons pattern input))
      (if (or (atom pattern) (atom input))
	  nil
	  (append (pat-binds (first pattern) (first input))
		  (pat-binds (rest pattern) (rest input))))))

(defconstant  no-bindings nil)
(defconstant fail nil)

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun pat-match2 (pattern input &optional (bindings no-bindings))
  (if (null pattern)
      (if (null input)
	  bindings
	  nil)
      (let ((pat (first pattern))
	    (inp (first input)))
	(cond ((variable-p pat)
	       (let ((mv (match-variable pat inp bindings)))
		 (if mv
		     (pat-match2 (rest pattern) (rest input) mv)
		     nil)))
	      ((segment-pattern-p pat)
	       (let ((seg (segment-match (cadr pattern)  input bindings)))
		 (let ((branch-binds (mapcar
				      (lambda(x) (cons (cdr x) (list (cons (cadr pat) (car x)))))
				      seg)))
		   
		   (if branch-binds 
		       (some-val (lambda(x)
				   (pat-match2 (cdr pattern)
					       (car x)
					       (match-variable (caadr x) (cdadr x)
							       bindings)))
					       ;(append bindings (cdr x))))
				 branch-binds)
		       nil))))
	      ((atom pat)
	       (if (eql pat inp)
		   (pat-match2 (rest pattern) (rest input) bindings)
		   nil))
	      (t (pat-match2 (rest pattern)
			     (rest input)
			     bindings))))))

(defun match-variable (var input bindings)
  (let ((bind (assoc var bindings)))
    (cond ((not bind) (append bindings (list (cons var input))))
	  ((equal input (cdr bind)) bindings)
	  (t fail))))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (eql (car pattern) '?*)))

(defun segment-match-go (p i bindings)
  (cond ((variable-p p)
	 (let ((bind (assoc p bindings))) 
	   (if (not bind)
	       nil
	       (segment-match-go (cdr bind) i))))
	((segment-pattern-p p)
	 (let ((bind (assoc (cadr p) bindings)))
	   (if (not bind)
	       nil
	       (segment-match-go-seg (cdr bind) i))))
	(t
	 (segment-match-go p i))))

(defun segment-match (p i &optional ip)
  (cond ((null i) (if (null p)
		      (list (cons ip i))
		      nil))
	((eql p (first i)) (cons (cons ip i) (segment-match p (rest i) (append ip(list p)))))
	(t (segment-match p (rest i) (append ip (list (first i)))))))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (position (first pat) input :start start :test #'equal)))
	  
	  (if (null pos)
	      fail
	      (let ((b2 (pattern-match pat (subseq input pos)
				   (match-variable var (subseq input 0 pos)
						   bindings))))
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings no-bindings)
	    nil
	    bindings)))

(defun pattern-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pattern-match (rest pattern) (rest input)
		    (pattern-match (first pattern) (first input)
			       bindings)))
	(t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hellp (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y) (Really -- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun eliza ()
  (loop
     (print '<eliza>)
     (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  (some #'(lambda(rule)
	    (let ((result (pat-match2 (rule-pattern rule) input)))
	      (if (not (eq result fail))
		  (sublis (switch-viewpoint result)
			  (random-elt (rule-responses rule))))))
	*eliza-rules*))
(defun switch-viewpoint (words)
  (sublis '((I . you) (you . I) (me . you) (am . are))
	  words))

(defun flatten (the-list)
  (mappend #'mklist the-list))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  (elt choices (random (length choices))))
