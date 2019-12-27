
;;;;;;;;;;;;;;;;;;; Probability distributions ;;;;;;;;;;;;;;;;;;;


(defclass distribution ()
  ((arguments :initarg :arguments :reader arguments :initform nil)
   (variable :initarg :variable :reader dist-var)))
(defclass bernouilli (distribution)
  ((p :initarg :p :accessor p)
   (symbols :initarg :symbols :accessor symbols)))
(defclass categorical (distribution)
  ((category-counts :accessor category-counts :initform (make-hash-table :test #'equal))
   (p :reader p :initform (make-hash-table :test #'equal))))
(defclass uniform (distribution) ())

(defmethod initialize-instance :after ((d categorical) &key parameters)
  "Parameters must be supplied as an ALIST: a list with items (PARAM . PROB). 
The context of a parameter is (CDR PROB), and corresponds to a list of states 
corresponding to variables that D is conditioned on. If D is not conditioned 
on anything, the context may be set to NIL. This means that each parameter 
must be a list of length 1 (the CDR of which is NIL)."
  (dolist (parameter parameters)
    (setf (gethash (car parameter) (parameters d)) (cdr parameter)))
  (let ((contexts (remove-duplicates (mapcar #'cdar parameters) :test #'equal)))
    (dolist (context contexts)
      (let ((sum 0))
	(maphash (lambda (param v) (when (equal (cdr param) context)
				     (incf sum v)))
		 (parameters d))
	(when (> (abs (- sum 1)) 1.0e-10)
	  (warn "Parameters of ~A sum to ~A, not to approximately 1.0, for context ~A."
		(dist-var d) sum context))))))

(defmethod observe ((d distribution) state symbol training?)
  "Ignore observations by default.")

(defmethod observe ((d categorical) state symbol training?)
  (when training?
    (let* ((arguments (mapcar (lambda (v) (getarg v state)) (arguments d)))
	   (counts (category-counts d))
	   (new-arg-count (1+ (gethash arguments counts 0)))
	   (new-s-count (1+ (gethash (cons symbol arguments) counts 0))))
      (setf (gethash arguments counts) new-arg-count)
      (setf (gethash (cons symbol arguments) counts) new-s-count)
      (setf (gethash (cons symbol arguments) (parameters d))
	    (/ new-s-count new-arg-count)))))

(defmethod next-sequence ((d distribution) training?)
  "Called after each training sequence. May be used to update
model state.")
       
(defmethod probability ((d uniform) arguments symbol)
  1)


(defmethod probability ((d bernouilli) arguments symbol)
  (when (not (null arguments))
    (warn "It looks like you're conditioning a Bernouilli distribution on something,
the implementation does not support this."))
  (if (equal symbol (car (symbols d)))
      (p d)
      (progn
	(unless (equal symbol (cadr (symbols d)))
	  (warn "Generating (1 - p) probability for unfamiliar symbol."))
	(- 1 (p d)))))


(defmethod probability ((d categorical) arguments symbol)
  (gethash (cons symbol arguments) (parameters d)))

(defmethod probabilities ((d distribution) parents-state congruent-states)
  (let* ((table (make-hash-table :test #'equal))
	 (arguments (mapcar (lambda (v) (getarg v parents-state)) (arguments d)))
	 (probabilities (mapcar (lambda (s) (probability d arguments s))
				congruent-states))
	 (sum (apply #'+ probabilities)))
    (loop for s in congruent-states for p in probabilities do
	 (setf (gethash s table)
	       (/ p sum)))
    table))
