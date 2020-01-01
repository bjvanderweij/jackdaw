(cl:defpackage #:jackdaw
  (:use #:common-lisp)
  (:export
   "GENERATIVE-MODEL" "V" "RECURSIVE" "ACCUMULATOR" "NGRAM-ACCUMULATOR"
   "ONE-SHOT"
   "DEFMODEL" "TRANSITION" "GENERATE-STATES"
   "DISTRIBUTION" "BERNOUILLI" "CATEGORICAL" "UNIFORM"
   "OBSERVE" "PROBABILITY" "PROBABILITIES"
   "+INACIVE+" "+SINGLETON+")
  (:documentation "A toolkit for defining dynamic Bayesian networks 
with congruency constraints."))

(cl:in-package #:jackdaw)

(defvar +inactive+ '*)
(defvar +singleton+ (list +inactive+))
(defvar +ngram-filler+ '*)

(defclass generative-model (dag)
  ((training? :accessor training? :initform nil)
   (observed :reader observed :initform nil :type 'list)
   (constraints :reader constraints :type 'hashtable)
   (distributions :reader distributions :type 'hashtable)))

(defclass dag ()
  ((vertices :initarg :vertices :accessor vertices)
   (edge-table :initarg :edge-table :reader edge-table :type 'hastable)))

(defmethod edges ((m dag) vertex)
  (gethash vertex (edge-table m)))

(define-condition dag-contains-cycle (error) ())

(defmethod topological-sort ((graph dag)
			     &optional
			       (vertices (vertices graph))
			       (visited (make-hash-table))
			       result)
  "Sort the vertices of the graph represented by EDGES topologically. Return a list
of vertex indices."
  (if (null vertices) result
      (let ((vertex (car vertices))
	    (remaining (cdr vertices)))
	(topological-sort graph remaining visited (visit graph vertex visited result)))))

(defmethod visit ((graph dag) vertex visited result)
  "A node is a CONS whose CDR is its children and whose CAR is another
CONS the CAR of which is its mark and whose CDR is the a feature index."
  (let ((children (edges graph vertex)))
    (case (gethash vertex visited :no)
      (:yes result)
      (:in-progress
       (error 'dag-contains-cycle))
      (:no
       (progn
	 (setf (gethash vertex visited) :in-progress)
	 (let ((result (topological-sort graph children visited result)))
	   (setf (gethash vertex visited) :yes)
	   (cons vertex result)))))))

(defun constraint-argument (v)
    "Given a keyword representation of a variable, return
its congruency constraint function symbol. For example,
given :X, return $X"
  (intern (format nil "$~A" (symbol-name v))))

(defun getarg (key state)
  "Given a keyword representation of a variable, obtain
its value from a model state."
  (gethash key state))
;;(getf state key))

(defun apriori (v)
  (intern (format nil "^~A" (symbol-name v)) :keyword))

(defun basename (s)
  "Given an a priori version of a variable name, return its
stem. For example, if S is :^X, (BASENAME S) is :X."
  (intern (subseq (symbol-name s) 1) :keyword))

(defun get-horizontal-arguments (arglist)
  (loop for arg in arglist
     if (eq (elt (symbol-name arg) 0) #\^)
       collect (basename arg)))

(defun get-vertical-arguments (arglist)
  (loop for arg in arglist
     if (not (eq (elt (symbol-name arg) 0) #\^))
     collect (intern (symbol-name arg) :keyword)))

;; Constraint definition macros

(defmacro v (self parents constraint)
  (declare (ignorable self)) ;; Remove?
  `(lambda (model %state)
     (declare (ignorable model))
     (let (,@(loop for v in parents collect
		  (list (constraint-argument v) `(getarg ,v %state))))
       ,constraint)))

(defmacro recursive (self parents
		     constraint initialization-constraint)
  (let ((previous-self (apriori self)))
    (unless (member previous-self parents)
      (error "Recursive variable ~A must contain ~A in its parents."
	     self previous-self))
    `(v ,self ,parents
       (if (eq ,(constraint-argument previous-self) +inactive+)
	   ,initialization-constraint
	   ,constraint))))

(defmacro one-shot (self parents constraint)
  `(recursive ,self ,parents (list ,(constraint-argument (apriori self))) ,constraint))

(defmacro accumulator (self parents constraint initialization-constraint)
  `(recursive ,self ,parents
	      (mapcar (lambda (s) (cons s ,(constraint-argument (apriori self))))
		      ,constraint)
	      (mapcar #'list ,initialization-constraint)))

(defmacro ngram-accumulator (self parents constraint initialization-constraint n)
  `(recursive ,self ,parents
	      (mapcar (lambda (s)
			(cons s (subseq ,(constraint-argument (apriori self)) 0 (1- ,n))))
		      ,constraint)
	      (mapcar (lambda (s)
			(cons s (loop repeat (1- ,n) collect +ngram-filler+)))
		      ,initialization-constraint)))

;; Model definition 

(defmacro defmodel (class superclasses direct-slots variable-specs distribution-specs)
  (let* ((edges (make-hash-table))
	 (variables) (constraints) (distributions))
    (loop for specification in variable-specs do
	 (destructuring-bind (type v parents &rest args)
	     specification
	   (push v variables)
	   (setf (gethash v edges) parents)
	   (push (macroexpand `(,type ,v ,parents ,@args))
		 constraints)))
        (loop for specification in distribution-specs do
	 (destructuring-bind (type v parents &rest args)
	     specification
	   (setf (gethash v edges)
		 (union (gethash v edges) parents))
	   (push `(,v (make-instance ',type :arguments (list ,@parents)
				     :variable ,v ,@args))
		 distributions)))
;;	   (setf (gethash v constraints)
;;		 (macroexpand `(,type ,v ,parents ,@args)))))
    `(defclass ,class ,superclasses
       ((distributions :initform (list ,@(apply #'append (reverse distributions))))
	(vertices :initform (list ,@(reverse variables)))
	(edge-table :initform ,edges)
	(constraints :initform (list ,@(reverse constraints)))
	,@direct-slots))))

;; Model mechanics

(defmethod constraint ((m generative-model) variable)
  "Return the congruency constraint associated with VARIABLE in model M."
  (let ((p (position variable (vertices m))))
    (elt (constraints m) p)))

(defmethod distribution ((m generative-model) variable)
  (getf (distributions m) variable (make-instance 'uniform)))

(defmethod marginal-params ((m generative-model))
  (let* ((horizontal-dependencies
	 (mapcar (lambda (v) (get-horizontal-arguments (edges m v))) (vertices m))))
    (remove-duplicates (apply #'append horizontal-dependencies))))

(defmethod a-priori-congruent ((m generative-model) variable parents-state)
  (funcall (constraint m variable) m parents-state))

(defmethod a-posteriori-congruent ((m generative-model) variable parents-state moment a-priori)
  "Return the a posteriori congruent states of VARIABLE."
  (let ((value (getf moment variable 'notfound)))
    (if (member variable (observed m))
	(progn
	  (when (eq value 'notfound)
	    (warn "Observed attribute ~A not set in moment." variable))
	  (list value))
	a-priori)))

(defmethod generate-states ((m generative-model) vertices previous-state moment)
  (let* ((parent-states (if (null (cdr vertices))
			    (list previous-state)
			    (generate-states m (cdr vertices) previous-state moment)))
	 (vertex (car vertices))
	 (training? (training? m))
	 (new-states))
    (dolist (parents-state parent-states new-states)
      (let* ((probability (gethash :probability parents-state))
	     (a-priori (a-priori-congruent m vertex parents-state))
	     (a-posteriori (a-posteriori-congruent m vertex parents-state moment a-priori))
	     (distribution (unless training?
			     (probabilities (distribution m vertex)
							parents-state
							a-priori))))
	(when (eq (length a-priori) 0)
	  (warn "~A has no a priori congruent states." vertex))
	(when (and training? (> (length a-posteriori) 1))
	  (warn "Don't know what to do with multiple a posteriori 
congruent states of ~A during training." vertex))
	(when (eq (length a-posteriori) 0)
	  (warn "~A has no a posteriori congruent states." vertex))
	(observe (distribution m vertex) parents-state a-posteriori training?)
	(dolist (s a-posteriori)
	  (let ((s-probability (unless training? (gethash s distribution)))
		(new-state (copy-hash-table parents-state)))
	    (setf (gethash vertex new-state) s)
	    (unless training?
	      (setf (gethash :probability new-state) (pr:mul probability s-probability)))
	    (push new-state new-states)))))))

(defmethod rotate-state ((m generative-model) state &key (keep-trace? t))
  "\"Rotate\" a state. In the a priori version of a state, every parameter
:X is renamed :^X and variables of the form :^X in STATE are dropped."
  (let ((new-state (make-hash-table)))
    (setf (gethash :probability new-state) (gethash :probability state))
    (when keep-trace?
      (let ((trace (make-hash-table)))
	(dolist (key (cons :trace (marginal-params m)))
	  (setf (gethash key trace) (gethash key state)))
	(setf (gethash :trace new-state) trace)))
    (dolist (variable (marginal-params m) new-state)
      (setf (gethash (apriori variable) new-state) (gethash variable state)))))

(defun trace-back (state variable &optional trace)
  (let ((new-trace (cons (gethash variable state) trace))
	(previous-state (gethash :trace state)))
    (if (null previous-state)
	new-trace
	(trace-back previous-state variable new-trace))))

(defmethod root-state ((m generative-model))
  (let ((state (make-hash-table)))
    (setf (gethash :probability state) (pr:in 1))
    (dolist (variable (mapcar #'apriori (marginal-params m)) state)
      (setf (gethash variable state) +inactive+))))

(defun marginalize (states variables &optional (marginal (make-hash-table :test #'equal)))
  (dolist (state states)
    (let* ((state-prob (gethash :probability state))
	   (trace (gethash :trace state))
	   (key (loop for v in variables collect (gethash v state)))
	   (marginal-prob (gethash key marginal))
	   (new-prob (if (null marginal-prob) (car state-prob)
			 (pr:add (car state-prob) marginal-prob))))
      (setf (gethash key marginal) (cons new-prob trace)))))

(defun marginal->states (marginal variables)
  (let ((states))
    (maphash (lambda (state-key p)
	       (let ((state (make-hash-table)))
		 (setf (gethash :probability state) (car p))
		 (setf (gethash :trace state) (cdr p))
		 (loop for v in variables
		    for s in state-key do
		      (setf (gethash v state) s))
		 (push state states)))
	     marginal)
    states))

(defmethod moment ((m generative-model) moment congruent-states)
  (let ((marginal (make-hash-table :test #'equal))
	(marginal-variables (mapcar #'apriori (marginal-params m))))
    (dolist (previous-state congruent-states)
      (let* ((new-states (model-congruent-states m previous-state moment))
	     (marginal-states (mapcar (lambda (s) (rotate-state m s)) new-states)))
	(marginalize marginal-states marginal-variables marginal)))
    (marginal->states marginal marginal-variables)))

(defmethod model-congruent-states ((m generative-model) previous-state moment)
  "Call GENERATE-STATES on topologically sorted list of variables, from which
inactive variables have been pruned."
  (generate-states m (get-vertical-arguments
		      (topological-sort m)) previous-state
		      moment))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))


