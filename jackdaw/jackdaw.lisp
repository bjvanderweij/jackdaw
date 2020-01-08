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

(defparameter *sequence* nil)
(defparameter *event* nil)


(defmacro defreader ((type model data) &body body)
  `(defmethod read-model ((,model ,type) stream)
     (let ((,data (read stream)))
       ,@body)))

(defmacro defwriter ((type model) data)
  `(defmethod write-model ((,model ,type) &optional stream)
     (let ((data ,data))
       (unless (null stream) (write data :stream stream))
       data)))
     

(defclass generative-model (dag)
  ((training? :accessor training? :initform nil)
   (output :accessor output :initarg :output :initform nil)
   (outputvars :accessor outputvars :initarg :outputvars :initform nil)
   (hidden :accessor hidden :initform nil)
   (observed :reader observed :initform nil :type 'list)
   (prior-constraints :reader prior-constraints :type 'hashtable)
   (posterior-constraints :reader posterior-constraints :type 'hashtable)
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

(defun input-argument (i)
  "Given an input identifier, return
its congruency constraint function symbol. For example,
given :X, return <X"
  (intern (format nil "<~A" (symbol-name i))))

(defun tokeyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun getarg (key state)
  "Given a variable identifier, obtain
its value from a model state."
  (cond
    ((hash-table-p state)
     (gethash key state))
    ((listp state)
     (getf state (tokeyword key)))))
;;(getf state key))


(defun apriori (v)
  (intern (format nil "^~A" (symbol-name v)) :jackdaw))

(defun basename (s)
  "Given an a priori version of a variable name, return its
stem. For example, if S is :^X, (BASENAME S) is :X."
  (intern (subseq (symbol-name s) 1) :jackdaw))

(defun get-horizontal-arguments (arglist)
  (loop for arg in arglist
     if (eq (elt (symbol-name arg) 0) #\^)
       collect (basename arg)))

(defun get-vertical-arguments (arglist)
  (loop for arg in arglist
     if (not (eq (elt (symbol-name arg) 0) #\^))
     collect arg))

;; Constraint definition macros

(defmacro make-constraint (self parents constraint &optional inputs)
  `(lambda (model %state &optional moment ,(constraint-argument self))
     (declare (ignorable model moment ,(constraint-argument self)))
     (let* (,@(loop for v in parents collect
		   (list (constraint-argument v) `(getarg ',v %state)))
	    ,@(when (member (apriori self) parents)
		    `(($^self ,(constraint-argument (apriori self)))))
	      ,@(loop for i in inputs collect
		     (list (input-argument i) `(getarg ',i moment))))
	      ,constraint)))

(defmacro deterministic (congruent-value) `(list ,congruent-value))

(defmacro normal (constraint) constraint)

(defmacro recursive (constraint initialization-constraint)
  `(if (eq $^self +inactive+) ,initialization-constraint ,constraint))

(defmacro one-shot (constraint)
  `(recursive (list $^self) ,constraint))

(defmacro accumulator (constraint initialization-constraint)
  `(recursive
    (mapcar (lambda (s) (cons s $^self)) ,constraint)
    (mapcar #'list ,initialization-constraint)))

(defmacro ngram-accumulator (constraint initialization-constraint n)
  `(recursive 
    (mapcar (lambda (s) (cons s (subseq $^self 0 (1- ,n)))) ,constraint)
    (mapcar (lambda (s)	(cons s (loop repeat (1- ,n) collect +ngram-filler+)))
	    ,initialization-constraint)))

;; Model definition 

(defmacro defmodel (class superclasses direct-slots variable-specs distribution-specs)
  (let* ((edges (make-hash-table))
	 (variables) (posterior-constraints) (prior-constraints)
	 (distributions))
    (loop for specification in variable-specs do
	 (destructuring-bind
	       (v parents prior-constraint &key posterior-constraint inputs)
	     specification
	   (push v variables)
	   (setf (gethash v edges) parents)
	   (let ((prior-constraint (macroexpand `,prior-constraint))
		 (posterior-constraint (macroexpand `,posterior-constraint)))
	     (push (macroexpand `(make-constraint ,v ,parents ,prior-constraint))
		   prior-constraints)
	     (push
	      (unless (null posterior-constraint)
		(macroexpand `(make-constraint ,v ,parents ,posterior-constraint ,inputs)))
	      posterior-constraints))))
    (loop for specification in distribution-specs do
	 (destructuring-bind (type v parents &rest args)
	     specification
	   (setf (gethash v edges)
		 (union (gethash v edges) parents))
	   (push `(,v (make-instance ',type :arguments (list ,@parents)
				     :variable ,v ,@args))
		 distributions)))
    `(defclass ,class ,superclasses
       ((distributions :initform (list ,@(apply #'append (reverse distributions))))
	(vertices :initform '(,@(reverse variables)))
	(edge-table :initform ,edges)
	(prior-constraints :initform (list ,@(reverse prior-constraints)))
	(posterior-constraints :initform (list ,@(reverse posterior-constraints)))
	,@direct-slots))))

;; Model mechanics

(defmethod prior-constraint ((m generative-model) variable)
  "Return the congruency constraint associated with VARIABLE in model M."
  (let ((p (position variable (vertices m))))
    (elt (prior-constraints m) p)))

(defmethod posterior-constraint ((m generative-model) variable)
  "Return the congruency constraint associated with VARIABLE in model M."
  (let ((p (position variable (vertices m))))
    (elt (posterior-constraints m) p)))

(defmethod hidden? ((m generative-model) variable)
  (member variable (hidden m)))

(defmethod hide ((m generative-model) &rest variables)
  "Hide VARIABLES. Make VARIABLES the only hidden variables. 
To observe everything, call without variables."
  (setf (hidden m) variables))

(defmethod distribution ((m generative-model) variable)
  (getf (distributions m) variable (make-instance 'uniform)))

(defmethod marginal-params ((m generative-model))
  (let* ((horizontal-dependencies
	 (mapcar (lambda (v) (get-horizontal-arguments (edges m v))) (vertices m))))
    (remove-duplicates (apply #'append horizontal-dependencies))))

(defmethod a-priori-congruent ((m generative-model) variable parents-state)
  (funcall (prior-constraint m variable) m parents-state))

(defmethod a-posteriori-congruent ((m generative-model) variable parents-state moment state)
  "Return the a posteriori congruent states of VARIABLE."
  (let ((constraint (posterior-constraint m variable)))
    (if (not (or (null constraint) (hidden? m variable)))
	(funcall constraint m parents-state moment state)
	t)))

(defmethod generate-states ((m generative-model) variables previous-state moment)
  (let* ((parent-states (if (null (cdr variables))
			    (list previous-state)
			    (generate-states m (cdr variables) previous-state moment)))
	 (variable (car variables))
	 (training? (training? m))
	 (final? (eq (length variables) (length (vertices m)))) ;; warning: bit of a hack
	 (new-states))
    (format t "Generating ~a.~%" variable)
    (dolist (parents-state parent-states new-states)
      (let* ((probability (gethash :probability parents-state))
	     (a-priori (a-priori-congruent m variable parents-state))
	     (a-posteriori (a-posteriori-congruent m variable parents-state moment a-priori))
	     (distribution (unless training?
			     (probabilities (distribution m variable)
					    parents-state
					    a-priori))))
	(when (eq (length a-priori) 0)
	  (warn "~A has no a priori congruent states." variable))
	(when (and training? (> (length a-posteriori) 1))
	  (warn "Don't know what to do with multiple a posteriori 
congruent states of ~A during training." variable))
;;	(when (eq (length a-posteriori) 0)
;;	  (warn "~A has no a posteriori congruent states." variable))
	;(observe (distribution m variable) parents-state a-posteriori training?)
	(dolist (s a-priori)
	  (let ((congruent (a-posteriori-congruent m variable parents-state moment s))
		(new-state (copy-hash-table parents-state)))
	    (setf (gethash variable new-state) s)
	    (when final? (write-state m new-state congruent))
	    (when congruent
	      (let ((s-probability (unless training? (gethash s distribution))))
		(when (null s-probability)
		  (warn "State ~a not found in distribution." s))
		(unless training?
		  (setf (gethash :probability new-state) (pr:mul probability s-probability)))
		(push new-state new-states)))))))))

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

(defmethod write-state ((m generative-model) state congruent)
  (format (output m) "~a,~a~{,~a~^~},~a,~a~%" *sequence* *event*
	  (loop for v in (outputvars m) collect (getarg v state))
	  congruent (gethash :probability state)))

(defun trace-back (state variable &optional trace)
  (let ((new-trace (cons (gethash variable state) trace))
	(previous-state (gethash :trace state)))
    (if (null previous-state)
	new-trace
	(trace-back previous-state variable new-trace))))

(defmethod root-state ((m generative-model))
  "The root state. Root nodes of Bayesian networks must be conditioned
on a root state."
  (let ((state (make-hash-table)))
    (setf (gethash :probability state) (pr:in 1))
    (dolist (variable (mapcar #'apriori (marginal-params m)) state)
      (setf (gethash variable state) +inactive+))))

(defun marginalize (states variables &optional (marginal (make-hash-table :test #'equal)))
  "Given a list of states, create a hash table representing a marginal distribution."
  (dolist (state states)
    (let* ((state-prob (gethash :probability state))
	   (trace (gethash :trace state))
	   (key (loop for v in variables collect (gethash v state)))
	   (marginal-prob (gethash key marginal))
	   (new-prob (if (null marginal-prob) state-prob
			 (pr:add state-prob (car marginal-prob)))))
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

(defmethod process-dataset ((m generative-model) dataset)
  (let* ((sequence (car dataset))
	 (*sequence* (car sequence)))
    (process-sequence m (cdr sequence)))
  (unless (null (cdr dataset))
    (process-dataset m (cdr dataset))))
	 
(defmethod process-sequence ((m generative-model) moments
			     &optional (event 0) (congruent-states (list (root-state m))))
  (let ((*event* event)
	(new-congruent-states (transition m (car moments) congruent-states)))
    (if (null (cdr moments))
	new-congruent-states
	(process-sequence m (cdr moments) (1+ event) new-congruent-states))))

(defmethod transition ((m generative-model) moment congruent-states)
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
  (let ((congruent-states
	 (generate-states m (get-vertical-arguments
			     (topological-sort m)) previous-state
			     moment)))
    (when (eq (length congruent-states) 0)
      (warn "Moment ~a cold not be generated." moment))
    congruent-states))

(defwriter (generative-model m)
    (let ((params))
      (loop for (var dist) on (distributions m) by #'cddr do
	   (setf (getf params var) (serialize dist)))))

(defreader (generative-model m distributions)
  (loop for (var dist) in distributions by #'caddr do
       (with-input-from-string (s (write-to-string dist))
	 (deserialize (gethash var (distributions m)) s))))

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

(defun hash-table->alist (hashtab)
  (let ((alist '()))
    (maphash #'(lambda (key value) (setq alist (cons (list key value) alist)))
             hashtab)
    alist))

(defun alist->hash-table (alist &key (test #'eql))
  (let ((hashtable (make-hash-table :test test)))
    (mapc #'(lambda (x) (setf (gethash (car x) hashtable) (cadr x)))
          alist)
    hashtable))
