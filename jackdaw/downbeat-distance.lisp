(cl:in-package #:jackdaw)

(defclass idyom-ppm (distribution)
  ((alphabet :reader alphabet :initform nil)
   (escape :reader escape :initform :c)
   (mixtures :reader mixtures :initform t)
   (update-exclusion :reader update-exclusion :initform nil)
   (order-bound :reader order-bound :initform nil)
   (ppms :reader ppms :initform (make-hash-table :test #'equal))
   (locations :reader locations :initform (make-hash-table :test #'equal))
   (training? :accessor training? :initarg :training? :initform nil))
  (:documentation "PPM model that's also a jackdaw-native
 sequence model. The overridden fields serve to set some defaults."))

(defwriter (ppm:ppm m)
    (list :leaves (utils:hash-table->alist (ppm::ppm-leaves m))
	  :branches (utils:hash-table->alist (ppm::ppm-branches m))
	  :dataset (ppm::dataset->alist m)
	  :alphabet (ppm::ppm-alphabet m)
	  :order-bound (ppm::ppm-order-bound m)
	  :mixtures (ppm::ppm-mixtures m)
	  :escape (ppm::ppm-escape m)
	  :update-exclusion (ppm::ppm-update-exclusion m)))

(defreader (ppm:ppm m data)
  (setf (slot-value m 'ppm::leaves)
	(utils:alist->hash-table (getf data :leaves))
	(slot-value m 'ppm::branches)
	(utils:alist->hash-table (getf data :branches))
	(slot-value m 'ppm::dataset)
	(ppm::alist->dataset (getf data :dataset))
	(slot-value m 'ppm::alphabet)
	(getf data :alphabet)
	(slot-value m 'ppm::order-bound)
	(getf data :order-bound)
	(slot-value m 'ppm::mixtures)
	(getf data :mixtures)
	(slot-value m 'ppm::escape)
	(getf data :escape)
	(slot-value m 'ppm::update-exclusion)
	(getf data :update-exclusion))
    (ppm::initialize m))

(defwriter (idyom-ppm d)
    (loop for arguments being the hash-keys of (ppms d) collect
	 (cons arguments (serialize (gethash arguments (ppms d))))))

(defreader (idyom-ppm d data)
  (dolist (pair data)
    (with-input-from-string (s (write-to-string (cdr pair)))
      (setf (gethash (car pair) (ppms d))
	    (deserialize (make-instance 'ppm:ppm) s)))))

(defmethod spawn-ppm ((d idyom-ppm))
  (make-instance
   'ppm:ppm :escape (escape d) :order-bound (order-bound d)
   :mixtures (mixtures d) :update-exclusion (update-exclusion d)
   :alphabet (alphabet d)))

(defmethod next-sequence ((d idyom-ppm) training?)
  (loop for k being the hash-keys of (category-models d) do
       (let ((model (gethash k (category-models d))))
	 (when training? (ppm:initialise-virtual-nodes model))
	 (ppm:increment-sequence-front model))))

(defmethod get-location ((d idyom-ppm) (model ppm:ppm) state arguments)
  (multiple-value-bind (location found?)
      (gethash (cons state arguments) (locations d))
    (if found? location
	(let ((location
	       (if (null state) (ppm::get-root)
		   (ppm::ukkstep model nil
				 (gethash (cons (cdr state) arguments) (locations d))
				 (car state) (training? d)))))
	  (ppm::add-event-to-model-dataset model (car state))
	  (when (training? d) (ppm::increment-counts model location))
	  (ppm:increment-event-front model)
	  (setf (gethash (cons state arguments) (locations d)) location))))))

(defmethod get-model ((d idyom-ppm) arguments)
  (multiple-value-bind (model found?)
      (gethash arguments (ppms d) (spawn-ppm d))
    (unless found? (setf (gethash arguments (ppms d)) model))
    model))

(defmethod probabilities ((d idyom-ppm) parents-state congruent-states)
  (let* ((context (getarg (apriori (dist-var d)) parents-state))
	 (context (unless (eq context +inactive+) context))
	 (arguments (mapcar (lambda (v) (getarg v parents-state))
			    (remove (dist-var d) (arguments d))))
	 (model (get-model d arguments))
	 (table (make-hash-table :test #'equal))
	 (location (get-location d model context arguments))
	 (alphabet (mapcar #'car congruent-states)))
    (ppm:set-alphabet model alphabe)t
    (dolist (p (ppm::get-distribution model location))
      (let ((symbol (car p))
	    (probability (cadr p)))
	(print (cons symbol context))
	(print probability)
	(setf (gethash (cons symbol context) table) probability)))
    table))

(defmodel downbeat-distance (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain
	       :initform '(1 2 3 4))
   (meter-domain :initarg :meter-domain :reader meter-domain
		 :initform '((2 3) (3 2)))
   (hidden :initform '(m p0)))
  ((M (^m) (one-shot (meter-domain model)))
   (B (^b m)
      (accumulator (loop for ioi in (ioi-domain model) collect
			(+ (mod (car $^b) (car $m)) ioi))
		   (loop for phase below (car $m) collect phase))
      :inputs (ioi)
      :posterior-constraint
      (recursive (eq (car $b) (+ (mod (car $^b) (car $m)) <ioi)) t)))
  ((idyom-ppm 'B ('m))))

(defmodel downbeat-distance-trainable (downbeat-distance)
  ((hidden :initform '(m p0)))
  ((M (^m)
      (one-shot (meter-domain model))
      :inputs (meter)
      :posterior-constraint (recursive t (equal $m <meter)))
   (B-out (b) (deterministic (car $b)))
   (M-out (m) (deterministic (format t "~a/~a" (car $m) (cadr $m))))
   (P0 (^p0 b) ;; Observe this to constrain the first phase.
       (one-shot (list $b))
       :inputs (ioi) :posterior-constraint (recursive t (eq (car $b) <ioi))))
  ((idyom-ppm 'B ('m))))

(defmodel downbeat-distance (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain
	       :initform '(1 2 3 4))
   (meter-domain :initarg :meter-domain :reader meter-domain
		 :initform '((2 3) (3 2)))
   (hidden :initform '(m p0)))
  ((M (^m)
      (one-shot (meter-domain model))
      :inputs (meter)
      :posterior-constraint (recursive t (equal $m <meter)))
   (B (^b m)
      (accumulator (loop for ioi in (ioi-domain model) collect
			(+ (mod (car $^b) (car $m)) ioi))
		   (loop for phase below (car $m) collect phase))
      :inputs (ioi)
      :posterior-constraint
      (recursive (eq (car $b) (+ (mod (car $^b) (car $m)) <ioi)) t))
   (B-out (b) (deterministic (car $b)))
   (M-out (m) (deterministic (format t "~a/~a" (car $m) (cadr $m))))
   (P0 (^p0 b) ;; Observe this to constrain the first phase.
       (one-shot (list $b))
       :inputs (ioi) :posterior-constraint (recursive t (eq (car $b) <ioi))))
  ((idyom-ppm 'B ('m))))
;;   (categorical 'M nil)))

(defmethod toggle-training ((m downbeat-distance))
  (let ((training? (training? (distribution m 'B))))
    (setf (training? (distribution m 'B)) (not training?))
    (warn "Training is ~A." (if training? "OFF" "ON"))))

(defmethod initialize-instance :after (&key meter-params)
  )

;; after initialize should set meter distribution
;; test reading and writing
;; why is generate state ran so many times?
;; generate state should be run extra time to update model locations in training
;; make definition of distributions in model more nice

(defwriter (downbeat-distance m)
    (list (call-next-method m)
	  (ioi-domain m)
	  (meter-domain m)))

(defreader (downbeat-distance m data)
  (let ((model (first data))
	(ioi-domain (second data))
	(meter-domain (third data)))
    (with-input-from-string (s (write-to-string model))
      (deserialize m s)
      (setf (slot-value m 'ioi-domain) ioi-domain)
      (setf (slot-value m 'meter-domain) meter-domain))))


