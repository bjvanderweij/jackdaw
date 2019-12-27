(defclass idyom-ppm (categorical)
  ((alphabet :reader alphabet :initform nil)
   (escape :reader escape :initform :c)
   (mixtures :reader mixtures :initform t)
   (update-exclusion :reader update-exclusion :initform nil)
   (order-bound :reader order-bound :initform nil)
   (category-models :reader category-models :initform (make-hash-table :test #'equal))
   (locations :reader locations :initform (make-hash-table)))
  (:documentation "PPM model that's also a jackdaw-native
 sequence model. The overridden fields serve to set some defaults."))

(defmethod next-sequence ((d idyom-ppm) training?)
  (loop for k being the hash-keys of (category-models d) do
       (let ((model (gethash k (category-models d))))
	 (when training? (ppm:initialise-virtual-nodes model))
	 (ppm:increment-sequence-front model))))


(defmethod observe ((d idyom-ppm) state symbol training?)
  (let* ((location-key (getarg (dist-var d) state)) ; previous state of self
	(model-key (mapcar (lambda (v) (getarg v state))
			   (remove (dist-var d) (arguments d))))
	(model (gethash model-key (category-models d)
			(initialize-ppm d)))
	(location (gethash location-key (locations d))))
    (remhash location-key (locations d))
    (ppm::add-event-to-model-dataset model symbol)
    (let* ((next-location (ppm::ukkstep model nil location symbol t)))
      (when training? (ppm::increment-counts model next-location))
      (ppm:increment-event-front model)
      (setf (gethash symbol (locations d)) next-location))))


(defmethod probabilities ((d idyom-ppm) parents-state congruent-states)
  (let* ((table (make-hash-table :test #'equal))
	 (location-key (getarg (dist-var d) parents-state)) ; previous state of self
	 (model-key (mapcar (lambda (v) (getarg v parents-state))
			    (remove (dist-var d) (arguments d))))
	 (model (gethash model-key (category-models d)))
	 (location (gethash location-key (locations d)))
	 (alphabet (mapcar #'car congruent-states)))
    (ppm:set-alphabet model alphabet)
    (dolist (p (ppm::get-distribution model location))
      (let ((symbol (car p))
	    (probability (cadr p)))
      (setf (gethash (cons symbol location-key) table) probability)))))

(defmethod initialize-ppm ((d idyom-ppm))
  (make-instance 'ppm:ppm
		 :escape (escape d)
		 :order-bound (order-bound d)
		 :mixtures (mixtures d)
		 :update-exclusion (update-exclusion d)
		 :alphabet (alphabet d)))

(defmodel downbeat-distance (generative-model)
  ((observed :initform '(:B '(:IOI))) ;; observed :variable :input
   (ioi-domain :initarg :ioi-domain :reader ioi-domain
	       :initform '(1 2 3 4))
   (meter-domain :initarg :meter-domain :reader meter-domain
		 :initform '((2 3) (3 2))))
  ((one-shot :Meter (:^meter) (meter-domain model))
   (accumulator :B (:^b :meter)
		(loop for ioi in (ioi-domain model) collect
		     (+ (mod $^b (car $meter)) ioi))
		(loop for phase below (car $meter) collect phase)
		:observe (+ (mod $^b (car $meter)) @ioi)
		:observe-init @ioi))
  ())
