(cl:in-package #:jackdaw)

(defclass temperley-phase-prior (distribution) ())
(defmethod probability ((d temperley-phase-prior) arguments symbol))

(defmodel enculturation-s (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain)
   (meter-domain :initarg :meter-domain :reader meter-domain)
   (training? :initarg :training? :accessor training? :initform nil))
  ((M (^m)
      (if (eq $^m '*)
	  (meter-domain model)
	  (list $^m)))
   (D (^d ^p)
      (if (eq $^d '*)
	  (list '(*))
	  (loop for ioi in (ioi-domain model)
	     collect (cons (+ $^p ioi) $^d))))
   (P (^p m d)
      (if (eq $^p *)
	  (list (mod (car $d) (car $m)))
	  (loop for phase below (car $m)
	     collect phase)))
   (IOI (d ^p)
	(if (eq $d '(*))
	    (list '*)
	    (list (- (car $d) $^p)))))
  ((D (m) (accumulator-model))
   (M () (categorical)))
  :required-fields (ioi-domain))


(defmodel enculturation (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain)
   (meter-domain :initarg :meter-domain :reader meter-domain)
   (training? :initarg :training? :accessor training? :initform nil))
  ((M (^m) (persistent (meter-domain model))
      :inputs (period pulses))
   (D (^d ^p)
      (recursive (loop for ioi in (ioi-domain model)
		       collect (cons (+ $^p ioi) $^d))
		 (deterministic '(*))))
   (P (^p m d)
      (recursive (loop for phase below (car $m)
		       collect phase)
		 (list (mod (car $d) (car $m)))))
   (IOI (d ^p)
	(recursive (list (- (car $d) $^p))
		   (list '*))
	:inputs (ioi)))
  ((D (m) (accumulator-model))
   (M () (categorical)))
  :required-fields (ioi-domain))


(defmodel classical (generative-model)
  ((tactus-intervals :initarg :tactus-intervals :reader tactus-intervals)
   (ioi-domain :initarg :ioi-domain :reader ioi-domain))
  ((U (^u) (persistent '(2 3)))
   (L (^l) (persistent '(2 3)))
   (T (^t) (persistent (tactus-intervals model)))
   (UPH (^uph u) (persistent (loop for uph below $u collect uph)))
   (TPH (^tph t) (persistent (loop for tph below $t collect tph)))
   (PH (^ph ioi u t uph tph)
       (recursive (deterministic (mod (+ $^ph $ioi) (* $u $t)))
		  (deterministic (+ $tph (* $uph $t)))))
   (IOI (^ioi ^ph)
	(recursive (ioi-domain model)
		   (deterministic '*))))
  ((U () (bernouilli :symbols '(3 2)))
   (L () (bernouilli :symbols '(3 2)))
   (T () (categorical))
   (UPH (u) (bar-phase))
   (TPH (t) (tactus-phase))
   (IOI (^ph t u l) (temperley-ioi))))

(defmethod initialize-instance :after ((m classical)
				&key u l t0 t-ph0 duple-ph0 triple-ph0 triple-ph1 note)
  "T0 must be a list of pairs containing tactus and corresponding probability."
  (setf (slot-value (distribution m 'U) 'p) u
	(slot-value (distribution m 'L) 'p) l
	(slot-value (distribution m 'UPH) 'duple) (list duple-ph0)
	(slot-value (distribution m 'UPH) 'triple) (list triple-ph0 triple-ph1)
	(slot-value (distribution m 'TPH) 'zero) t-ph0
	(slot-value (distribution m 'IOI) 'p) note
	(slot-value m 'tactus-intervals) (mapcar #'car t0))
  (hide m 'U 'L 'T 'UPH 'TPH)
  (loop for (interval p) in t0 do
       (set-param (distribution m 'T) nil interval p)))

(defmodel downbeat-distance (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain)
   (meter-domain :reader meter-domain)
   (training? :initarg :training? :reader training? :initform nil))
  ((M (^m)
      (one-shot (meter-domain model))
      :inputs (period pulses) :posterior-constraint
      (recursive t (equal $m (list <period <pulses))))
   (B (^b ^p)
      (chain
       (accumulator
	(loop for ioi in (ioi-domain model) collect (+ $^p ioi)))
       (^p))
      :output (lambda (s) (if (eq +inactive+ s) "" (car s)))
      :inputs (ioi)
      :posterior-constraint
      (chain-posterior (eq (+ $^p <ioi) (car $b)) (^p)))
   (period (m) (deterministic (car $m)))
   (pulses (m) (deterministic (cadr $m)))
   (P (^p m b) ;; Observe this to constrain the first phase.
      (recursive (deterministic (mod (car $b) (car $m)))
		 (loop for phase below (car $m) collect phase))
      :inputs (ioi)
      :posterior-constraint
      (recursive t (eq $p <ioi)))
   (P0 (^p0 p) (one-shot (deterministic $p)))
   (IOI (b ^p) (chain (deterministic (- (car $b) $^p)) (^p))))
  ((B (m) (accumulator-model))
   (M nil (categorical)))
   :required-fields (ioi-domain))

(defwriter downbeat-distance (m)
  (list (read-from-string (with-output-to-string (s) (call-next-method m s)))
	(ioi-domain m) (meter-domain m)))
(defreader downbeat-distance (m data)
  (let ((model (first data))
	(ioi-domain (second data))
	(meter-domain (third data)))
    (with-input-from-string (s (write-to-string model))
      (call-next-method m s)
      (setf (slot-value m 'ioi-domain) ioi-domain)
      (setf (slot-value m 'meter-domain) meter-domain))))

(defmethod initialize-instance :after ((m downbeat-distance)
				       &key meter-params update-exclusion
					 (mixtures t) order-bound (escape :c))
  (assert (not (member 0 (ioi-domain m))) nil
	  "0 may not be a member of the IOI domain.")
  (assert (or (null meter-params)
	      (< (abs (- (apply #'+ (mapcar #'cadr meter-params)) 1)) 1e-10))
	  nil "Meter probabilities do not sum to approximately one.")
  (let ((ppm-dist (distribution m 'b)))
    (setf (training? ppm-dist) (training? m)
	  (slot-value ppm-dist 'mixtures) mixtures
	  (slot-value ppm-dist 'escape) escape
	  (slot-value ppm-dist 'update-exclusion) update-exclusion
	  (slot-value ppm-dist 'order-bound) order-bound))
  (if (training? m) (funcall #'hide m) (funcall #'hide m 'M 'P))
  (warn "Training is ~A." (if (training? m) "ON" "OFF"))
  (setf (slot-value m 'meter-domain) (mapcar #'car meter-params))
  (loop for (meter probability) in meter-params do
       (set-param (distribution m 'M) nil meter probability)))

;; test reading and writing
;; why is generate state ran more than once? or is it? 

(defmethod initialize-instance :after ((m enculturation)
				       &key meter-params update-exclusion
					 (mixtures t) order-bound (escape :c))
  (assert (not (member 0 (ioi-domain m))) nil
	  "0 may not be a member of the IOI domain.")
  (assert (or (null meter-params)
	      (< (abs (- (apply #'+ (mapcar #'cadr meter-params)) 1)) 1e-10))
	  nil "Meter probabilities do not sum to approximately one.")
  (let ((ppm-dist (distribution m 'd)))
    (setf (training? ppm-dist) (training? m)
	  (slot-value ppm-dist 'mixtures) mixtures
	  (slot-value ppm-dist 'escape) escape
	  (slot-value ppm-dist 'update-exclusion) update-exclusion
	  (slot-value ppm-dist 'order-bound) order-bound))
  (if (training? m) (funcall #'hide m) (funcall #'hide m 'M 'P 'D 'IOI))
  (warn "Training is ~A." (if (training? m) "ON" "OFF"))
  (setf (slot-value m 'meter-domain) (mapcar #'car meter-params))
  (loop for (meter probability) in meter-params do
       (set-param (distribution m 'M) nil meter probability)))
