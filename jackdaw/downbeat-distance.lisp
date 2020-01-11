(cl:in-package #:jackdaw)

(defmodel downbeat-distance (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain)
   (meter-domain :reader meter-domain)
   (training? :initarg :training? :reader training?))
  ((M (^m)
      (one-shot (meter-domain model))
      :inputs (period pulses) :posterior-constraint
      (recursive t (equal $m (list <period <pulses))))
   (B (^b m)
      (accumulator (loop for ioi in (ioi-domain model) collect
			(+ (mod (car $^b) (car $m)) ioi))
		   (loop for phase below (car $m) collect phase))
      :inputs (ioi)
      :posterior-constraint
      (recursive (eq (car $b) (+ (mod (car $^b) (car $m)) <ioi)) t))
   (B-out (b) (deterministic (car $b)))
   (M-out (m) (deterministic (format nil "~a/~a" (car $m) (cadr $m))))
   (P0 (^p0 b) ;; Observe this to constrain the first phase.
       (one-shot (list (car $b)))
       :inputs (ioi) :posterior-constraint (recursive t (eq $p0 <ioi))))
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
  (assert (or (null meter-params)
	      (< (abs (- (apply #'+ (mapcar #'cadr meter-params)) 1)) 1e-10))
	  nil "Meter probabilities do not sum to approximately one.")
  (let ((ppm-dist (distribution m 'b)))
    (setf (training? ppm-dist) (training? m)
    (setf (slot-value ppm-dist 'mixtures) mixtures
	  (slot-value ppm-dist 'escape) escape
	  (slot-value ppm-dist 'update-exclusion) update-exclusion
	  (slot-value ppm-dist 'order-bound) order-bound)))
  (if (training? m) (funcall #'hide m) (funcall #'hide m '(M P0))) 
  (warn "Training is ~A." (if (training? m) "ON" "OFF"))
  (setf (slot-value m 'meter-domain) (mapcar #'car meter-params))
  (loop for (meter probability) in meter-params do
       (set-param (distribution m 'M) nil meter probability)))

;; test reading and writing
;; why is generate state ran more than once? or is it? 
