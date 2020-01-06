(cl:in-package #:jackdaw)

;;;;;;;;;;;;;;;;;;;; Probability distributions ;;;;;;;;;;;;;;;;;;;;

(defclass tactus-interval (distribution)
  ((t0 :initarg :t0 :accessor t0)))
(defclass beat-deviation (distribution)
  ((subdivision :initarg :subdivision :reader subdivision)
   (phase :initarg :phase :reader beat-phase :initform 1)
   (p :initarg :p :reader p)))
(defclass bar-phase (distribution)
  ((duple :initarg :duple :reader duple)
   (triple :initarg :triple :reader triple)))
(defclass tactus-phase (distribution)
  ((zero :initarg :zero :reader zero
	 :documentation
	 "Probability of a phase of zero. Must be > 0 and <= 1.")))
(defclass onset (distribution)
  ((p :initarg :p :reader p)))

(defmethod probability ((d tactus-interval) arguments symbol)
  (let ((previous (car arguments)))
    (pr:in (if (eq previous +inactive+)
	       (elt (t0 d) (- symbol 9)) ;(min-tactus (model d)))
	       (exp (- (expt (* 0.5 (- symbol previous )) 2)))))))

(defmethod probability ((d onset) arguments symbol)
  (let ((p (elt (p d) (car arguments))))
    (pr:in (if symbol p (- 1 p)))))

(defmethod probability ((d bar-phase) arguments symbol)
  (let ((grouping (car arguments)))
    (pr:in
     (case grouping
       (2 (elt (append (duple d) (list (- 1 (apply #'+ (duple d))))) symbol))
       (3 (elt (append (triple d) (list (- 1 (apply #'+ (triple d))))) symbol))))))

(defmethod probability ((d beat-deviation) arguments symbol)
  (if (eq symbol '*) (pr:in 1)
      (let* ((period (car arguments))
	     (center (beat-location period (subdivision d) (beat-phase d)))
	     (deviation (abs (- center symbol))))
	(pr:in (elt (p d) deviation)))))

(defmethod probability ((d tactus-phase) arguments symbol)
  (let ((interval (car arguments))
	(p-zero (zero d)))
    (pr:in
     (if (eq symbol 0) p-zero
	 (/ (- 1 p-zero) (1- interval))))))

(defun beat-location (period subdivision phase)
  (floor (/ (* phase period) subdivision)))

(defun beat-locations (max-beat-dev s period phase &optional (previous 0))
  (let ((center (beat-location period s phase)))
    (loop for p
       from (max (1+ previous) (- center max-beat-dev))
       to (min (- period (- s phase)) (+ center max-beat-dev))
       collect p)))

;;;;;;;;;;;;;;;;;;;; Congruency constraints ;;;;;;;;;;;;;;;;;;;;

(defmodel temperley (generative-model)
  ((observed :initform '(:n))
   (tacti :initarg :tacti :reader tacti
	  :initform (loop for $t from 9 below 23 collect $t))
   (max-beat-dev :initarg :max-beat-dev :reader max-beat-dev :initform 0));3))
  ((U (^u) (one-shot '(2 3)))
   (L (^l) (one-shot '(2 3)))
   (T (^t ^tph)
       (recursive (if (eq (1+ $^tph) $^t) (tacti model) (list $^t))
		  (tacti model))))
   (BPH (^bph tph u)
	(recursive (if (eq $tph 0)
		       (list (mod (1+ $^bph) $u))
		       (list $^bph))
		   (loop for bph below $u collect bph)))
   (TPH (^tph ^t t)
	(recursive (list (mod (1+ $^tph) $^t))
		   (loop for tph below $t collect tph)))
   (DB (^db tph t l)
       (recursive (cond ((eq $l 3) +singleton+)
			((eq $tph 0) (beat-locations (max-beat-dev model) 2 $t 1))
			(t (list $^db)))
		  (if (eq $l 3) +singleton+ (beat-locations (max-beat-dev model) 2 $t 1))))
   (TB1 (^tb1 tph t l)
	(recursive (cond ((eq $l 2) +singleton+)
			 ((eq $tph 0) (beat-locations (max-beat-dev model) 3 $t 1))
			 (t (list $^tb1)))
		   (if (eq $l 2) +singleton+ (beat-locations (max-beat-dev model) 3 $t 1))))
   (TB2 (^tb2 tb1 tph t l)
	(recursive (cond ((eq $l 2) +singleton+)
			 ((eq $tph 0) (beat-locations (max-beat-dev model) 3 $t 2 $tb1))
			 (t (list $^tb2)))
		   (if (eq $l 2)
		       +singleton+
		       (beat-locations (max-beat-dev model) 3 $t 2 $tb1))))
   (BS (db tb1 tb2 tph bph)
       (normal (list (cond ((and (eq $tph 0) (eq $bph 0)) 3)
			   ((eq $tph 0) 2)
			   ((member $tph (list $db $tb1 $tb2)) 1)
			   (t 0)))))
   (N (^n) (recursive (list t nil) (list t)))
  ((bernouilli :U nil :symbols '(2 3) :p .24)
   (bernouilli :L nil :symbols '(2 3) :p .22)
   (tactus-interval :T (:^t) :t0 '(.1 .2 .3 .23 .13 .03 .006 .002 .001
				   .0006 .0002 .0001 .00005 .00005))
   (bar-phase :BPH (:u) :duple '(.65) :triple '(.33 .667))
   (tactus-phase :TPH (:t) :zero .6)
   (beat-deviation :DB (:t) :subdivision 2 :p '(.32 .24 .08 .02))
   (beat-deviation :TB1 (:t) :subdivision 3 :p '(.32 .24 .08 .02))
   (beat-deviation :TB2 (:t) :subdivision 3 :phase 2 :p '(.32 .24 .08 .02))
   (onset :N (:bs) :p '(.01 .48 .74 .95))))

(defmodel symbolic-temperley (generative-model)
  ((observed :initform '(:n))
   (tacti :initarg :tacti :reader tacti
	  :initform '(4 6 8 12))
   (max-beat-dev :initarg :max-beat-dev :reader max-beat-dev :initform 0));3))
  ((one-shot :U (:^u) '(2 3))
   (one-shot :L (:^l) '(2 3))
   (one-shot :T (:^t) (tacti model)) ; changed to one-shot
   (recursive :BPH (:^bph :tph :u)
	      (if (eq $tph 0)
		  (list (mod (1+ $^bph) $u))
		  (list $^bph))
	      (loop for bph below $u collect bph))
   (recursive :TPH (:^tph :^t :t)
	      (list (mod (1+ $^tph) $^t))
	      (loop for tph below $t collect tph))
   (v :BS (:t :l :tph :bph) ; $t and $l replace the beat locations
     (list (cond ((and (eq $tph 0) (eq $bph 0)) 3)
		 ((eq $tph 0) 2)
		 ((eq (mod $tph (/ $t $l)) 0) 1)
		 (t 0))))
   (recursive :N (:^n) (list t nil) (list t)))
  ((bernouilli :U nil :symbols '(2 3) :p .24)
   (bernouilli :L nil :symbols '(2 3) :p .22)
   (tactus-interval :T (:^t) :t0 '(.25 .25 .25 .25))
   (bar-phase :BPH (:u) :duple '(.65) :triple '(.33 .667))
   (tactus-phase :TPH (:t) :zero .6)
   (onset :N (:bs) :p '(.01 .48 .74 .95))))

(defmethod moment ((m temperley) moment congruent-states)
  ;; Todo implement more efficient state generation
  (call-next-method))

(defmethod generate-states ((m temperley) vertices previous-state moment)
  ;; Todo implement more efficient state generation
  (call-next-method))
