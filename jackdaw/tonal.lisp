(cl:in-package #:jackdaw)

(defun cartesian-product (&rest lists)
  "Computes the cartesian product of <lists>."
  (if (null lists)
      (list nil)
      (let ((list (car lists))
	    (lists (cdr lists)))
	(let ((cartesian-product-of-lists (apply #'cartesian-product lists)))
	  (mapcan #'(lambda (x)
		      (mapcar #'(lambda (y) (cons x y))
			      cartesian-product-of-lists))
		  list)))))

(defparameter *scale-degrees* (loop for sd below 12 collect sd))
(defparameter *tonics* (loop for sd below 12 collect sd))
(defparameter *modes* '(0 5))

(defmodel tonal (generative-model)
  ()
  ((M (^m) (one-shot *modes*)
      :inputs (mode)
      :posterior-constraint (eq <mode $m))
   (T (^t m) (one-shot *tonics*)
      :inputs (keysig)
      :posterior-constraint
      (recursive t
		 (eq $t (if (> <keysig 0)
			    (mod (+ (* <keysig 7) $m) 12)
			    (mod (+ (* (- <keysig) 5) $m) 12)))))
   (S (^s t) (accumulator *scale-degrees*)
	      :inputs (pitch)
	      :posterior-constraint
	      (normal (eq (mod (- <pitch $t) 12) $s))))
  ((S (m) (accumulator))
   (M () (categorical))))

;; The interval model is more minimalist as it does not need to keep track of a tonic.
;; However, it throws away the first scale degree. Unless...
;; - by convention the first moment is empty
;; - pairs of scale degrees are stored (but this is wasteful)
;; - a pair of scale degrees is produced only for the first moment (current solution)
;; - a mechanism exists for not "consuming" a moment

(defmodel tonal-interval (generative-model) 
  ()
  ((M (^m) (one-shot *modes*)
      :inputs (mode)
      :posterior-constraint (eq <mode $m))
   (S (^s) (accumulator *scale-degrees*
			(cartesian-product *scale-degrees* *scale-degrees*))
	      :inputs (interval)
	      :posterior-constraint 
	      (recursive (eq (mod (+ $^s <interval) 12) <interval)
			 (eq (mod (+ (car $s) (cadr $s)) 12) <interval))))
    ((S (m) (accumulator))
     (M () (categorical))))


