(cl:in-package #:cl-user)

(defpackage #:jackdaw-system (:use #:asdf #:cl))
(in-package #:jackdaw-system)

(defsystem jackdaw
  :name "jackdaw"
  :version "0.1"
  :author "Bastiaan van der Weij"
  :licence "GPL"
  :description "Congruency constraints framework for discrete dynamic Bayesian networks."
  :depends-on (fiveam closer-mop sb-md5 unix-options fare-csv)
  :serial t
  :components
  ((:module jackdaw
	    :serial t
            :components 
            ((:file "jackdaw")
             (:file "distributions")))
   (:module probs
	    :serial t 
	    :components 
	    ((:file "probabilities")))))
