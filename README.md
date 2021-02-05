# Jackdaw

Common Lisp implementation of a congruency constraint-based framework for discrete dynamic Bayesian networks.

* defmodel
* generate-states
* transition
* distributions
    * probability
    * probabilities

## Model definition

How to define a generative model?
    How to define a variable?
        Which arguments need to be supplied?
        How to define a posterior constraint?
            What are inputs?
            What if no posterior constraint is supplied
    How to assign a distribution to a variable
        How to condition a distribution on other variables
    How to define custom distributions?
    How to make variables hidden or observed?

A *state* is a hash table representing a parameter of a joint distribution. The keys are variable names, values are parameter values. There are two special keys: :probability and :trace, representing the probability of the state and its parent state (or its max sum parent state?).

Well-formedness (sanity checks to be implemented):
Optimizations: HASH table size for states?

One-shot and recursive variables require recursive dependency to be present
Including argument that doesn't exist in model should raise warning
Defined but never used warnings
One-shot and accumulator etc. cannot be used as posterior constraints (types required?)
There's a grammar to how constraints can be nested. Combining one-shot and recursive doesn't make sense.
Symbol management: move each model into its own package


A state may be represented as a vector. The problem is that the items of the vector may represent any subset of variables.
Solutions:
* Vector of powersets:
    * Pros: 
        * memory: individual states need only store one integer extra
        * speed: retrieval is quick: lookup subset, 
To address this, we create a vector representing all an indexed powerset of variables. When generating states, it needs to be determined once at which index of the powerset a given set of variables occurs. The powerset vector itself may grow quite large. By convention, the first item of the vector represents the probability, the second item 

Variable names are symbols in the jackdaw package.
Arguments can have any name, except self, which is reserved to refer to the previous self.
Arguments are assigned to the symbol $[variable-name] in the constraint.
Inputs are assigned to <[variable-name]
Moments are PLISTS with :[variable-name] being the slot for a variable

Example model

```commonlisp
(defmodel downbeat-distance (generative-model)
  ((ioi-domain :initarg :ioi-domain :reader ioi-domain
	       :initform '(1 2 3 4))
   (meter-domain :initarg :meter-domain :reader meter-domain
		 :initform '((2 3) (3 2))))
  ((M (^m) (one-shot (meter-domain model)))
   (B (^b m)
      (accumulator (loop for ioi in (ioi-domain model) collect
			(+ (mod (car $^b) (car $m)) ioi))
		   (loop for phase below (car $m) collect phase))
      :inputs (ioi)
      :posterior-constraint
      (recursive (eq (car $b) (+ (mod (car $^b) (car $m)) <ioi)) t)))
  ())
```

Yields 5 hidden states each time.
Constraining the first phase to be observed limits it to two (one for each meter)

Variable names should be distinguishable by EQ
Variable states should be distinguishable by EQUAL

## Defining custom distributions

Convention: params should be stored not in log space, conversion happens later.

* Define class with params
* Provide an specialization of the probability method, which should return a probability (this time it should be in log-space)
* Alternatively, provide a specialization of the probabilities method, which takes a joint-state (dict) and variable-state (data) and returns a dictionary 
