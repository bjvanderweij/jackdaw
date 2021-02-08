# Documentation

Write some.

* Installation
* Training
* Testing
* Example models

# Publication

Github/gitlab.
OSF.

Models in separate repo.

Register for quicklisp quickload.

* http://blog.quicklisp.org/2015/01/getting-library-into-quicklisp.html
    * Test on multiple cl implementations: sbcl, ...
    * :license :author :description in ASDF
    * must be loadable with (asdf:load-system ...)
        * script / command to setup database?

# Code

Must haves

* Expose the right functions of various packages through packages.lisp files
* Clean up comments

Nice to haves

* Go over IDyOM changes and make some PRs
* Model definition
    * Improve inheritance
    * Remove need for initialization functions
* CLI
    * Automatically supported for defined models
* Implementation of IDyOM with various viewpoints and notebook for combining predictions
* Python library for interacting with jackdaw


* New name? jackdaw, eventual, quencese, bayes4time, time4bayes, event-prediction, generative event prediction, bayeslet, thomas, reverend
    * discrete, dynamic, syntactic, time, sequence, moments, infer, latent, hidden
* Documentation
    * Guide with some examples as well as docstrings for the most important, and all API exposed, functions
        * Basic generative models

# Frequent issues

* State cannot be generated
    * Did you remove the pickup events (event 0)?
    * Does the pickup exceed the meter's period?

