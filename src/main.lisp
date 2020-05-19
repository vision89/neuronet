(defpackage main
  (:use :cl :lla)
  (:import-from :neuronet :MAKE_NEURONET :ADD_LAYER
                :ADD_RANDOM_LAYER :SET_ACTIVATION :SET_D_ACTIVATION
                :SET_LEARNING_RATE :GET_LAYERS :GET_ACTIVATION
                :GET_D_ACTIVATION :GET_LEARNING_RATE))

; (load "../../../quicklisp.lisp")
(defparameter *nn* (MAKE_NEURONET))

(setf *nn* (ADD_RANDOM_LAYER 5 4 *nn*))

(setf *nn* (ADD_RANDOM_LAYER 4 3 *nn*))

(setf *nn* (ADD_RANDOM_LAYER 3 1 *nn*))

(print *nn*)
