(defpackage .
  (:use :cl :lla)
  (:import-from :neuronet :SIGMOID :D_SIGMOID :MAKE_NEURONET :ADD_LAYER
                :ADD_RANDOM_LAYER :SET_ACTIVATION :SET_D_ACTIVATION
                :SET_LEARNING_RATE :GET_LAYERS :GET_ACTIVATION
                :GET_D_ACTIVATION :GET_LEARNING_RATE))
(in-package :.)

; (load "../../../quicklisp.lisp")
