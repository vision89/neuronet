(defpackage neuronet
  (:use :cl :lla)
  (:import-from :neuronet_utilities :REMOVE_ASSOC :REPLACE_ASSOC :RANDMATRIX)
  (:export :MAKE_NEURONET :ADD_LAYER
                :ADD_RANDOM_LAYER :SET_ACTIVATION :SET_D_ACTIVATION
                :SET_LEARNING_RATE :GET_LAYERS :GET_ACTIVATION
                :GET_D_ACTIVATION :GET_LEARNING_RATE))

(defconstant MAX_RAND 1.0)

(defun SIGMOID (z)
  "Sigmoid function"
  (/ 1 (+ 1 (exp (- z)))))

(defun D_SIGMOID (z)
  "Derivative of sigmoid function"
  (let ((sig_of_z (sigmoid z)))
    (* sig_of_z (- 1 sig_of_z))))

(defun MAKE_NEURONET (&optional (activation #'SIGMOID) (d_activation #'D_SIGMOID)
                                (learning_rate 0.01) (layers nil))
  (pairlis '(activation d_activation learning_rate layers)
           `(,activation ,d_activation ,learning_rate ,layers)))

(defun ADD_LAYER (layer nn)
  (let ((current_layers (GET_LAYERS nn)))
    (REPLACE_ASSOC 'layers (cons layer current_layers) nn)))

(defun ADD_RANDOM_LAYER (rows cols nn)
  (let ((current_layers (GET_LAYERS nn)))
    (REPLACE_ASSOC 'layers (cons (RANDMATRIX rows cols MAX_RAND) current_layers) nn)))

(defun SET_ACTIVATION (activation nn)
  "Set the activation in the nn"
  (REPLACE_ASSOC 'activation activation nn))

(defun SET_D_ACTIVATION (d_activation nn)
  "Set the activation derivative in the nn"
  (REPLACE_ASSOC 'd_activation d_activation nn))

(defun SET_LEARNING_RATE (learning_rate nn)
  "Set the learning rate in the nn"
  (REPLACE_ASSOC 'learning_rate learning_rate nn))

(defun GET_LAYERS (nn)
  "Get the layers"
  (cdr (assoc 'layers nn)))

(defun GET_ACTIVATION (nn)
  "Get the activation"
  (cdr (assoc 'activation nn)))

(defun GET_D_ACTIVATION (nn)
  "Get the activation derivative"
  (cdr (assoc 'd_activation nn)))

(defun GET_LEARNING_RATE (nn)
  "Get the learning rate"
  (cdr (assoc 'learning_rate nn)))
