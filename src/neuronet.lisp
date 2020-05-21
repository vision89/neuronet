(defpackage neuronet
  (:use :cl :lla)
  (:import-from :neuronet_utilities :REMOVE_ASSOC :REPLACE_ASSOC :RANDMATRIX)
  (:export :MAKE_NEURONET :ADD_LAYER
                :ADD_RANDOM_LAYER :SET_ACTIVATION :SET_D_ACTIVATION
                :SET_LEARNING_RATE :GET_LAYERS :GET_ACTIVATION
                :GET_D_ACTIVATION :GET_LEARNING_RATE :GET_LAST
                :REMOVE_LAST))

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

(defun SET_LAYERS (layers nn)
  "Set the layers in the neural network"
  (REPLACE_ASSOC 'layers layers nn))

(defun ADD_LAYER (layer nn)
  "Add a layer to the neural network"
  (let ((current_layers (GET_LAYERS nn)))
    (REPLACE_ASSOC 'layers (cons layer current_layers) nn)))

(defun ADD_RANDOM_LAYER (rows cols nn)
  "Add a layer (random matrix of rows * cols) to the neural network"
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

;;; Multiplies a vector by a matrix
;;; If this were the input layer being multiplied by the
;;; first hidden layer of 4 nodes it would work like so
;;; (n1 n2 n3) * ((w1n1 w2n1 w3n1 w4n1) (w1n2 w2n2 w3n2 w4n2) (w1n3 w2n3 w3n3 w4n3))
;;; So if you create the layer using 4 nodes with a value for each of the inputs
;;; ie ((h11 h12 h13) (h21 h22 h23) (h31 h32 h33) (h41 h42 h43)) then you will need
;;; to call TRANSPOSE_M on the matrix before multiplying
(defun MULTIPLY_V_M (vec_list mat_list)
  "Multiply a vector by a matrix"
  (lla:mm (coerce vec_list 'vector)
          (make-array (list (length mat_list) (length (car mat_list)))
                      :initial-contents mat_list)))

(defun TRANSPOSE_M (lol)
  "Transposes a matrix in list form"
  (apply #'mapcar #'list lol))

(defun ACTIVATE_LAYER (input activation)
  "Activate all elements in a list"
  (map 'list activation input))

(defun FEED_FORWARD_MULTIPLIER (input activation layers)
  "Performs the act of multiplying each layer and applying the activation"
  (cond ((null layers) input)
    (t (FEED_FORWARD_MULTIPLIER
        (ACTIVATE_LAYER (coerce (MULTIPLY_V_M input (TRANSPOSE_M (car layers))) 'list) activation)
        activation (cdr layers)))))

(defun FEED_FORWARD (input nn)
  "Starts the act of feeding forward the input"
  (FEED_FORWARD_MULTIPLIER input (GET_ACTIVATION nn) (GET_LAYERS nn)))

(defun BACKPROPOGATE (training_input training_output nn)
  (let ((output (FEED_FORWARD training_input nn)))

  ))
