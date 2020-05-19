(defpackage neuronet_utilities
  (:use :cl)
  (:export
     :REMOVE_ASSOC
     :REPLACE_ASSOC
     :RANDLIST
     :RANDMATRIX))

(defun REMOVE_ASSOC (k a_list)
  "Remove an association from the list"
  (loop for a in a_list
    unless (equal (car a) k) collect a))

(defun REPLACE_ASSOC (k v a_list)
  "Place association within association list"
  (acons k v (REMOVE_ASSOC k a_list)))

(defun RANDLIST (n r)
  "Get a list of n random integers"
  (loop for i below n
    collect (random r)))

(defun RANDMATRIX (row col r)
  "Creates a random matrix of size row x col"
  (loop for i below row
    collect (RANDLIST col r)))
