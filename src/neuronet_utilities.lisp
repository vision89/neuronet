(defpackage neuronet_utilities
  (:use :cl)
  (:export
     :REMOVE_ASSOC
     :REPLACE_ASSOC
     :RANDLIST
     :RANDMATRIX
     :GET_LAST
     :REMOVE_LAST))

(defun GET_LAST (alist)
  "Get the last element in a list"
  (car (reverse alist)))

(defun REMOVE_LAST (alist)
  "Remove the last element from a list"
  (reverse (cdr (reverse alist))))

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

;;; Untested
(defmacro OP_ON_LISTS (l1 l2 op)
  "Skeloten for performing an op on the elements of two lists and returning the list"
  `(let ((return_l nil))
    (setq l (min (length ,l1) (length ,l2))
    (dotimes (n l (reverse return_l))
             (setq return_vec (cons (,op (nth n ,l1) (nth n ,l2)) return_vec))))))

;;; Untested
(defmacro VECTOR_SUBTRACT (vec1 vec2)
  "Subtracts two vectors"
  `(OP_ON_LISTS (,vec1 ,vec2 #'-)))

;  (let ((return_vec nil))
;    (setq l (min (length vec1) (length vec2))
;    (dotimes (n l (reverse return_vec))
;             (setq return_vec (cons (- (nth n vec1) (nth n vec2)) return_vec))))))

;;; Untested
(defmacro MATRIX_SUBTRACT (mat1 mat2)
  "Subtracts two matrixes"
  `(OP_ON_LISTS (,mat1 ,mat2 #'VECTOR_SUBTRACT)))

;  (let ((return_mat nil))
;    (setq l (min (length mat1) (length mat2))
;          (dotimes (n l (reverse return_mat))
;                   (setq return_mat (cons (VECTOR_SUBTRACT (nth n mat1) (nth n mat2)) return_mat))))))
