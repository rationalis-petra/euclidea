(defpackage :vec
  (:use :cl)
  (:shadow common-lisp::- common-lisp::+)
  (:export
   :+ :- :dot :cross :scale :normalize :magnitude
   :vec4 :vec3))

(in-package :vec)

;;; Vector operations
(declaim (optimize (speed 3))) ;; debug 0 safety 0

(defun magnitude (vec)
  (declare (type (vector number) vec))
  (loop for x across vec
        summing (* x x) into total
        finally (return (sqrt total))))

(defun scale (s vec)
  (declare (type (vector number) vec) (type number s))
  (map 'vector #'*
       (make-array (length vec) :initial-element s)
       vec))

(defun normalize (vec)
  (declare (type (vector number) vec))
  (scale (/ 1 (magnitude vec)) vec))

(defun + (left right)
  "Vector addition cl:- requires equal length vectors"
  (declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (map 'vector #'cl:+ left right))

(defun - (left right)
  "Vector subtraction cl:- requires equal length vectors"
  ;;(declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (map 'vector #'cl:- left right))

(defun dot (left right)
  "Dot product cl:- throws error when vectors have unequal length"
  (declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (reduce #'cl:+ (map 'vector #'* left right)))

(defun cross (left right)
  "Cross product for vectors of dimension 3"
  (declare (type (vector number 3) left right))
  (vector
   (cl:- (* (aref left 1) (aref right 2))
      (* (aref left 2) (aref right 1)))
   (cl:- (* (aref left 2) (aref right 0))
      (* (aref left 0) (aref right 2)))
   (cl:- (* (aref left 0) (aref right 1))
      (* (aref left 1) (aref right 0)))))



;;; Vector constructors
(defun vec4 (vec num)
  (concatenate 'vector vec (vector num)))

(defun vec3 (vec)
  (subseq vec 0 3))
