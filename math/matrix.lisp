;;;; Math file: for opengl/physics math helper functions

(declaim (optimize (speed 3))) ;; debug 0 safety 0

(defpackage :matrix
  (:use :cl :vec)
  (:shadow common-lisp::identity common-lisp::+ common-lisp::* common-lisp::apply
           vec::- vec::+ vec::scale)
  (:export
   :matrix
   :identity :scale :rotate :translate
   :look-at :perspective :detailed-perspective
   :+ :* :apply))

(in-package :matrix)

;;; Matrix Details
;; The matrix is defined as a struct because structs will allow for more optimization later on
;; Data is stored in a row-major order
(declaim (inline make-matrix))
(defstruct matrix
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (data #() :type (vector float)))

;;; This function is utility and *not* exported by the package
(declaim (inline mref))
(defun mref (matrix i j)
  "Gets element at row i, col j, does no bounds-checking!"
  (aref (matrix-data matrix) (cl:+ j (cl:* i (matrix-cols matrix)))))
(defun mref-upd (matrix i j new)
  (setf (aref (matrix-data matrix) (cl:+ j (cl:* i (matrix-cols matrix)))) new))
(defsetf mref mref-upd)

;;; these functions perform matrix operations
(defun * (&rest args)
  "Performs standard matrix multiplication for n x m and m x k matrices"

  (flet ((two-times (left right)
           (assert (= (matrix-cols left) (matrix-rows right)))
           (let ((out (make-matrix
                       :rows (matrix-rows left)
                       :cols (matrix-cols right)
                       :data (make-array (cl:* (matrix-rows left) (matrix-cols right))
                                         :initial-element 0.0 :element-type 'float))))
              (loop for i from 0 to (cl:- (matrix-rows left) 1) do
                (loop for j from 0 to (cl:- (matrix-cols right) 1) do
                  (loop for k from 0 to (cl:- (matrix-cols left) 1) do
                    (incf (mref out i j)
                          (cl:*
                           (mref left i k)
                           (mref right k j))))))
              out)))
    (reduce #'two-times args)))


(defun + (left right)
  "Adds two matrices together: Assumes that they are 4x4"
  (declare (type matrix left right))
  (assert (and (= (matrix-cols left) (matrix-cols right))
               (= (matrix-rows left) (matrix-rows right))))
  (let ((out (make-matrix
              :rows (matrix-rows left)
              :cols (matrix-cols left)
              :data (make-array (cl:* (matrix-rows left) (matrix-cols left))
                                :initial-element 0.0))))
    (loop for i from 0 to (matrix-rows left) do
      (loop for j from 0 to (matrix-cols left) do
        (setf (mref out i j) (cl:+ (mref left i j) (mref right i j)))))))


(defun apply (matrix vector)
  "Multiplies a nxn matrix and a nx1 vector"

  (declare (type matrix matrix) (type vector vector))
  (assert (= (matrix-cols matrix) (length vector)))

  (let ((result (make-array (matrix-rows matrix) :initial-element 0.0 :element-type 'float)))
    (loop for i from 0 to (matrix-rows matrix) do
      (setf (aref result i)
            (loop for j from 0 to (length vector)
                  summing (cl:* (mref matrix i j) (aref vector j)) into val
                  finally (return val)))
            finally (return result))))





;;; These functions are matrix generation functions, which create matrices of various types
(defun identity (dim)
   (let ((matrix (make-matrix
                  :rows dim
                  :cols dim
                  :data (make-array (cl:* dim dim) :initial-element 0.0 :element-type 'float))))
     (loop for i from 0 to (cl:- dim 1) do
       (setf (aref (matrix-data matrix) (cl:+ i (cl:* dim i))) 1.0))
     matrix))

(defun scale (scale)
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let ((x (elt scale 0)) (y (elt scale 1)) (z (elt scale 2)))
     (vector x 0.0 0.0 0.0
             0.0 y 0.0 0.0
             0.0 0.0 z 0.0
             0.0 0.0 0.0 1.0))))

(defun translate (pos)
  (declare (type (vector number 3) pos))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let ((x (elt pos 0)) (y (elt pos 1)) (z (elt pos 2)))
     (vector 1.0 0.0 0.0 x
             0.0 1.0 0.0 y
             0.0 0.0 1.0 z
             0.0 0.0 0.0 1.0))))

(defun rotate (angles)
  (declare (type (vector number 3) angles))
  (make-matrix 
   :rows 4
   :cols 4
   :data
   (let ((x (elt angles 0))
         (y (elt angles 1))
         (z (elt angles 2)))
     (vector (cl:* (cos z) (cos y))
             (cl:- (cl:* (cos z) (sin y) (sin x)) (cl:* (sin z) (cos x)))
             (cl:+ (cl:* (cos z) (sin y) (cos x)) (cl:* (sin z) (sin x)))
             0.0
             (cl:* (sin z) (cos y))
             (cl:+ (cl:* (sin z) (sin y) (sin x)) (cl:* (cos z) (cos z)))
             (cl:- (cl:* (sin z) (sin y) (cos x)) (cl:* (cos z) (sin x)))
             0.0
             (cl:- (sin y))
             (cl:* (cos y) (sin x))
             (cl:* (cos y) (cos x))
             0.0
             0.0
             0.0
             0.0
             1.0))))


(defun look-at (position direction up)
  (declare (type (vector number 3) position direction up))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let* ((camera-direction direction)
          (camera-right (normalize (cross up camera-direction)))
          (camera-up (cross camera-direction camera-right)))
     (vector (elt camera-right 0)
             (elt camera-right 1)
             (elt camera-right 2)
             (dot camera-right (vec:scale -1 position))
             (elt camera-up 0)
             (elt camera-up 1)
             (elt camera-up 2)
             (dot camera-up (vec:scale -1 position))
             (elt camera-direction 0)
             (elt camera-direction 1)
             (elt camera-direction 2)
             (dot camera-direction (vec:scale -1 position))
             0.0
             0.0
             0.0
             1.0))))


(defun perspective (fov aspect near far)
  "Returns a matrix which corresponds to matrix projection"

  (declare (type number fov aspect near far))
  (assert (> fov 0.0))
  (assert (not (= aspect 0.0)))

  (make-matrix
   :rows 4
   :cols 4
   :data
   (let* ((ti (/ 1.0 (tan (/ fov 2.0))))
          (ri (cl:* ti aspect)))
     (vector ti 0.0 0.0 0.0
             0.0 ri 0.0 0.0
             0.0 0.0 (/ (cl:- (cl:+ far near)) (cl:- far near)) (/ (cl:* -2 far near) (cl:- far near))
             0.0 0.0 -1.0 0.0))))

(defun detailed-perspective (top bottom left right near far)
  "Produces a perspective transform matrix"
  (declare (type number top bottom left right near far))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (vector (/ (cl:* 2 near) (cl:- right left))
           0.0
           (/ (cl:+ right left) (cl:- right left))
           0.0

           0.0
           (/ (cl:* 2 near) (cl:- top bottom))
           (/ (cl:+ top bottom) (cl:- top bottom))
           0.0

           0.0
           0.0
           (/ (cl:+ near far) (cl:- near far))
           (/ (cl:* 2 far near) (cl:- near far))

           0.0
           0.0
           -1.0
           0.0)))


