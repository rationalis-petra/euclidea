;;;; Math file: for opengl/physics math helper functions

;;; Vector operations
(declaim (optimize (speed 3))) ;; debug 0 safety 0

(defun magnitude (vec)
  ;; (declare (type vector vec))
  (loop for x across vec
        summing (* x x) into total
        finally (return (sqrt total))))

(defun vec-scale (s vec)
  ;; (declare (type vector vec) (type float s))
  (map 'vector #'*
       (make-array (length vec) :initial-element s)
       vec))

(defun vec-normalize (vec)
  ;; (declare (type vector vec))
  (vec-scale (/ 1 (magnitude vec)) vec))

(defun vec+ (left right)
  "Vector addition - requires equal length vectors"
  ;; (declare (type vector left right))
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec+ with unequal length"))
  (map 'vector #'+ left right))

(defun vec- (left right)
  "Vector subtraction - requires equal length vectors"
  ;; (declare (type vector left right))
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec- with unequal length"))
  (map 'vector #'- left right))

(defun vec-dot (left right)
  "Dot product - throws error when vectors have unequal length"
  ;; (declare (type vector left right))
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec-dot with unequal length"))
  (reduce #'+ (map 'vector #'* left right)))

(defun vec-cross (left right)
  "Cross product for vectors of dimension 3"
  ;; (declare (type vector left right))
  (vector
   (- (* (aref left 1) (aref right 2))
      (* (aref left 2) (aref right 1)))
   (- (* (aref left 2) (aref right 0))
      (* (aref left 0) (aref right 2)))
   (- (* (aref left 0) (aref right 1))
      (* (aref left 1) (aref right 0)))))

;;; Matrix operations
;; Matrices are a 1-dimensional arary (for easy opengl interop)

(defun matrix-zeros ()
  (make-array 16))

(defun matrix-identity ()
  (vector 1.0 0.0 0.0 0.0
          0.0 1.0 0.0 0.0
          0.0 0.0 1.0 0.0
          0.0 0.0 0.0 1.0))

(defun matrix-scale (scale)
  (let ((x (elt scale 0)) (y (elt scale 1)) (z (elt scale 2)))
  ;; (declare (type float x y z))
    (vector x 0.0 0.0 0.0
            0.0 y 0.0 0.0
            0.0 0.0 z 0.0
            0.0 0.0 0.0 1)))

(defun matrix-translate (pos)
  ;; (declare (type float x y z))
  (let ((x (elt pos 0)) (y (elt pos 1)) (z (elt pos 2)))
    (vector 1 0 0 x
            0 1 0 y
            0 0 1 z
            0 0 0 1)))

(defun matrix-rotate (angles)
  ;; (declare (type float x y z))
  (let ((x (elt angles 0))
        (y (elt angles 1))
        (z (elt angles 2)))
    (vector (* (cos z) (cos y))
            (- (* (cos z) (sin y) (sin x)) (* (sin z) (cos x)))
            (+ (* (cos z) (sin y) (cos x)) (* (sin z) (sin x)))
            0.0
            (* (sin z) (cos y))
            (+ (* (sin z) (sin y) (sin x)) (* (cos z) (cos z)))
            (- (* (sin z) (sin y) (cos x)) (* (cos z) (sin x)))
            0.0
            (- (sin y))
            (* (cos y) (sin x))
            (* (cos y) (cos x))
            0.0
            0.0
            0.0
            0.0
            1.0)))


(defun matrix-look-at (position target up)
  ;; (declare (type vector position target up))
  (let* ((camera-direction (vec-normalize (vec- position target)))
          (camera-right (vec-normalize (vec-cross up camera-direction)))
          (camera-up (vec-cross camera-direction camera-right)))
    (vector (elt camera-right 0)
            (elt camera-right 1)
            (elt camera-right 2)
            (vec-dot camera-right (vec-scale -1 position))
            (elt camera-up 0)
            (elt camera-up 1)
            (elt camera-up 2)
            (vec-dot camera-up (vec-scale -1 position))
            (elt camera-direction 0)
            (elt camera-direction 1)
            (elt camera-direction 2)
            (vec-dot camera-direction (vec-scale -1 position))
            0.0
            0.0
            0.0
            1.0)))


(defun matrix-perspective (fov aspect near far)
  ;; (declare (type float fov aspect near far))
  "Returns a matrix which corresponds to matrix projection"
  (when (<= fov 0.0) (error "matrix-perspective error: fov <= 0"))
  (when (= aspect 0.0) (error "matrix-perspective error: aspect = 0"))

  (let* ((ti (/ 1 (tan (/ fov 2.0))))
         (ri (* ti aspect)))
    (vector ti 0.0 0.0 0.0
            0.0 ri 0.0 0.0
            0.0 0.0 (/ (- (+ far near)) (- far near)) (/ (* -2 far near) (- far near))
            0.0 0.0 -1.0 0.0)))

;;(declaim (inline mref))
(defmacro mref (mat i j)
  "Linear matrix reference: Row i, column j (row-major!!)"
  ;; (declare (type integer i j) (type vector mat))
  `(aref ,mat (+ (* 4 ,i) ,j)))

(defun matrix* (left right)
  "Performs standard matrix multiplication for 4x4 matrices"
  (let ((out (make-array 16)))
    (loop for i from 0 to 3 do
      (loop for j from 0 to 3 do
        (loop for k from 0 to 3 do
          (incf (mref out i j)
                (*
                 (mref left i k)
                 (mref right k j))))))
    out))


(defun matrix+ (left right)
  "Adds two matrices together: Assumes that they are 4x4"
  (loop for i from 0 to 15
        with out = (make-array 16) do
          (setf (aref out i) (+ (aref left i) (aref right i)))))


(defun matrix-act-on (matrix vector)
  "Multiplies a 4x4 matrix and a 4x1 vector"
  (let ((result (vector 0 0 0 0)))
    (loop for i from 0 to 3 do
      (setf (aref result i)
            (loop for j from 0 to 3
                  summing (* (mref matrix i j) (aref vector j)) into val
                  finally (return val)))
            finally (return result))))


