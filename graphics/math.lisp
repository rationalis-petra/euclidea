;;;; Math file: for opengl/physics math helper functions

;;; Vector operations

(defun magnitude (vec)
  (loop for x across vec
        summing (* x x) into total
        finally (return (sqrt total))))

(defun vec-scale (s vec)
  (map 'vector #'*
       (make-array (length vec) :initial-element s)
       vec))

(defun vec-normalize (vec)
  (vec-scale (/ 1 (magnitude vec)) vec))

(defun vec+ (left right)
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec+ with unequal length"))
  (map 'vector #'+ left right))

(defun vec- (left right)
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec- with unequal length"))
  (map 'vector #'- left right))

(defun vec-dot (left right)
  (unless (eq (length left) (length right))
    (error "supplied arguments to vec-dot with unequal length"))
  (map 'vector #'- left right))

(defun vec-cross (left right)
  (vector
   (- (* (aref left 1) (aref right 2))
      (* (aref left 2) (aref right 1)))
   (- (* (aref left 2) (aref right 0))
      (* (aref left 0) (aref right 2)))
   (- (* (aref left 0) (aref right 1))
      (* (aref left 1) (aref right 0)))))

;;; Matrix operations

(defun matrix-zeros ()
  (make-array '(4 4)))

(defun matrix-identity ()
  #2A((1 0 0 0)
      (0 1 0 0)
      (0 0 1 0)
      (0 0 0 1)))

(defun matrix-scale (x y z)
  (make-array
   '(4 4)
   :initial-contents
   `((,x  0  0 0)
     ( 0 ,y  0 0)
     ( 0  0 ,z 0)
     ( 0  0  0 1))))

(defun matrix-translate (x y z)
  (make-array
   '(4 4)
   :initial-contents
   `((1 0 0 ,x)
     (0 1 0 ,y)
     (0 0 1 ,z)
     (0 0 0  1))))

(defun matrix-rotate (x y z)
  (make-array
   '(4 4)
   :initial-contents
   (list
    (list (* (cos z) (cos y))
          (- (* (cos z) (sin y) (sin x)) (* (sin z) (cos x)))
          (+ (* (cos z) (sin y) (cos x)) (* (sin z) (sin x)))
          0.0)
    (list (* (sin z) (cos y))
          (+ (* (sin z) (sin y) (sin x)) (* (cos z) (cos z)))
          (- (* (sin z) (sin y) (cos x)) (* (cos z) (sin x))) 
          0.0)
    (list (- (sin y))
          (* (cos y) (sin x))
          (* (cos y) (cos x))
          0.0)
    (list 0.0 0.0 0.0 1.0))))
          

(defun matrix-look-at (position target up)
  ((let* ((camera-direction (vec-normalize (vec- position target)))
          (camera-right (vec-cross (up camera-direction)))
          (camera-up (vec-cross camera-direction camera-right)))
     (make-array
      '(4 4)
      :initial-contents
      (list
       (list (elt camera-right 0)
             (elt camera-right 1)
             (elt camera-right 2)
             (- (* (elt camera-right 0) (elt position 0))
                (* (elt camera-right 1) (elt position 1))
                (* (elt camera-right 2) (elt position 2))))
       (list (elt camera-up 0)
             (elt camera-up 1)
             (elt camera-up 2)
             (- (* (elt camera-up 0) (elt position 0))
                (* (elt camera-up 1) (elt position 1))
                (* (elt camera-up 2) (elt position 2))))
       (list (elt camera-direction 0)
             (elt camera-direction 1)
             (elt camera-direction 2)
             (- (* (elt camera-direction 0) (elt position 0))
                (* (elt camera-direction 1) (elt position 1))
                (* (elt camera-direction 2) (elt position 2))))
       (list 0.0 0.0 0.0 1.0))))))

(defun matrix-perspective (fov aspect near far)
  "Returns a matrix which corresponds to matrix projection"
  (when (<= fov 0.0) (error "matrix-perspective error: fov <= 0"))
  (when (= aspect 0.0) (error "matrix-perspective error: aspect = 0"))
  (let* ((ti (/ 1 (tan (/ fov 2.0))))
         (ri (* ti aspect)))
    (make-array
     '(4 4)
     :initial-contents
     `((,ti 0.0 0.0 0.0)
       (0.0 ,ri 0.0 0.0)
       (0.0 0.0 ,(/ (- (+ far near)) (- far near)) ,(/ (* -2 far near) (- far near)))
       (0.0 0.0 -1 0.0)))))

(defun matrix* (left right)
  "Performs standard matrix multiplication for 4x4 matrices"
  (let ((out (make-array '(4 4))))
    (loop for i from 0 to 3 do 
      (loop for j from 0 to 3 do
        (loop for k from 0 to 3 do
          (setf (aref out i j)
                (+ (aref out i j)
                   (*
                    (aref left i k)
                    (aref right k j)))))))
    out))

(defun matrix+ (left right)
  "Adds two matrices together: Assumes that they are 4x4"
  (loop for i from 0 to 15
        with out = (make-array '(4 4))
        do
           (setf (row-major-aref out i)
                 (+
                  (row-major-aref left i)
                  (row-major-aref right i)))))

(defun matrix-act-on (matrix vector)
  "Multiplies a 4x4 matrix and a 4x1 vector"
  (let ((result (vector 0 0 0 0)))
    (loop for i from 0 to 3 do
      (setf (elt result i)
            (loop for j from 0 to 3 do
                  summing (* (aref i j) (elt vector j)) into val
                  finally (return val))


