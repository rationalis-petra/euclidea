;;;; TRANSFORM
;; This file is quite short, and simply defines the transform class. This can be 
;; Easily attached to any entity to give it a position, rotation and scale

(defgeneric calc-model-matrix (transform &key translation rotation scale)
  (:documentation "Calculates the model matrix for the transform, optionally setting any of the
translation, rotation or scale parameters to nil so that it is not included"))

(defclass transform (entity)
  ((position
    :accessor transform-position
    :initarg :position
    :initform #(0.0 0.0 0.0)
    :documentation "The position of a transformed element")
   (rotation
    :accessor transform-rotation
    :initarg :rotation
    :initform #(0.0 0.0 0.0)
    :documentation "The rotation of a transformed element, in euler angles")
   (scale
    :accessor transform-scale
    :initarg :scale
    :initform #(1.0 1.0 1.0)
    :documentation "The scaling applied along the (local) x, y & z axes"))
  (:documentation "
A generic component for a class containing a spatial transform,
can be used in either physics or rendering, but is by default
tied to both.
"))


(defmethod calc-model-matrix ((transform transform) &key (translation t) (rotation t) (scale t))
  (with-slots ((pos position) (rot rotation) (s scale)) transform
    (apply
     #'matrix:*
     (concatenate 'list
                  (when translation (list (matrix:translate pos)))
                  (when rotation (list (matrix:rotate rot)))
                  (when scale (list (matrix:scale s)))))))
