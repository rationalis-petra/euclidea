
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


(defgeneric calc-model-matrix (transform &key translation rotation scale))

(defmethod calc-model-matrix ((transform transform) &key (translation t) (rotation t) (scale t))
  (with-slots ((pos position) (rot rotation) (s scale)) transform
    (apply
     #'matrix:*
     (concatenate 'list
                  (when translation (list (matrix:translate pos)))
                  (when rotation (list (matrix:rotate rot)))
                  (when scale (list (matrix:scale s)))))))
