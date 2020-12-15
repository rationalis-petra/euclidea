(defclass rigidbody (entity)
  ((position
    :accessor rb-position
    :initform (vector 0.0 0.0 0.0)
    :documentation "Position vector")
   (velocity
    :accessor rb-velocity
    :initform (vector 0.0 0.0 0.0)
    :documentation "velocity vector")
   (acceleration
    :accessor rb-acceleration
    :initform (vector 0.0 0.0 0.0)
    :documentation "acceleration vector")
   (mass
    :accessor rb-mass
    :initform (vector 0.0 0.0 0.0)
    :documentation "Mass in kgs"))

  (:documentation "Components of a point particle"))

(defgeneric physics-update (entity)
  (:documentation "A function which will get called on an entity every frame.
Determines the behaviour of the physics system"))

(defun physics-system (entities time)
  (mapcar #'physics-update entities))

(defmethod physics-update ((e entity))
  "Do nothing for a generic enitty")

(defmethod physics-update ((e rigidbody))
  "Updates position, velocity, acceleration based on rules"
  (with-slots (position) e
    ;; when e is a model, update the model matrix
    (when (typep e 'model)
      (setf (model-matrix e) (matrix-translate position)))))
