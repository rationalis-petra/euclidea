
(defclass camera ()
  ((position
    :accessor camera-pos
    :initarg :position
    :initform (vector 0.0 0.0 0.0)
    :documentation "The position of the camera")
   (up
    :accessor camera-up
    :initarg :up
    :initform (vector 0.0 1.0 0.0)
    :documentation "Gives the orientation of the camera"))
  (:documentation "A class describing a camera, used to generate view matrices"))


(defgeneric gen-view-matrix (camera)
  (:documentation "Gets the view matrix for a particular camera"))

(defclass polar-camera (camera)
  ((direction
    :accessor camera-direction
    :initarg :direction
    :initform (vector (/ pi 2) 0.0)
    :documentation "Alternate to target: a pair of spherical coords denoting the direction the camera is looking"))
  (:documentation ""))

(defmethod gen-view-matrix ((camera polar-camera))
  (with-slots (direction position up) camera
    ;; view matrix
    (let* ((theta  (elt direction 0))
           (phi    (elt direction 1))
           ;;  theta = polar angle (measured down from 0 1 0),
           ;;  phi   = azimuthal angle (measured from 1 0 0 clockwise)
           ;;  dir   = this same point, but in cartesian coords
           (dir (vector (* (sin theta) (cos phi))
                        (cos theta)
                        (* (sin theta) (sin phi)))))

       (matrix:look-at position (vec:scale -1 dir)  up))))

(defclass rectangular-camera (camera)
  ((direction
    :accessor camera-direction
    :initarg :direction
    ;; default look direction = negative z axis
    :initform (vector 0 0 -1)
    :documentation "Alternate to target: a pair of spherical coords denoting the direction the camera is looking"))
  (:documentation ""))

(defmethod gen-view-matrix ((camera rectangular-camera))
  (with-slots (direction position up) camera
    (matrix:look-at position direction up)))

(defclass absolute-camera (camera)
   ((target
     :accessor camera-target
     :initarg :target
     :initform (vector 0.0 0.0 0.0)
     :documentation "The position of the point the camera is looking at"))
  (:documentation ""))

(defmethod gen-view-matrix ((camera absolute-camera))
  (with-slots (target position up) camera
    (matrix:look-at
     position
     (vec:normalize (vec:- target position))
     up)))
