
(defgeneric gen-view-matrix (camera)
  (:documentation "Gets the view matrix for a particular camera"))

(defgeneric camera-rect-direction (camera)
  (:documentation "Gets the direction the camera is facing, expressed as a unit-vector"))

(defgeneric camera-polar-direction (camera)
  (:documentation "Returns a 2-vector containing the polar angle and the azimuthal angle"))

;; the 'root' camera class: should not be initialized...
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

;; (defmethod initialize-instance ((camera camera) &key)
;;   ;;(error "Should not initialize the camera class: use either polar-camera, rectangular-camera or absolute-camera"))

;;; POLAR CAMERA: a camera with it's direction vector in polar coordinates
(defclass polar-camera (camera)
  ((polar-direction
    :accessor camera-polar-direction
    :type (vector float 2)
    :initarg :direction
    :initform (vector (/ pi 2) 0.0)
    :documentation "Alternate to target: a pair of spherical coords denoting the direction the camera is looking"))
  (:documentation ""))

;; (defmethod initialize-instance ((camera polar-camera) &key)
;;   (call-next-method)
;;   ;; do nothing, *but*, this prevents camera's error from being thrown above
;;   )

(defmethod gen-view-matrix ((camera polar-camera))
  (with-slots (polar-direction position up) camera
    ;; view matrix
    (let* ((theta  (elt polar-direction 0))
           (phi    (elt polar-direction 1))
           ;;  theta = polar angle (measured down from 0 1 0),
           ;;  phi   = azimuthal angle (measured from 1 0 0 clockwise)
           ;;  dir   = this same point, but in cartesian coords
           (dir (vector (* (sin theta) (cos phi))
                        (cos theta)
                        (* (sin theta) (sin phi)))))

       (matrix:look-at position (vec:scale -1 dir)  up))))

(defmethod camera-rect-direction ((camera polar-camera))
  (with-slots (polar-direction position up) camera
    ;; view matrix
    (let* ((theta  (elt polar-direction 0))
           (phi    (elt polar-direction 1))
           ;;  theta = polar angle (measured down from 0 1 0),
           ;;  phi   = azimuthal angle (measured from 1 0 0 clockwise)
           ;;  dir   = this same point, but in cartesian coords
           (dir (vector (* (sin theta) (cos phi))
                        (cos theta)
                        (* (sin theta) (sin phi)))))
      (vec:scale -1 dir))))






;;; RECTANGULAR CAMERA: a camera with it's direction stored as a 
(defclass rectangular-camera (camera)
  ((direction
    :accessor camera-direction
    :initarg :direction
    ;; default look direction = negative z axis
    :initform (vector 0 0 -1)
    :documentation "Alternate to target: a pair of spherical coords denoting the direction the camera is looking"))
  (:documentation ""))

;; (defmethod initialize-instance ((camera rectangular-camera) &key)
;;   ;; do nothing, *but*, this prevents camera's error from being thrown above
;;   )

(defmethod gen-view-matrix ((camera rectangular-camera))
  (with-slots (direction position up) camera
    (matrix:look-at position direction up)))

(defmethod camera-rect-direction ((camera rectangular-camera))
  (camera-direction camera))












;;; ABSOLUTE CAMERA: a camera storing not a direction, but a single point as a target
(defclass absolute-camera (camera)
   ((target
     :accessor camera-target
     :initarg :target
     :initform (vector 0.0 0.0 0.0)
     :documentation "The position of the point the camera is looking at"))
  (:documentation ""))

;; (defmethod initialize-instance ((camera absolute-camera) &key)
;;   ;; do nothing, *but*, this prevents camera's error from being thrown above
;;   )

(defmethod gen-view-matrix ((camera absolute-camera))
  (with-slots (target position up) camera
    (matrix:look-at
     position
     (vec:normalize (vec:- target position))
     up)))
