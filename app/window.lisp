


(defclass ec-window (window)
  ((camera
    :type camera
    :accessor camera
    :initform (make-instance 'polar-camera)
    :documentation "The default camera. It is from this perspective that a model will be rendered")
   (prev-cursor-pos
    ;:type (vector 2)
    :accessor prev-cursor-pos
    :documentation "The change in location of the cursor since last main-loop iteration")
   (cursor-delta-pos
    ;:type (vector fixnum 2)
    :accessor cursor-delta-pos
    :initform (vector 0 0)
    :documentation "The change in location of the cursor since last main-loop iteration")))

(defmethod initialize-instance ((window ec-window) &key &allow-other-keys)
  (setf (prev-cursor-pos window) (cursor-pos window))
  (setf (camera window) (make-instance 'polar-camera))
  (with-slots (width height projection) (camera window)
    (setf projection (matrix:perspective (/ pi 2) (/ (width window) (height window)) 0.1 100))
    (setf width (width window))
    (setf height (height window)))
  (setf (camera-pos (camera window)) #(-4.0 0.0 0.0)))

(defmethod window-render ((window ec-window) engine))

;; attach a glfw window to the engine instance. May update the (currently clunky)
;; way of handling windows: they are just a raw glfw window handle at the moment


