


(defclass ec-window (window)
  ((camera
    :type camera
    :accessor camera
    :initform (make-instance 'polar-camera)
    :documentation "The default camera. It is from this perspective that a model will be rendered")
   (camera-mode
    :accessor camera-mode
    :documentation "Describes how the window should process input. 'Normal'
  implies regular gameplay, while 'editor' implies that the mouse has been freed
  and the user wishes to inspect/edit the level.")
   (prev-cursor-pos
    ;:type (vector 2)
    :accessor prev-cursor-pos
    :documentation "The change in location of the cursor since last main-loop iteration")
   (cursor-delta-pos
    ;:type (vector fixnum 2)
    :accessor cursor-delta-pos
    :initform (vector 0 0)
    :documentation "The change in location of the cursor since last main-loop iteration")))

(defmethod initialize-instance :before ((window ec-window) &key &allow-other-keys)
  (setf (prev-cursor-pos window) (cursor-pos window))
  (setf (camera window) (make-instance 'polar-camera))
  (with-slots (width height projection) (camera window)
    (setf projection (matrix:perspective (/ pi 2) (/ (width window) (height window)) 0.1 100))
    (setf width (width window))
    (setf height (height window)))
  (setf (slot-value window 'camera-mode) :normal)
  (setf (camera-pos (camera window)) #(-4.0 0.0 0.0)))

(defmethod window-render ((window ec-window) engine))

;; attach a glfw window to the engine instance. May update the (currently clunky)
;; way of handling windows: they are just a raw glfw window handle at the moment


(defmethod (setf camera-mode) (mode (window ec-window))
  (case mode
    (:normal (glfw:set-input-mode :cursor :disabled (%handle-of window)))
    (:editor (glfw:set-input-mode :cursor :normal (%handle-of window))))
  (setf (slot-value window 'camera-mode) mode))


(defmethod (setf width) (width (window ec-window))
  (setf (camera-width (camera window)) width)
  (setf (slot-value window 'width) width)
  (setf (slot-value (camera window) 'projection)
        (matrix:perspective (/ pi 2) (/ (width window) (height window)) 0.1 100)))

(defmethod (setf height) (height (window ec-window))
  (setf (camera-height (camera window)) height)
  (setf (slot-value window 'height) height)
  (setf (slot-value (camera window) 'projection)
        (matrix:perspective (/ pi 2) (/ (width window) (height window)) 0.1 100)))
