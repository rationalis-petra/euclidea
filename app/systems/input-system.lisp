;;;; INPUT SYSTEM
;; this is a simple file containing the input system function
;; it does nothing /except/ update the user's camera location
;; based on keyboard/mouse input

(defvar *key-sensitivity* 5.0)

(defvar *mouse-sensitivity* 0.1)

(defun input-system (entities world)
  (declare (ignore entities))
  (glfw:poll-events)
  (loop for window in (windows world) do
    (update-window window)
    (input-window window world)))

(defun do-comma-callback ()
  ())

(defun input-window (window world)
  (let ((camera (camera window))
        (time (delta-time world)))

    (when (or (window-should-close-p window)
              (key-is-pressed-p window :escape))
      (setf (should-exit world) t))

    (when (key-is-pressed-p window :comma)
      (ecase (camera-mode window)
        (:normal (setf (camera-mode window) :editor))
        (:editor (setf (camera-mode window) :normal))))
    

    (when (key-is-pressed-p window :n)
      (push (make-instance
             'ec-window
             :title "Child"
             :width 1280
             :height 720
             :app world
             :shared window)
            (windows world)))

    (when (eq (camera-mode window) :normal)
      (with-slots (position polar-direction up) camera
        (let* ((theta (elt (camera-polar-direction camera) 0))
               (phi   (elt (camera-polar-direction camera) 1))
               ;; calculate a 'forward' and 'right' vectors based on the camera's direction
               (forward (vector (* (sin theta) (cos phi))
                                (cos theta)
                                (* (sin theta) (sin phi))))
               (right (vec:cross forward (camera-up camera))))

          ;; forward/backward 
          (when (key-is-pressed-p window :w)
            (setf position
                  (vec:+ position (vec:scale (* time *key-sensitivity*) forward))))
          (when (key-is-pressed-p window :s)
            (setf position
                  (vec:+ position (vec:scale (* -1.0 time *key-sensitivity*) forward))))

          ;; left/right (x-coord)
          (when (key-is-pressed-p window :a)
            (setf position
                  (vec:+ position (vec:scale (* -1.0 time *key-sensitivity*) right))))
          (when (key-is-pressed-p window :d)
            (setf position
                  (vec:+ position (vec:scale (* time *key-sensitivity*) right))))

          ;; up/down (y-coord)
          (when (key-is-pressed-p window :space)
            (setf position
                  (vec:+ position (vec:scale (* time *key-sensitivity*) up))))
          (when (key-is-pressed-p window :left-shift)
            (setf position
                  (vec:+ position
                         (vec:scale (* -1.0 time *key-sensitivity*) up)))))

        (let ((delta-pos (cursor-delta-pos window)))
          ;; index 0 = x, affects phi = index 1
          (incf (elt polar-direction 0) (* (elt delta-pos 1) time *mouse-sensitivity*))
          ;; index 1 = y, affects theta = index 0
          (incf (elt polar-direction 1) (* (elt delta-pos 0) time *mouse-sensitivity*)))))))
