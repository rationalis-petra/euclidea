
;; (defun input-system (camera) 
;;   (when (key-is-pressed :w)
;;     (incf (aref (camera-pos camera) 2) 1.0))
;;   (when (key-is-pressed :s)
;;     (incf (aref (camera-pos camera) 2) 1.0))
;;   (when (key-is-pressed :a)
;;     (incf (aref (camera-pos camera) 0) 1.0))
;;   (when (key-is-pressed :d)
;;     (incf (aref (camera-pos camera) 0) 1.0)))

(defvar *key-sensitivity* 5.0)
(defvar *mouse-sensitivity* 0.1)

(defun input-system (_ state)
  (let ((camera (world-camera state))
        (time (world-delta-time state)))
    (with-slots (position direction up) camera
      (let* ((theta (elt (camera-direction camera) 0))
             (phi   (elt (camera-direction camera) 1))
             ;; calculate a 'forward' and 'right' vectors based on the camera's direction
             (forward (vector (* (sin theta) (cos phi))
                              (cos theta)
                              (* (sin theta) (sin phi))))
             (right (vec-cross forward (camera-up camera))))

        ;; forward/backward 
        (when (key-is-pressed-p :w)
          (setf position
                (vec+ position (vec-scale (* time *key-sensitivity*) forward))))
        (when (key-is-pressed-p :s)
          (setf position
                (vec+ position (vec-scale (* -1.0 time *key-sensitivity*) forward))))

        ;; left/right (x-coord)
        (when (key-is-pressed-p :a)
          (setf position
                (vec+ position (vec-scale (* -1.0 time *key-sensitivity*) right))))
        (when (key-is-pressed-p :d)
          (setf position
                (vec+ position (vec-scale (* time *key-sensitivity*) right))))

        ;; up/down (y-coord)
        (when (key-is-pressed-p :space)
          (setf position
                (vec+ position (vec-scale (* time *key-sensitivity*) up))))
        (when (key-is-pressed-p :left-shift)
          (setf position
                (vec+ position
                      (vec-scale (* -1.0 time *key-sensitivity*) up)))))

      (let ((delta-pos (world-cursor-delta-pos state)))
        ;; index 0 = x, affects phi = index 1
        (incf (elt direction 0) (* (elt delta-pos 1) time *mouse-sensitivity*))
        ;; index 1 = y, affects theta = index 0
        (incf (elt direction 1) (* (elt delta-pos 0) time *mouse-sensitivity*))))))
