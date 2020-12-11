(defvar *cursor-pos* nil)

(defun input-system ()
  (when (or (window-should-close) (key-is-pressed 'ESC))
    (stop))

  (let* ((player (first-match #'is-player))
         (player-rigidbody (get-component 'rigidbody player))
         (player-velocity (rigidbody-velocity player-rigidbody))
         (new-cursor-pos (get-cursor-pos))
         (player-camera (get-component 'camera player))
         (dir #(0 0 0)))
    (letf ((-sin (x) (- (sin x)))
           (-cos (x) (- (cos x))))
          (macrolet ((key-action (key x z)
                       `(when (key-is-pressed ,key)
                          (stf dir (vec+ dir
                                         (vector (,x (camera-phi player-camera))
                                                 0.0
                                                 (,z (camera-phi player-camera))))))))
            (key-action :w  cos  sin)
            (key-action :a  sin -cos)
            (key-action :s -cos -sin)
            (key-action :d -sin  cos)))

    (when (and (key-is-pressed :space) (< elt 1.0e-2))
      (setf player-veclocity (vec+ player-velocity #(0 1 0))))

    (setf player-velocity
          (vec+ player-velocity (normalize dir)))

    (setf (camera-phi player-camera)
          (vec+ (vec* 0.003 (- (elt new-cursor-pos 0) (elt *cursor-pos* 0)))))
    (setf (camera-theta player-camera)
          (vec+ (vec* 0.001 (- (elt new-cursor-pos 1) (elt *cursor-pos* 1)))))
    (setf *cursor-pos* new-cursor-pos)))
          

(defun input-init ()
  (setf *cursor-pos* (get-cursor-pos)))

(defun input-clean ())
