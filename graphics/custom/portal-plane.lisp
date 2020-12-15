(defclass portal-plane (model)
  ((framebuffer
    :accessor portal-framebuffer
    :documentation "Holds the texture for the portal plane")
   (camera
    :accessor portal-camera
    :documentation "The camera perspective to render into the framebuffer"))
  (:documentation "Will act as a portal from one rectangular plane to another"))


(defun portal-frame-init (plane)
  ;; TODO: create a framebuffer & texture so the framebuffer renders to that texture
  ;; bind vao as usual
  )

(defmethod draw ((m portal-plane))
  ;; TODO: set to render to the framebuffer, rather than the actual output...


  ;; then render the texture 
  (dolist (entity (get-entities (world-state)))
    ;; the only thing we don't render is portal-planes, for to avoid infinite loop
    ;; may add some kind of max-depth, probably with a visibility test in the meantime
    (unless (typep entity 'portal-plane)
      (draw entity))))

(defun make-portal-plane (position)
  (let ((portal-plane (make-instance 'model)))
    (make-vao portal-plane (load-obj #p"resources/meshes/portal-plane.obj"))
    (setf (model-matrix portal-plane) (matrix*
                                      (matrix-translate position)
                                      (matrix-scale (vector 2.0 2.0 2.0))))
    portal-plane))
