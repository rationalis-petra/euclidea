
;; The box is a cube with one side missing: that 'missing side' is used as a place to put
;; the portal plane. We place our cubes in here to provide the illusion that they are the 'centre'
;; of the hollow cube
(defun make-box (position rotation)
  (let ((box (make-instance 'model)))
    (make-vao box (load-obj #p"resources/meshes/box.obj" :normal-p t))
    (setf (model-matrix box) (matrix:*
                                      (matrix:translate position)
                                      (matrix:rotate rotation)))
    box))
