
;; The Hollow cube will be obvious as a sort of 'wire-frame' box with thick
;; sides, located at (0, 0, 0)
(defun make-hollow-cube ()
  (let ((hollow-cube (make-instance 'model)))
    (make-vao hollow-cube (load-obj #p"resources/meshes/hollow-cube.obj" :normal-p t))
    (setf (model-matrix hollow-cube) (matrix:scale (vector 2.0 2.0 2.0)))
    hollow-cube))
