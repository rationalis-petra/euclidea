
(load "engine.lisp")

(defvar *engine* (make-instance 'engine))

(load "custom/portal-plane.lisp")

(defclass rotating-cube (model)
  ((rotation
    :accessor cube-rotation
    :initform (vector (/ pi 4) 0.0 (/ pi 4))
    :documentation "The current rotation of the cube")
   (position
    :accessor cube-position
    :initform (vector 0.0 0.0 0.0)
    :initarg :position
    :documentation "The position of the cube"))
  (:documentation "A cube which rotates over time"))
  


(defun make-rotating-cube (position)
  (let ((cube (make-instance 'rotating-cube :position position)))
    (make-vao cube (load-obj #p"resources/meshes/cube.obj" :normal-p t))
    cube))

(defmethod update ((cube rotating-cube) state)
  (incf (elt (cube-rotation cube) 1) (* (world-delta-time state) 1.1))
  (setf (model-matrix cube) (matrix*
                             (matrix-translate (cube-position cube))
                             (matrix-rotate (cube-rotation cube)))))

(defun make-tunnel (position)
  (let ((tunnel (make-instance 'model)))
    (make-vao tunnel (load-obj #p"resources/meshes/tunnel.obj" :normal-p t))
    (setf (model-matrix tunnel) (matrix-translate position))
    tunnel))


(defun make-hollow-cube (position)
  (let ((hollow-cube (make-instance 'model)))
    (make-vao hollow-cube (load-obj #p"resources/meshes/hollow-cube.obj" :normal-p t))
    (setf (model-matrix hollow-cube) (matrix*
                                      (matrix-translate position)
                                      (matrix-scale (vector 2.0 2.0 2.0))))
    hollow-cube))


(defun main ()
  (setf *engine* (make-instance 'engine))
  (add-init-func *engine* #'make-portal-shader)
  (add-init-func
   *engine* 
   (lambda ()
     (setf (world-entities *engine*)
           (list
            (make-rotating-cube (vector -10.0 0.0 0.0))
            (make-rotating-cube (vector -14.0 0.0 0.0))
            (make-rotating-cube (vector -18.0 0.0 0.0))
            (make-rotating-cube (vector -18.0 0.0 4.0))
            (make-tunnel (vector 10 0 0))
            (make-hollow-cube (vector 0.0 0.0 0.0))
            (make-instance 'portal-plane)))))
  (run *engine*))


(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "euclidea"
   :toplevel #'main
   :executable t))
