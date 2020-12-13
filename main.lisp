
(load "engine.lisp")


(defclass rotating-cube (model)
  ((rotation
    :accessor cube-rotation
    :initform (vector (/ +pi+ 4) 0.0 (/ +pi+ 4))
    :documentation "The current rotation of the cube")
   (position
    :accessor cube-position
    :initform (vector 0.0 0.0 0.0)
    :initarg :position
    :documentation "The position of the cube"))
  (:documentation "A cube which rotates over time"))
  


(defun make-rotating-cube (position)
  (let ((cube (make-instance 'rotating-cube :position position)))
    (make-vao cube (load-obj #p"resources/meshes/cube.obj"))
    cube))

(defmethod update ((cube rotating-cube) state)
  (incf (elt (cube-rotation cube) 1) (* (world-delta-time state) 1.1))
  (setf (model-matrix cube) (matrix*
                             (matrix-translate (cube-position cube))
                             (matrix-rotate (cube-rotation cube)))))
(defclass tunnel (model) ())

(defmethod make-tunnel (position)
  (let ((tunnel (make-instance 'tunnel)))
    (make-vao tunnel (load-obj #p"resources/meshes/tunnel.obj"))
    (setf (model-matrix tunnel) (matrix-translate position))
    tunnel))

(defun make-rotating-cube (position)
  (let ((cube (make-instance 'rotating-cube :position position)))
    (make-vao cube (load-obj #p"resources/meshes/cube.obj"))
    cube))


(defun main ()
  (engine
   (lambda ()
     (list
      (make-rotating-cube (vector 2.0 0.0 2.0))
      (make-rotating-cube (vector -2.0 0.0 2.0))
      (make-rotating-cube (vector -2.0 0.0 -2.0))
      (make-rotating-cube (vector 2.0 0.0 -2.0))
      (make-tunnel (vector 10 0 0))))))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "euclidea"
   :toplevel #'main
   :executable t))
