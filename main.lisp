(load "engine/engine.lisp")
 
(defvar *engine* (make-instance 'engine))

(load "custom/portal-plane.lisp")

(defclass rotating-cube (model)
  ((rotation
    :accessor cube-rotation
    :initform (vector (/ pi 4) 0.0 (/ pi 4))
    :documentation "The current rotation of the cube")
   (direction
    :accessor cube-rot-dir
    :initarg :cube-rot-dir
    :initform 1)
   (position
    :accessor cube-position
    :initform (vector 0.6 0.0 0.0)
    :initarg :position
    :documentation "The position of the cube"))
  (:documentation "A cube which rotates over time"))
  

(defmethod initialize-instance ((cube rotating-cube) &key)
  (make-vao cube (load-obj #p"resources/meshes/cube.obj" :normal-p t))
  (call-next-method))

(defmethod update ((cube rotating-cube) state)
  (incf (elt (cube-rotation cube) (cube-rot-dir cube)) (* (world-delta-time state) 1.1))
  (setf (model-matrix cube) (matrix:*
                             (matrix:translate (cube-position cube))
                             (matrix:rotate (cube-rotation cube)))))

(defun make-tunnel (position)
  (let ((tunnel (make-instance 'model)))
    (make-vao tunnel (load-obj #p"resources/meshes/tunnel.obj" :normal-p t))
    (setf (model-matrix tunnel) (matrix:translate position))
    tunnel))


(defun make-hollow-cube (position)
  (let ((hollow-cube (make-instance 'model)))
    (make-vao hollow-cube (load-obj #p"resources/meshes/hollow-cube.obj" :normal-p t))
    (setf (model-matrix hollow-cube) (matrix:*
                                      (matrix:translate position)
                                      (matrix:scale (vector 2.0 2.0 2.0))))
    hollow-cube))


(defun main ()
  (setf *engine* (make-instance 'engine))
  (setf (camera-pos (world-camera *engine*)) #(-4.0 0.0 0.0))

  ;; TODO: add ability to add window /before init funcs??
  ;;(add-init-func *engine*)
  (add-init-func *engine* #'make-portal-shader)
  (add-init-func
   *engine* 
   (lambda ()
     (let ((portal-1 (make-instance 'portal
                                    :position #(2.0 0.0 0.0)))
           (portal-2 (make-instance 'portal
                                    :rotation (vector 0.0 (/ pi 2) 0.0)
                                    :position #(0.0 0.0 2.0))))
       (connect-portals portal-1 portal-2)
       (setf (world-entities *engine*)
             (list
              (make-instance 'rotating-cube :position #(-50.0 0.0 0.0)
                                            :color #(0.8 0.0 0.0))
              (make-instance 'rotating-cube :position #(50.0 0.0 0.0)
                                            :color #(0.0 0.8 0.0))
              (make-instance 'rotating-cube :position #(0.0 0.0 50.0)
                                            :color #(0.0 0.0 0.8))
              (make-instance 'rotating-cube :position #(0.0 0.0 -50.0)
                                            :color #(0.8 0.0 0.8))
              portal-1
              portal-2
              ;; (make-tunnel (vector 10 0 0))
              (make-hollow-cube (vector 0.0 0.0 0.0)))))))
   (run *engine*))


(defun make-executable ()
  ;; make an executable depending on implementation/ox
  (let ((name (if (eql (uiop:detect-os) :os-windows)
                  "euclidea-windows.exe"
                  "euclidea-unix"))
        (implementation (uiop:implementation-type)))
    (cond
      ((eql implementation :sbcl)
       (sb-ext:save-lisp-and-die
        name
        :toplevel #'main
        :executable t))
      ((eql implementation :clozure)
       ()))))
