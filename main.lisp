(load "engine/engine.lisp")
 
(defvar *engine* (make-instance 'engine))

(load "custom/portal-plane.lisp")

(defclass rotating-cube (model transform)
  ()
  (:documentation "A cube which rotates over time"))
  

(defmethod initialize-instance ((cube rotating-cube) &key)
  (setf (transform-scale cube) #(0.5 0.5 0.5))
  (make-vao cube (load-obj #p"resources/meshes/cube.obj" :normal-p t))
  (call-next-method))

(defmethod update ((cube rotating-cube) state)
  (incf (elt (transform-rotation cube) 1) (* (world-delta-time state) 0.1))
  (setf (model-matrix cube) (calc-model-matrix cube)))

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

(defun make-box (position rotation)
  (let ((box (make-instance 'model)))
    (make-vao box (load-obj #p"resources/meshes/box.obj" :normal-p t))
    (setf (model-matrix box) (matrix:*
                                      (matrix:translate position)
                                      (matrix:rotate rotation)))
    box))

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
                                    :position #(49.0 0.0 0.0)
                                    :rotation (vector 0.0 0.0 0.0)))
           (portal-2 (make-instance 'portal
                                    :rotation (vector 0.0 (/ pi 2) 0.0)
                                    :scale #(1.3333334 1.3333334 1.3333334)
                                    :position #(0.0 0.0 2.0))))
       (setf (model-color (portal-cube portal-1)) #(1.0 0.0 0.0))
       (setf (model-color (portal-cube portal-2)) #(0.0 1.0 0.0))
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
              (make-box #(50.0 0.0 0.0) (vector 0.0 (/ pi -2) 0.0))
              (make-box #(-50.0 0.0 0.0) (vector 0.0 (/ pi 2) 0.0))
              (make-box #(0.0 0.0 50.0) (vector 0.0 pi 0.0))
              (make-box #(0.0 0.0 -50.0) #(0.0 0.0 0.0))
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
