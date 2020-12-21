;;;; MAIN
;; This file mostly contains setup functions and simple object/entity
;; definitions
(defvar *engine*)

(load "engine/engine.lisp")
(load "custom/portal.lisp")

(setf *engine* (make-instance 'engine))

;;; CONSTRUCTORS
;; simple utility functions/classes that are used to make/manipulate simple meshes
;; the rotating cube will just rotate about the y-axis
(defclass rotating-cube (model transform) ()
  (:documentation "A cube which rotates over time"))
  
;; load mesh, scale down so it fits in the box
(defmethod initialize-instance ((cube rotating-cube) &key)
  (setf (transform-scale cube) #(0.5 0.5 0.5))
  (make-vao cube (load-obj #p"resources/meshes/cube.obj" :normal-p t))
  (call-next-method))

;; rotate on update by applying a rotation matrix to the model matrix
(defmethod update ((cube rotating-cube) state)
  (incf (elt (transform-rotation cube) 1) (* (world-delta-time state) 0.1))
  (setf (model-matrix cube) (calc-model-matrix cube)))


;; The Hollow cube will be obvious as a sort of 'wire-frame' box with thick
;; sides, located at (0, 0, 0)
(defun make-hollow-cube ()
  (let ((hollow-cube (make-instance 'model)))
    (make-vao hollow-cube (load-obj #p"resources/meshes/hollow-cube.obj" :normal-p t))
    (setf (model-matrix hollow-cube) (matrix:scale (vector 2.0 2.0 2.0)))
    hollow-cube))

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

;; Use the "engine": on initialization, create many objects/portals and link them together
(defun main ()
  (setf *engine* (make-instance 'engine))
  (setf (camera-pos (world-camera *engine*)) #(-4.0 0.0 0.0))

  ;; TODO: add ability to add window /before init funcs??
  ;;(add-init-func *engine*)
  (add-init-func *engine* #'make-portal-shader)
  (add-init-func
   *engine* 
   (lambda ()
     ;; create a bunch of portals & connect them
     (let ((portal-1 (make-instance 'portal
                                    :position #(49.0 0.0 0.0)
                                    :rotation (vector 0.0 0.0 0.0)))
           (portal-2 (make-instance 'portal
                                    :position #(-1.666 0.0 0.0)
                                    :rotation (vector 0.0 0.0 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-3 (make-instance 'portal
                                    :position #(-49.0 0.0 0.0)
                                    :rotation (vector 0.0 pi 0.0)))
           (portal-4 (make-instance 'portal
                                    :position #(1.666 0.0 0.0)
                                    :rotation (vector 0.0 pi 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-5 (make-instance 'portal
                                    :position #(0.0 0.0 49.0)
                                    :rotation (vector 0.0 (/ pi -2) 0.0)))
           (portal-6 (make-instance 'portal
                                    :position #(0.0 0.0 -1.666)
                                    :rotation (vector 0.0 (/ pi -2) 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-7 (make-instance 'portal
                                    :position #(0.0 0.0 -49.0)
                                    :rotation (vector 0.0 (/ pi 2) 0.0)))
           (portal-8 (make-instance 'portal
                                    :position #(0.0 0.0 1.666)
                                    :rotation (vector 0.0 (/ pi 2) 0.0)
                                    :scale #(1.33334 1.33334 1.33334))))
       (connect-portals portal-1 portal-2)
       (connect-portals portal-3 portal-4)
       (connect-portals portal-5 portal-6)
       (connect-portals portal-7 portal-8)


       ;; the list of entities in this scene 
       (setf (world-entities *engine*)
             (list
              (make-instance 'rotating-cube :position #(-50.0 0.0 0.0)
                                            :color #(0.8 0.0 0.0)) ; red
              (make-instance 'rotating-cube :position #(50.0 0.0 0.0)
                                            :color #(0.0 0.8 0.0)) ; green
              (make-instance 'rotating-cube :position #(0.0 0.0 50.0)
                                            :color #(0.0 0.0 0.8)) ; blue
              (make-instance 'rotating-cube :position #(0.0 0.0 -50.0)
                                            :color #(0.8 0.0 0.8)) ; purple
              portal-1
              portal-2
              portal-3
              portal-4
              portal-5
              portal-6
              portal-7
              portal-8
              (make-box #(50.0 0.0 0.0) (vector 0.0 (/ pi -2) 0.0))
              (make-box #(-50.0 0.0 0.0) (vector 0.0 (/ pi 2) 0.0))
              (make-box #(0.0 0.0 50.0) (vector 0.0 pi 0.0))
              (make-box #(0.0 0.0 -50.0) #(0.0 0.0 0.0))
              (make-hollow-cube))))))
   (run *engine*))


;; make an executable depending on implementation/ox
(defun make-executable ()
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
      ;; TODO: other implementations...
      ;; clozure, abcl, ...
      ((eql implementation :clozure)
       ()))))
