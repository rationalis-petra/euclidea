
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

