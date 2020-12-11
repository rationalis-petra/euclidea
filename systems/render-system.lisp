(defvar *shader-program*)

(defvar +pi+ 3.14159265358979)

;; (gl:define-gl-array-format positions
;;   (gl:vertex :type :float :components (x y z)))



(defclass mesh ()
  ((vertices :accessor get-vao
             :documentation "The gl array"))
             
  (:documentation "Wrapper around a gl array"))

(defun make-vao (m)
  ;; generate & bind the VBO (data storage)
  (let ((buffer (gl:gen-buffer)))
    (gl:bind-buffer :array-buffer buffer)

    ;; populate a c-style array with data
    (let ((arr (gl:alloc-gl-array :float 9))
          (verts #(-0.5 -0.5 0.0 0.5 -0.5 0.0 0.0 0.5 0.0)))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i)))
      ;; copy the data into the vbo
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind buffer
      (gl:bind-buffer :array-buffer 0)

      ;; generate a vao to reference this data
      (let ((vao (gl:gen-vertex-array)))
        (setf (get-vao m) vao)
        (gl:bind-vertex-array vao)
        (gl:bind-buffer :array-buffer buffer)
        ;; how the data is layed out
        (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
        (gl:enable-vertex-attrib-array 0)

        ;; unbind the vao
        (gl:bind-vertex-array 0)
        ;; return
        (setf (get-vao m) vao)))))
;; see https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp


(defgeneric draw (entity))

(defun render-system (entities)
  (loop for entity in entities do
    (draw entity)
    (display)
    (poll-events)))

(defmethod draw ((m mesh))
  ;;(gl:enable-client-state :vertices)
  (gl:use-program *shader-program*)
  (gl:bind-vertex-array (get-vao m))
  (gl:draw-arrays :triangles 0 3))

(defun render-init ()
  ;; make the shader: location resources/shaders/basic.(vert/frag)
  (unless *initialised*
    (mapcar #'make-vao *entities*)
    (setf *shader-program* (new-shader-program "resources/shaders/basic")))
    (setf *initialised* nil))

(defun render-clean ())
