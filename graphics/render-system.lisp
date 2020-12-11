(defvar *shader-program*)

(defvar +pi+ 3.14159265358979)

(defclass drawable ()
  ((vao :accessor get-vao
        :documentation "The gl array"))
  (:documentation "Wrapper around a gl array"))


;;(defun make-vao
;; vertices, layout, indices
;; drawable: vao

(defun make-vao (m)
  ;; generate & bind the VBO (data storage)
  (let* ((buffers (gl:gen-buffers 2))
         (vert-buf (elt buffers 0))
         (ind-buf (elt buffers 1)))

    ;; populate a c-style array with data
    (let ((arr (gl:alloc-gl-array :float 12))
          (verts #(-0.5 -0.5 0.0
                   -0.5  0.5 0.0
                    0.5 -0.5 0.0
                    0.5  0.5 0.0)))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i)))

      ;; bind the corret VBO
      (gl:bind-buffer :array-buffer vert-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :array-buffer 0))

    (let ((arr (gl:alloc-gl-array :unsigned-short 6))
          (indexes #(0 2 1 1 2 3)))
      (dotimes (i (length indexes))
        (setf (gl:glaref arr i) (aref indexes i)))

      ;; bind the index VBO
      (gl:bind-buffer :element-array-buffer ind-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :element-array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :element-array-buffer 0))


      ;; generate a vao to reference this data
      (let ((vao (gl:gen-vertex-array)))
        (setf (get-vao m) vao)
        (gl:bind-vertex-array vao)
        (gl:bind-buffer :array-buffer vert-buf)
        ;; how the data is layed out
        (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
        (gl:enable-vertex-attrib-array 0)

        ;; bind EAO
        (gl:bind-buffer :element-array-buffer ind-buf)

        ;; unbind the vao
        (gl:bind-vertex-array 0)
        ;; return
        (setf (get-vao m) vao))))

;; see https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp









(defgeneric draw (entity)
  (:documentation "for components/entities which are drawable"))

(defmethod draw ((m entity)))

(defmethod draw ((m drawable))
  ;;(gl:enable-client-state :vertices)
  (gl:use-program *shader-program*)
  (gl:bind-vertex-array (get-vao m))
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6))









(defun render-system (entities)
  (loop for entity in entities do
    (draw entity)
    (display)
    (poll-events)))

(defun render-init ()
  ;; make the shader: location resources/shaders/basic.(vert/frag)
  (mapcar #'make-vao *entities*)
  (setf *shader-program* (new-shader-program "resources/shaders/basic")))

(defun render-clean ())
