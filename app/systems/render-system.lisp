(defgeneric update-model-matrix (model)
  (:documentation "Generates a new model matrix (via calc-model-matrix) and "))

(defvar *shader-program*)

(defclass model (entity)
  ((vao
    :accessor model-vao
    :documentation "Handle to OpenGL VAO")
   (mesh-vbo
    :accessor model-mesh-vbo
    :documentation "Handle to OpenGL VBO")
   (indices-vbo
    :accessor model-index-vbo
    :documentation "Handle to OpenGL VBO")
   (size
    :accessor model-size
    :documentation "The number of vertices to draw: |indices|")
   (texture
    :initform nil
    :documentation "A handle to the texture that the framebuffer will render to")
   (shader
    :accessor model-shader
    :initform *shader-program*
    :documentation "A Handle to the shader of the entity")
   (color
    :accessor model-color
    :initarg :color
    :initform (vector 1.0 1.0 1.0)
    :documentation "An rgb vector denoting the color of the model")
   (model-matrix
    :type matrix:matrix
    :accessor model-matrix
    :initform (matrix:identity 4)
    :documentation "A matrix describing rotation, tranlsation, scale")
   (properties
    :accessor properties
    :initform nil))
  
  (:documentation "Contains information needed by OpenGL to render an Object"))


    


;;(defun make-vao
;; vertices, layout, indices
;; drawable: vao

;; a vao: currently just coordiantes & indices
(defun make-vao (mesh data)

  ;; generate & bind the VBO (data storage)
  (let* ((buffers (gl:gen-buffers 2))
         (vert-buf (elt buffers 0))
         (ind-buf (elt buffers 1)))

    ;; populate a c-style array with data
    (let* ((verts (getf data :vertices))
           (arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i)))

      ;; bind the corret VBO
      (gl:bind-buffer :array-buffer vert-buf)
      (setf (model-mesh-vbo mesh) vert-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :array-buffer 0))

    (let* ((indices (getf data :indices))
           (arr (gl:alloc-gl-array :unsigned-short (length indices))))
      ;; unique: set size to length indices
      (setf (model-size mesh) (length indices))
      
      (dotimes (i (length indices))
        (setf (gl:glaref arr i) (aref indices i)))

      ;; bind the index VBO
      (gl:bind-buffer :element-array-buffer ind-buf)
      (setf (model-index-vbo mesh) ind-buf)

      ;; copy the data into the vbo
      (gl:buffer-data :element-array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :element-array-buffer 0))

    (when (getf data :texture-p)
      (push :texture (properties mesh)))
    (when (getf data :normal-p)
      (push :normals (properties mesh)))
    ;; generate a vao to reference this data
    (setf (model-vao mesh)
          (make-instance
           'context-value
           :initializer
           (lambda () (generate-vao
                       mesh
                       (getf data :texture-p)
                       (getf data :normal-p)))
           :finalizer
           (lambda (x))))))

(defun generate-vao (mesh has-texture has-normals)
  (let ((vao (gl:gen-vertex-array))
        (stride (* (cffi:foreign-type-size :float)
                   (+ 3
                      (if has-texture 2 0)
                      (if has-normals 3 0)))))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer (model-mesh-vbo mesh))

    ;; how the data is layed out
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil stride 0)


    ;; TODO: it seems here that texture vs normals is mutually exclusive...
    ;; this should be coded more explicitly!!
    (when has-texture
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float nil stride (* 3 (cffi:foreign-type-size :float))))

    (when has-normals
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 3 :float nil stride (* 3 (cffi:foreign-type-size :float))))

    ;; bind EAO
    (gl:bind-buffer :element-array-buffer (model-index-vbo mesh))

    ;; unbind the vao
    (gl:bind-vertex-array 0)
    ;; return
    vao))

;; see https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp







(defgeneric draw (entity canvas world)
  (:method ((entity entity) canvas world)
    (declare (ignore m canvas world)))
  (:documentation "a generic draw method"))

(defmethod draw ((m model) canvas world)
  (with-slots (camera) canvas
    (gl:viewport 0 0 (camera-width camera) (camera-height camera))
    (with-slots (shader texture color) m
      (gl:use-program shader)

      (gl:uniformfv (get-uniform shader "light_pos") (vector 0.0 5.0 0.0))
      (gl:uniformfv (get-uniform shader "object_color") color)

      ;; model matrix
      (set-uniform (get-uniform shader "model") (model-matrix m))

      ;; view matrix
      (set-uniform (get-uniform shader "view") (gen-view-matrix camera))

      ;; perspective matrix
      (set-uniform (get-uniform shader "projection") (camera-projection camera))

      ;; now actually draw the shape
      (when texture (gl:bind-texture :texture-2d texture))
      (gl:bind-vertex-array (context-value (model-vao m)))
      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (model-size m)))))



(defun render-init ()
  ;; make the shader: location resources/shaders/basic.(vert/frag)
  (setf *shader-program* (new-shader-program "resources/shaders/basic")))


(defun render-system (entities world)
  (loop for window in (windows world) do
    (canvas-predraw window)
    (mapcar (lambda (x) (draw x window world)) entities)
    (canvas-postdraw window)))


