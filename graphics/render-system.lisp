(defvar *shader-program*)

(defvar +pi+ 3.14159265358979)

(defclass camera ()
  ((position
    :accessor camera-pos
    :initform (vector 0.0 0.0 0.0)
    :documentation "The position of the camera")
   ;; (target
   ;;  :accessor camera-target
   ;;  :initform (vector 0.0 0.0 0.0)
   ;;  :documentation "The position of the point the camera is looking at")
   (direction
    :accessor camera-direction
    ;; default look direction = negative z axis
    :initform (vector (/ +pi+ 2) 0)
    :documentation "Alternate to target: a pair of spherical coords denoting the direction the camera is looking")
   (up
    :accessor camera-up
    :initform (vector 0.0 1.0 0.0)
    :documentation "Gives the orientation of the camera"))
  (:documentation "A class describing a camera, used to generate view matrices"))

(defclass model ()
  ((vao
    :accessor model-vao 
    :documentation "Handle to OpenGL VAO")
   (size
    :accessor model-size
    :documentation "The number of vertices to draw: |indices|")
   (camera
    :accessor model-camera
    :initarg :camera
    :allocation :class
    :initform (make-instance 'camera) 
    :documentation "The Camera from which the scene is rendered")
   (model-matrix
    :accessor model-model-matrix
    :initform (matrix-identity)
    :documentation "A matrix describing rotation, tranlsation, scale"))
  
  (:documentation "Wrapper around a gl array"))

    

    


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

      ;; copy the data into the vbo
      (gl:buffer-data :element-array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      ;; unbind
      (gl:bind-buffer :element-array-buffer 0))


      ;; generate a vao to reference this data
      (let ((vao (gl:gen-vertex-array)))
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
        (setf (model-vao mesh) vao))))

;; see https://github.com/3b/cl-opengl/blob/master/examples/misc/opengl-array.lisp








(defgeneric draw (entity)
  (:documentation "a generic draw method"))

(defmethod draw ((m entity)))

(defmethod draw ((m model))
  (gl:use-program *shader-program*)

  (gl:uniform-matrix-4fv (get-uniform *shader-program* "model") (model-model-matrix m))
  (let* ((camera (model-camera m))
         (theta  (elt (camera-direction camera) 0))
         (phi    (elt (camera-direction camera) 1))
         ;;  theta = polar angle (measured down from 0 1 0),
         ;;  phi   = azimuthal angle (measured from 1 0 0 clockwise)
         ;;  dir   = this same point, but in cartesian coords
         (dir (vector (* (sin theta) (cos phi))
                      (cos theta)
                      (* (sin theta) (sin phi)))))
    
    (gl:uniform-matrix-4fv
     (get-uniform *shader-program* "view")
     (matrix-look-at (camera-pos camera) (vec+ dir (camera-pos camera)) (camera-up camera))))


  ;; args to perspective: fov, aspect ratio near-plane z coord, far-plane z coord
  (gl:uniform-matrix-4fv (get-uniform *shader-program* "projection")
                         (matrix-perspective (/ +pi+ 2) *aspect* 0.1 100.0)) 

  (gl:bind-vertex-array (model-vao m))
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (model-size m)))





(defun render-init (entities)
  ;; make the shader: location resources/shaders/basic.(vert/frag)
  (mapcar #'make-vao entities (list (load-obj #p"resources/meshes/tunnel.obj")))
  (setf *shader-program* (new-shader-program "resources/shaders/basic")))



(defun render-system (entities time)
  (poll-events)
  (mapcar #'draw entities)
  (display))


