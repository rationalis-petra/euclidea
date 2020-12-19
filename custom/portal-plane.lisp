(defvar *portal-shader*)

(defun make-portal-shader ()
  (setf *portal-shader* (new-shader-program "resources/shaders/portal")))


;; the primary purpose of a portal is to store a Warp, which is a datastructure
;; describing how two portals are connected.
;; it also golds a framebuffer and a 
(defclass warp () ())
(defclass portal (model transform) 
  ((fbo
    :type (unsigned-byte 32)
    :documentation "A Handle to an OpenGL Framebuffer Object")
   (vertices
    :type list
    :accessor portal-vertices
    :initarg :plane-vertices
    :initform (list
               #(0.0 1.0 -1.0)
               #(0.0 -1.0 1.0))
    :documentation "The *transformed* vertices of the portal plane: (bl, tr)")
   (warp
    :accessor portal-warp
    :type warp
    :documentation "A datastructure describing how two portals are connected")

   (forward
    :type (vector float 3)
    :initform #(1.0 0.0 0.0)
    :reader portal-forward)
   (up
    :type (vector float 3)
    :initform #(0.0 1.0 0.0)
    :reader portal-up)
   (right
    :type (vector float 3)
    :initform #(0.0 0.0 1.0)
    :reader portal-right))
  (:documentation "A plane which displays the scene as rendered from a different location"))

(defclass warp ()
  ((portal-from
    :type portal
    :initarg :portal-from
    :documentation "A reference to the 'container' portal: where the warp is from")
   (portal-to
    :type portal
    :initarg :portal-to
    :documentation "A refernce to the location at which a portal emerges")
   (delta
    :type (vector float 9)
    :initarg :delta
    :documentation "Matrix which can be applied to something in the from-portal to put it in the same
*relative* position to the to-portal. NO translation: is a 3x3 matrix")
   (delta-inv
    :type (vector float 9)))
  (:documentation ""))

(defgeneric connect-portals (portal-from portal-to)
  (:documentation "Connects two portals so that the entrance of one is the exit of another, and vice-versa"))
(defmethod connect-portals ((p1 portal) (p2 portal))
  (flet ((connect-portals (portal-from portal-to)
           (setf (portal-warp portal-from)
                 (make-instance 'warp
                  :portal-from portal-from
                  :portal-to portal-to
                  :delta
                   (matrix:*
                    (calc-model-matrix portal-to :scale nil)
                    (matrix:inverse (calc-model-matrix portal-from :scale nil)))))))

    (connect-portals p1 p2)
    (connect-portals p2 p1)))



(defgeneric get-portal-camera (warp world-camera))
(defmethod get-portal-camera((warp warp) (world-cam camera))
  (with-slots (portal-from portal-to delta) warp
    (let ((trans-cam
            (make-instance 'rectangular-camera
                           :position
                           (vec:vec3
                            (matrix:apply delta
                                          (vec:vec4
                                           (camera-pos world-cam)
                                           1.0)))
                           :direction
                           ;; we want to be facing the same direction /relative/ to the normal of the 
                           ;; portal
                           (if (< 0 (vec:dot (portal-forward portal-from)
                                             (vec:-
                                              (transform-position portal-from)
                                              (camera-pos world-cam))))
                               (vec:scale -1.0 (vec:normalize (slot-value portal-to 'forward)))
                               (vec:normalize (slot-value portal-to 'forward)))
                           :up
                           #(0.0 1.0 0.0)
                           :width
                           512
                           :height
                           512)))
      (setf (camera-projection trans-cam)
            (destructuring-bind (top-left bottom-right)
                ;; vertices will be in /absolute/ coords. get as relative to the trans-cam
                ;; in terms of the normal, right, up
                (mapcar
                 (lambda (vertex)
                   (vec:- vertex
                          (camera-pos trans-cam)))
                 (portal-vertices portal-to))
              (let ((up (portal-up portal-to))
                    (right (portal-right portal-to))
                    (forward (portal-forward portal-to)))
                (matrix:detailed-perspective
                 (vec:dot up top-left)          ;; top
                 (vec:dot up bottom-right)      ;; bottom
                 (vec:dot right top-left)       ;; left
                 (vec:dot right bottom-right)   ;; right
                 (vec:dot forward bottom-right)
                 (+ (vec:dot forward bottom-right) 100.0)))))
      trans-cam)))



(defmethod initialize-instance ((portal portal) &key)
  (call-next-method) ; like doing super()
  (setf (model-shader portal) *portal-shader*)

  (make-vao portal (load-obj #p"resources/meshes/portal-plane.obj" :texture-p t))

  ;; use the transformation matrix to adjust the vectors: up, forward, right
  ;; we deliberately ignore the translation transform because we only care about 
  ;; rotation, ...
  (with-slots (up forward right) portal
    (let ((transform (calc-model-matrix portal :translation nil :scale nil)))
      ;; you'll notice that for up, forward, right, we first extend them to be a vec4,
      ;; then reduce them back to a vec3
      (setf up (vec:vec3 (matrix:apply transform (vec:vec4 up 1.0))))
      (setf forward (vec:vec3 (matrix:apply transform (vec:vec4 forward 1.0))))
      (setf right (vec:vec3 (matrix:apply transform (vec:vec4 right 1.0))))))

  (with-slots (vertices) portal
    (setf vertices
          (mapcar
           (lambda (vertex)
             (vec:vec3
              (matrix:apply
               (calc-model-matrix portal :scale nil)
               (vec:vec4 vertex 1.0))))
           vertices)))


  (let ((rbo nil))
    (with-slots (model-matrix shader fbo texture) portal
      (setf model-matrix (calc-model-matrix portal))

      ;; generate fbo, rbo, texture
      (setf fbo (gl:gen-framebuffer))
      (setf rbo (gl:gen-renderbuffer))
      (setf texture (gl:gen-texture))


      ;; texture data/settings
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgb 512 512 0 :rgb :unsigned-byte (cffi:null-pointer))
      (gl:generate-mipmap :texture-2d)
      (gl:bind-texture :texture-2d 0)

      ;; renderbuffer data/settings
      (gl:bind-renderbuffer :renderbuffer rbo)
      (gl:renderbuffer-storage :renderbuffer :depth-component24 512 512)
      (gl:bind-renderbuffer :renderbuffer 0)

      ;; attach texture/rbo to the fbo
      (gl:bind-framebuffer :framebuffer fbo)
      (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
      (gl:framebuffer-renderbuffer :framebuffer :depth-attachment :renderbuffer rbo)

      ;; is this needed??
      (gl:enable :depth-test)

      ;; verify the framebuffer
      (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
        (unless (gl::enum= framebuffer-status :framebuffer-complete)
          (error "Framebuffer not complete: ~A." framebuffer-status)))

      (gl:bind-framebuffer :framebuffer 0))
    (call-next-method)))

(defmethod draw ((portal portal) (world engine))
  (with-slots (fbo texture shader position warp) portal

    ;; generate the texture to load
    (let* ((camera (world-camera world)))

      ;;(setf (transform-position (portal-cube portal)) (camera-pos (get-portal-camera warp camera)))
      ;;(draw (portal-cube portal) world)

      (gl:bind-framebuffer :framebuffer fbo)
      (gl:clear :color-buffer :depth-buffer)

      (setf (world-camera world) (get-portal-camera warp camera))

      ;;change the projection matrix so 
      (mapcar (lambda (e) (unless (typep e 'portal) (draw e world)))
              (world-entities world))

      (setf (world-camera world) camera))

    (gl:bind-framebuffer :framebuffer 0)
    (gl:viewport 0 0 1280 720)

    (gl:bind-texture :texture-2d texture)
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)

    ;; draw as usual
    (call-next-method)))

