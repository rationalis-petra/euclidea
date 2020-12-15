(defvar *portal-shader*)

(defun make-portal-shader ()
  (setf *portal-shader* (new-shader-program "resources/shaders/portal")))


(defclass portal-plane (model)
  ((fbo
    :initform nil
    :documentation "A Handle to an OpenGL Framebuffer Object")
   (rbo
    :initform nil
    :documentation "A renderbuffer object to store dept data")
   (shader
    :initform *portal-shader*)
   (position
    :initarg :position
    :initform #(0.0 0.0 0.0)
    :documentation "The position of the portal plane")
   (portal-out
    :initform #(0.0 0.0 0.0)
    :initarg :out-position
    :documentation "The location at which the portal plane emerges"))
  (:documentation "A plane which displays the scene as rendered from a different location"))

(defmethod initialize-instance ((portal portal-plane) &key rotation)
  (make-vao portal (load-obj #p"resources/meshes/portal-plane.obj" :texture-p t))

  (with-slots (portal-camera model-matrix shader fbo rbo texture) portal
    (setf model-matrix (matrix*
                        (matrix-identity)
                        ;;(matrix-translate position)
                        (matrix-rotate rotation)
                        (matrix-scale (vector 2.0 2.0 2.0))))

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
  (call-next-method))

(defmethod draw ((portal portal-plane) (world engine))
  (with-slots (fbo texture shader position portal-out) portal
    ;; generate the texture to load
    (gl:bind-framebuffer :framebuffer fbo)
    (gl:clear :color-buffer :depth-buffer)
    (gl:viewport 0 0 512 512)
    (setf *aspect* 1)
    (let ((camera (world-camera world)))
      (setf (world-camera world) (make-instance 'rectangular-camera
                                         :position portal-out
                                         :direction (vec-normalize (vec- (camera-pos camera) position))
                                         :up (camera-up camera)))
      (mapcar (lambda (e) (unless (typep e 'portal-plane) (draw e world)))
              (world-entities world))
      (setf (world-camera world) camera))

    (gl:bind-framebuffer :framebuffer 0)
    (gl:viewport 0 0 1280 720)
    (setf *aspect* 16/9)

    (gl:bind-texture :texture-2d texture)
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)

    ;; draw as usual
    (call-next-method)))
