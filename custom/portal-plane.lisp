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
    :type (vector (vector float 3) 2)
    :accessor portal-vertices
    :initarg :plane-vertices
    :initform (vector
               #(0.666667 0.666667 0.666666)
               #(0.666667 -0.666667 -0.666666))
    :documentation "The *transformed* vertices of the portal plane: (bl, tr)")
   (warp
    :accessor portal-warp
    :type warp
    :documentation "A datastructure describing how two portals are connected")

   (forward
    :initform #(1.0 0.0 0.0)
    :type (vector float 3)
    :reader portal-forward)
   (up
    :type (vector float 3)
    :reader portal-up)
   (right
    :type (vector float 3)
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
                  ;;(matrix:
                   (matrix:*
                    (calc-model-matrix portal-to :translation nil)
                    (calc-model-matrix portal-from :translation nil))))))

    (connect-portals p1 p2)
    (connect-portals p2 p1)))



(defgeneric get-portal-camera (warp world-camera))
(defmethod get-portal-camera((warp warp) (world-cam camera))
  (with-slots (portal-from portal-to delta) warp
    (make-instance 'rectangular-camera
                   :position
                   (vec:+
                    (matrix:apply delta
                                   (vec:- (transform-position portal-from)
                                         (camera-pos world-cam)))
                    (transform-position portal-to))
                   :direction
                   (vec:normalize (matrix:apply delta (camera-direction world-cam)))
                   :up
                   #(0.0 1.0 0.0))))

(defgeneric get-portal-view (warp world-camera))
(defmethod get-portal-view ((warp warp) (trans-cam camera))
  (with-slots (portal-to) warp 
    (destructuring-bind (top-left bottom-right)
        ;; vertices will be in /absolute/ coords. get as relative to the trans-cam
        ;; in terms of the normal, right, up
        (map 'vector
             (lambda (vertex)
               (matrix:apply (calc-model-matrix portal-to :translation nil)
                             (vec:- vertex
                                    (camera-pos trans-cam))))
             (portal-vertices portal-to))
      (let ((up (portal-up portal-to))
            (right (portal-right portal-to))
            (forward (portal-forward portal-to)))
        (matrix:detailed-perspective
         (vec:dot up top-left)
         (vec:dot up bottom-right)
         (vec:dot right top-left)
         (vec:dot right bottom-right)
         (vec:dot forward bottom-right)
         (+ (vec:dot forward bottom-right) 100.0))))))



(defmethod initialize-instance ((portal portal) &key)
  (call-next-method)
  (setf (model-shader portal) *portal-shader*)
  (make-vao portal (load-obj #p"resources/meshes/portal-plane.obj" :texture-p t))

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
    (gl:bind-framebuffer :framebuffer fbo)
    (gl:clear :color-buffer :depth-buffer)
    (gl:viewport 0 0 512 512)

    (let* ((camera (world-camera world))
           (view (world-view world)))
      (setf (world-camera world) (get-portal-camera warp camera))
      (setf (world-view world) (get-portal-view warp (world-camera world)))

      ;; change the projection matrix so 
      (mapcar (lambda (e) (unless (typep e 'portal) (draw e world)))
              (world-entities world))

      (setf (world-view world) view)
      (setf (world-camera world) camera))

    (gl:bind-framebuffer :framebuffer 0)
    (gl:viewport 0 0 1280 720)

    (gl:bind-texture :texture-2d texture)
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)

    ;; draw as usual
    (call-next-method)))

