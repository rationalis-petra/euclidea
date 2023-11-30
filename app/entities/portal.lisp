(defvar *portal-shader*)

;;; as always, we start with generic function definitions...
(defgeneric connect-portals (portal-from portal-to)
  (:documentation "Connects two portals so that the entrance of one is the exit of another, and vice-versa"))

(defgeneric get-portal-camera (warp world-camera)
  (:documentation "Returns a camera object that cam be used to render the scene into the portals' framebuffer"))




(defun make-portal-shader ()
  (setf *portal-shader* (new-shader-program "resources/shaders/portal")))

;; the portal class is relatively self-explanatory: it is a plane which is linked to
;; another one via a warp instance
(defclass portal (model transform) 
  ((fbo
    :accessor fbo
    ;:type (unsigned-byte 32)
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
position relative to the to-portal"))
  (:documentation "The warp class is used to connect two portals. An unconnected portal cannot be rendered!"))


(defmethod connect-portals ((p1 portal) (p2 portal))
  ;; define a connect-portals local function which is non-commutative!
  (flet ((connect-portals (portal-from portal-to)
           (setf (portal-warp portal-from)
                 (make-instance 'warp
                  :portal-from portal-from
                  :portal-to portal-to
                  :delta
                  ;; set the "Portal Math" pdf to lean about why we do the delta matrix
                   (matrix:*
                    ;; we set scale nil because this will be done by texture scaling
                    (calc-model-matrix portal-to :scale nil)  
                    (matrix:inverse (calc-model-matrix portal-from :scale nil)))))))

    (connect-portals p1 p2)
    (connect-portals p2 p1)))



;; if you want to learn about what this method is doing, it's the focus of the pdf "Portal Math"
;; in the doc directory
(defmethod get-portal-camera ((warp warp) (world-cam camera))
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
                           (if (> 0 (vec:dot (portal-forward portal-from)
                                             (camera-rect-direction world-cam)))
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
  (call-next-method)
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
  (setf (model-matrix portal) (calc-model-matrix portal))
  (setf (fbo portal)
        (make-instance 'context-value
                       :initializer (lambda () (initialize-fbo portal))
                       :finalizer (lambda ())))
  (call-next-method))

(defun initialize-fbo (portal)
  (let* ((fbo (gl:gen-framebuffer))
         (rbo (gl:gen-renderbuffer)))
    (with-slots (shader texture) portal
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

      ;; is this needed?? or do the same settings carry from the default framebuffer?
      (gl:enable :depth-test)

      ;; verify the framebuffer
      (let ((framebuffer-status (gl:check-framebuffer-status :framebuffer)))
        (unless (gl::enum= framebuffer-status :framebuffer-complete)
          (error "Framebuffer not complete: ~A." framebuffer-status)))

      (gl:bind-framebuffer :framebuffer 0))
    fbo))

(defclass portal-canvas ()
  ((camera
   :accessor camera)))
(defmethod initialize-instance ((canvas portal-canvas) &key camera)
  (setf (slot-value canvas 'camera) camera))

(defvar *portal-recursion-depth* 0)
(defvar *portal-framebuffer* 0)

;; Override the draw method for the portal
(defmethod draw ((portal portal) canvas world)
  ;; note: Bug when we set this number to > 1
  ;; this is because this portal at both a current & deeper recursion level 
  ;; use the same framebuffer object, leading to weird visual glitches
  (when (< *portal-recursion-depth* 1)
    (let ((*portal-recursion-depth* (+ 1 *portal-recursion-depth*)))
      (with-slots (fbo texture shader position warp) portal

        ;; we start by binding the framebuffer of the portal so that future calls
        ;; to draw will instead draw the object to the active framebuffer
        (gl:bind-framebuffer :framebuffer (context-value fbo))
        (gl:clear :color-buffer :depth-buffer)

        ;; we generate a new camera that can be used
        (let* ((*portal-framebuffer* (context-value fbo))
               (new-canvas
                 (make-instance 'portal-canvas
                                :camera (get-portal-camera warp (camera canvas)))))
          ;; draw all entities - note that the recursion check occurs in the
          ;; beginning of the `draw` function.
          (mapcar (lambda (e)
                    (unless (eq portal e)
                      (draw e new-canvas world)))
                  (world-entities world)))

        ;; re-bind the default framebuffer so future calls to draw will output to
        (gl:bind-framebuffer :framebuffer *portal-framebuffer*)


        ;; generate mipmap for the texture
        (gl:bind-texture :texture-2d texture)
        (gl:generate-mipmap :texture-2d)
        (gl:bind-texture :texture-2d 0)

        ;; draw as usual
        (call-next-method)))))

