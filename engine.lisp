;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp
(ql:quickload '(cl-glfw3 cl-opengl array-operations split-sequence bordeaux-threads))

(defclass entity () ()
  (:documentation "Root of Object hierarchy for systems"))

(defgeneric update (entity state)
  (:documentation "A function that a user of the engine can choose to implement to define custom behaviours, but
which do not justify building an entirely new system"))

(defgeneric attach-window (engine window)
  (:documentation "An engine will render to an attached window: can only have one attached window at a time"))





(defmethod update ((entity entity) state) nil)

(load "math/vector.lisp")
(load "math/matrix.lisp")
(load "math/transform.lisp")

(load "graphics/window.lisp")
(load "graphics/camera.lisp")
(load "graphics/mesh-loader.lisp")
(load "graphics/render-system.lisp")

(load "physics/physics-system.lisp")
(load "input/input-system.lisp")


(defclass engine ()
  ((init-functions
    :accessor engine-init-funcs
    :initform (list (lambda () (attach-window *engine* (new-window 1280 720))) #'render-init)
    :documentation "Methods which initialize resources etc. Take 0 arguments")
   (system-functions
    :accessor engine-system-funcs
    :initform (list #'input-system #'physics-system #'render-system)
    :documentation "Methods which are called every frame. Take 2 arguments: an entity list & the engine")
   (cleanup-functions
    :accessor engine-cleanup-funcs
    :initform (list (lambda () (delete-window)))
    :documentation "Methods which cleanup resources, etc. Guaranteed to be called via unwind-protect")

   ;; Variables relating to 
   (entities
    :accessor world-entities
    :initform nil
    :documentation "The objectes which the simulation engine will operate on")
   (camera
    :accessor world-camera
    :initform (make-instance 'polar-camera)
    :documentation "The default camera. It is from this perspective that a model will be rendered")
   (view
    :accessor world-view
    :initform (matrix:identity 4)
    :documentation "The default camera. It is from this perspective that a model will be rendered")

   ;; variables relating to input/output (incl. time)
   (delta-time
    :accessor world-delta-time
    :initform 0.0
    :documentation "Time since last main-loop iteration, in seconds")
   (cursor-pos
    :accessor world-cursor-pos
    :initform (vector 0 0)
    :documentation "The coordinates of the cursor")
   (cursor-delta-pos
    :accessor world-cursor-delta-pos
    :initform (vector 0 0)
    :documentation "The change in location of the cursor since last main-loop iteration"))
   (:documentation "Encapsulates the necessary state & methods to run a simulation"))


(defmethod attach-window ((engine engine) window)
  (setf (world-view engine)
        (matrix:perspective (/ pi 2) *aspect* 0.1 100.0))

  (glfw:def-window-size-callback update-viewport (window w h)
    (declare (ignore window))
    (setf *aspect* (/ w h))
    (gl:viewport 0 0 w h)
    (lambda ()
      (setf (world-view engine)
            (matrix:perspective (/ pi 2) *aspect* 0.1 100.0)))))


(defun run (engine)
  (with-slots (init-functions system-functions cleanup-functions
               entities delta-time cursor-pos cursor-delta-pos)
      engine
    (unwind-protect
         (progn
           (mapcar #'funcall init-functions)

           (let* (;; used to keep track of deltatime: first deltatime is 0
                  (time-1 (get-internal-real-time))
                  (time-2 time-1)
                  ;; used to keep track of cursor position
                  (cursor-1 (get-cursor-pos))
                  (cursor-2 cursor-1))

             (loop until (or (window-should-close-p) (key-is-pressed-p :escape)) do
               ;;; update the world state
               ;; deltatime
               (setf delta-time
                     (/ (- time-2 time-1) internal-time-units-per-second))
               (setf time-1 time-2)
               (setf time-2 (get-internal-real-time))
               ;;(setf time time-2)

               ;; cursorpos
               (setf cursor-delta-pos (vec:- cursor-2 cursor-1))
               (setf cursor-1 cursor-2)
               (setf cursor-2 (get-cursor-pos))
               (setf cursor-pos cursor-2)

               ;; the names of these systems should be self-explanatory
               ;; the update function is for overriding
               (mapcar (lambda (entity) (update entity engine)) entities)
               (mapcar (lambda (s) (funcall s entities engine)) system-functions))))

      (mapcar #'funcall cleanup-functions))))


(defun add-init-func (engine func)
  (with-slots ((in-fns init-functions)) engine
    (setf in-fns (append in-fns (list func)))))




