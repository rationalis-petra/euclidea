;;;; ENGINE.LISP
;; This file details an extremely simpe render engine (for the OpenGl code, see
;; engine/graphics/render-system.lisp) The engine has a set of functions that
;; it will call
;; + on initialization
;; + on update
;; + on cleanup/finish (using unwind-protect)
;; in addition, the engine contains several state variables:
;; + a list of entites
;; + a camera (see engine/graphics/camera.lisp)
;; + update & input values: delta-time, cursor position, ...



(defclass entity () ()
  (:documentation "Root of Object hierarchy for systems"))


(defclass engine ()
  ;; as mentioned at the beginning of the file, the engine contains three lists of functions
  ((init-functions
    :type list
    :accessor engine-init-funcs
    :initform (list #'initialize-all-contexts)
    :documentation "Methods which initialize resources etc. Take 0 arguments")
   (system-functions
    :type list
    :accessor engine-system-funcs
    :initform nil 
    :documentation "Methods which are called every frame. Take 2 arguments: an entity list & the engine")
   (cleanup-functions
    :accessor engine-cleanup-funcs
    :initform nil
    :documentation "Methods which cleanup resources, etc. Guaranteed to be called via unwind-protect")

   (windows
    :accessor windows
    :initform nil
    :documentation "The list of windows currently associated with the application")

   ;; The engine also contains several variables relating to world-state. these are specified here
   (entities
    :type list
    :accessor world-entities
    :initform nil
    :documentation "The objectes which the simulation engine will operate on")
   (delta-time
    :type float
    :accessor delta-time
    :initform 0.0
    :documentation "Time since last main-loop iteration, in seconds")
   (should-exit
    :type boolean
    :accessor should-exit
    :initform nil
    :documentation "Used to signal if the engine should exit (cleanly) at the
   end of the current loop"))
   (:documentation "Encapsulates the necessary state & methods to run a simulation"))

(defgeneric update (entity state)
  (:method ((entity entity) state)
    (declare (ignore entity state)))
  (:documentation "A function that a user of the engine can choose to implement to define custom behaviours, but
which do not justify building an entirely new system"))

(defgeneric attach-window (window engine)
  (:method :around (window (engine engine))
    (push window (windows engine)))
  (:method (window (engine engine))
    (declare (ignore window engine)))
  (:documentation "An engine will render to an attached window: can only have one attached window at a time"))

;; The run method is a function because we don't want it being overriden
(defun run (engine)
  (with-slots (init-functions system-functions cleanup-functions
               entities delta-time cursor-pos cursor-delta-pos)
      engine
    (unwind-protect
         (progn
           (mapcar #'funcall init-functions)

           (let* (;; used to keep track of deltatime: first deltatime is 0
                  (time-1 (get-internal-real-time))
                  (time-2 time-1))

             (loop until (should-exit engine) do
               ;;; update the world state
               ;; deltatime
               (setf delta-time
                     (/ (- time-2 time-1) internal-time-units-per-second))
               (setf time-1 time-2)
               (setf time-2 (get-internal-real-time))
               ;;(setf time time-2)

               ;; cursorpos
               ;; (setf cursor-delta-pos (vec:- cursor-2 cursor-1))
               ;; (setf cursor-1 cursor-2)
               ;; (setf cursor-2 (get-cursor-pos))
               ;; (setf cursor-pos cursor-2)

               ;; the names of these systems should be self-explanatory
               ;; the update function is for overriding
               (mapcar (lambda (entity) (update entity engine)) entities)
               (mapcar (lambda (s) (funcall s entities engine)) system-functions))))

      (mapcar #'funcall cleanup-functions))))

;; a simple utility function to add an init method to the engine. This lets us
;; more easily create entities of various types on startup
(defun add-init-func (engine func)
  (with-slots ((in-fns init-functions)) engine
    (setf in-fns (append in-fns (list func)))))

(defun add-system-func (engine func)
  (with-slots ((sys-fns system-functions)) engine
    (setf sys-fns (append sys-fns (list func)))))

(defun add-cleanup-func (engine func)
  (with-slots ((clean-fns cleanup-functions)) engine
    (setf clean-fns (append clean-fns (list func)))))






