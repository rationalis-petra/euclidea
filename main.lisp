;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp
(ql:quickload '(cl-glfw3 cl-opengl array-operations split-sequence))

(defclass entity ()())

(load "graphics/math.lisp")
(load "graphics/window.lisp")
(load "graphics/mesh-loader.lisp")
(load "graphics/render-system.lisp")
(load "physics/physics-system.lisp")
(load "input/input.lisp")

(defclass world-state ()
  ((delta-time
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
  (:documentation "A class which stores global external state for the program"))



(defclass cube (model rigidbody) ())

(defun main ()
  ;; use unwind-protect to ensure (delete-window) always gets called
  (unwind-protect
       (progn
         ;; call new-window first to ensure there exists an active OpenGL context
         (new-window 1280 720)
         (let* ((entities (list (make-instance 'cube)))
                (world-state (make-instance 'world-state))
                ;; used to keep track of deltatime: first deltatime is 0
                (time-1 (get-internal-real-time))
                (time-2 time-1)
                (cursor-1 (get-cursor-pos))
                (cursor-2 cursor-1))

           ;; load shader-programs, VAOs
           (render-init entities)

           (loop until (or (window-should-close-p) (key-is-pressed-p :escape)) do
             ;;; update the world state
             ;; deltatime
             (setf (world-delta-time world-state)
                   (/ (- time-2 time-1) internal-time-units-per-second))
             (setf time-1 time-2)
             (setf time-2 (get-internal-real-time))

             ;; cursorpos
             (setf (world-cursor-delta-pos world-state)
                   (vec- cursor-2 cursor-1))
             (setf cursor-1 cursor-2)
             (setf cursor-2 (get-cursor-pos))

             ;; the names of these systems should be self-explanatory
             (input-system entities world-state)
             (physics-system entities world-state)
             (render-system entities world-state)))
         (delete-window))))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))


