;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp
(ql:quickload '(cl-glfw3 cl-opengl array-operations split-sequence))

(defclass entity ()())

(defvar *entities* nil)
(defvar *initialized* nil)

(load "graphics/math.lisp")
(load "graphics/window.lisp")
(load "graphics/mesh-loader.lisp")
(load "graphics/render-system.lisp")
(load "input/input.lisp")


(defclass cube (model rigidbody) ())

(defun main ()
  (unwind-protect
       (progn
         (let* ((entities (list (make-instance 'cube)))
                (time-1 (get-internal-real-time))
                (time-2 0))

           (new-window 1280 720)
           (render-init entities)
           (setf *initialized* t)

           (setf time-2 (get-internal-real-time))
           (loop until (or (window-should-close-p) (key-is-pressed :escape)) do
             (let ((delta-time (/ (- time-2 time-1) internal-time-units-per-second)))
               (setf time-1 time-2)

               (input-system entities delta-time)
               (physics-system entities delta-time)
               (render-system entities delta-time)

               (setf time-2 (get-internal-real-time))))))
         (delete-window)))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))


