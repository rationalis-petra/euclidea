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

(defun main ()
  (unwind-protect
       (progn
         (let* ((camera (make-instance 'camera))
                (entities (list make-instance 'model :camera camera)))

           (new-window 1280 720)
           (render-init)
           (setf *initialized* t)

           (loop until (or (window-should-close-p) (key-is-pressed :escape)) do
             (render-system entities))))
         (delete-window)))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))


