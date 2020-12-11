;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp

(defclass entity ()())

(defvar *entities* nil)
(defvar *initialized* nil)

(load "graphics/window.lisp")
(load "graphics/render-system.lisp")

(setf *entities* (list (make-instance 'drawable)))

(defun main ()
  (new-window 1280 720)
  (unless *initialized*
    (render-init)
    (setf *initialized* t))
  (loop until (or (window-should-close-p) (key-is-pressed :escape)) do
    (render-system *entities*))
  (delete-window))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))

