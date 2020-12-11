;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp
(load "graphics/window.lisp")
(load "graphics/render-system.lisp")

(defvar *entities* nil)
(defvar *initialised* nil)

(setf *entities* (list (make-instance 'mesh)))

(defun main ()
  (new-window 1080 720)
  (render-init)
  (loop until (or (window-should-close-p) (key-is-pressed :escape)) do
    (render-system *entities*))
  (delete-window))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))

