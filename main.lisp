;; load components.lisp
;; load cl-glfw
;; load cl-opengl
;; files:
;; graphics.lisp
;; render-system.lisp
(load "engine/graphics.lisp")
(load "systems/render-system.lisp")

(defvar *entities* nil)
(defvar *initialised* nil)

(setf *entities* (list (make-instance 'mesh)))

(defun main ()
  (new-window 1080 720)
  (render-init)
  (loop until (or (window-should-close-p) (key-is-pressed :escape)) do
    (render-system *entities*))
  (delete-window))

;; (defun main ()
;;   (clean
;;    (run
;;     (init
;;      (make-ecs
;;       :window (new-window 2560 1440)
;;       :components (list model-comp camera-comp rigidbody-comp attachment-comp sensor-comp actuator-comp agent-comp info-comp)
;;       :systems (list #'enemy-system #'input-system #'physics-system #'render-system)
;;       :resources texture-res mesh-res)))))

(defun make-executable ()
  (sb-ext:save-lisp-and-die
   "enigma-lispy"
   :toplevel #'main
   :executable t))

