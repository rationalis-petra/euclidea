;;;; WINDOW
;; A file which mostly provides thin wrappers over glfw/opengl functions

;; basically a hack, *window* stores the most recently created window
(defvar *window* nil)

(defun new-window (width height)
  (glfw:initialize)
  (%glfw:window-hint :focused (cffi:convert-to-foreign t :boolean))
  (glfw:create-window :width width :height height :title "Euclidea" :resizable t)
  (setf *window* (glfw:get-current-context))
  (glfw:set-input-mode :cursor :disabled)

  (gl:viewport 0 0 width height)
  (gl:clear-color 0.2 0.3 0.3 1.0)
  (gl:enable :depth-test))


;; deletes the /currently active/ window
(defun delete-window (window)
  (glfw:destroy-window window)
  (setf *window* nil))

;; To be called when rendering is done: will swap the render buffer and
;; window buffer, then clear the (new) render-buffer
(defun display ()
  (glfw:swap-buffers *window*)
  (gl:clear :color-buffer :depth-buffer))




