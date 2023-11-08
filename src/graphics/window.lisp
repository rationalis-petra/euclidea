;;;; WINDOW
;; A file which mostly provides thin wrappers over glfw/opengl functions

(defclass window ()
  ((handle :initform nil :reader %handle-of)
   (app :initform nil :reader app)
   (width :initform nil :accessor width  :initarg :width)
   (height :initform nil :accessor height :initarg :height)
   (title :initform nil :initarg :title))
  (:documentation "Represents an application window"))

(defmethod initialize-instance :around ((window window) &key app title width height)
  (with-slots ((window-handle handle)
               (window-app app)
               (window-title title)
               (window-width width)
               (window-height height))
      window
    ;; TODO: where does this go??
    (glfw:initialize)

    ;(%glfw:window-hint :focused (cffi:convert-to-foreign t :boolean))
    (setf window-handle (glfw:create-window :width width :height height :title "Euclidea" :resizable t))
    (setf window-app app)
    (setf window-title title)
    (setf window-width width)
    (setf window-height height)

    (attach-window window app)
  
    ;; TODO: remove this code
    (setf *window* (glfw:get-current-context))

    ;; TODO where to put this code?
    ;;(glfw:set-input-mode :cursor :disabled)
    (gl:viewport 0 0 width height)
    (gl:clear-color 0.2 0.3 0.3 1.0)
    (gl:enable :depth-test))
  (call-next-method))


;; (defun new-window (width height)
;;   (glfw:initialize)
;;   (%glfw:window-hint :focused (cffi:convert-to-foreign t :boolean))
;;   (glfw:create-window :width width :height height :title "Euclidea" :resizable t)
;;   (setf *window* (glfw:get-current-context))
;;   (glfw:set-input-mode :cursor :disabled)

;;   (gl:viewport 0 0 width height)
;;   (gl:clear-color 0.2 0.3 0.3 1.0)
;;   (gl:enable :depth-test)
;;   )

(defun update-window (window)
  (setf (cursor-delta-pos window) (vec:- (cursor-pos window) (prev-cursor-pos window)))
  (setf (prev-cursor-pos window) (cursor-pos window)))


;; deletes the /currently active/ window
(defun delete-window (window)
  (glfw:destroy-window (%handle-of window))
  (setf (slot-value window 'handle) nil))

;; To be called every frame
(defgeneric window-render (window engine)
  (:method :around (window engine)
    (gl:viewport 0 0 (width window) (height window))

    (call-next-method)

    (glfw:swap-buffers window)
    (gl:clear :color-buffer :depth-buffer)))

(defun window-prerender (window)
  (gl:viewport 0 0 (width window) (height window)))

(defun window-postrender (window)
  (glfw:swap-buffers (%handle-of window))
  (gl:clear :color-buffer :depth-buffer))

(defgeneric window-process-input (window)
  (:documentation "Called on each window every frame")
  (:method ((window window))
    (declare (ignore window))))

(defun cursor-pos (window)
  "Return cursor position as a 2-vector"
  (make-array 2 :initial-contents
              (glfw:get-cursor-position (%handle-of window))))

(defun key-is-pressed-p (window key)
  (eq (glfw:get-key key (%handle-of window)) :press))

(defun window-should-close-p (window)
  (glfw:window-should-close-p (%handle-of window)))

