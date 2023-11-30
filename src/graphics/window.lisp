;;;; WINDOW
;; A file which mostly provides thin wrappers over glfw/opengl functions
(defpackage euclid.window
  (:use :cl)
  (:export :window :context-value))

(defclass window ()
  ((handle :initform nil :reader %handle-of)
   (app :initform nil :reader app)
   (width :initform nil :accessor width  :initarg :width)
   (height :initform nil :accessor height :initarg :height)
   (title :initform nil :initarg :title))
  (:documentation "Represents an application window"))

(defclass context-value ()
  ((initializer
    :accessor initializer)
   (finalizer
    :accessor finalizer)
   (context-values
    :accessor context-values)))

(defvar *context-map* nil)  
(defvar *context-values* (make-array 0 :fill-pointer 0))
(defvar *context-indices* 0) 
(defvar *current-context-index*)

(defun initialize-all-contexts ()
  (setf *context-map* nil)  
  (setf *context-values* (make-array 0 :fill-pointer 0))
  (setf *context-indices* 0) 
  (setf *current-context-index* 0))

(defmethod initialize-instance :around
    ((val context-value) &key initializer finalizer)

  (setf (context-values val) (make-array *context-indices* :fill-pointer *context-indices*))
  (setf (initializer val) initializer)
  (setf (finalizer val) finalizer)
  (register-context-value val)

  (loop for (key . idx) in *context-map* do
        (setf (aref (context-values val) idx) (funcall initializer))))


(defun context-value (value)
  (aref (context-values value) *current-context-index*))

(defun register-context-value (value)
  (vector-push-extend value *context-values*))

(defun add-context-value (value)
  (vector-push-extend (funcall (initializer value)) (context-values value)))

(defmethod initialize-instance :around
    ((window window) &key app title width height shared)
  (with-slots ((window-handle handle)
               (window-app app)
               (window-title title)
               (window-width width)
               (window-height height))
      window
    ;; TODO: where does this go??
    (glfw:initialize)

    ;(%glfw:window-hint :focused (cffi:convert-to-foreign t :boolean))
    (setf window-handle
          (glfw:create-window
           :width width :height height :title "Euclidea"
           :resizable t :shared (if shared (%handle-of shared) (cffi:null-pointer))))

    (setf *current-context-index* *context-indices*) 
    (incf *context-indices*) 
    (push (cons (glfw:get-current-context) *current-context-index*) *context-map*)
    (loop for ctx-val across *context-values* do
          (add-context-value ctx-val))

    (setf window-app app)
    (setf window-title title)
    (setf window-width width)
    (setf window-height height)

    (attach-window window app)

    (glfw:def-window-size-callback ws-callback (win w h)
      (declare (ignore win))
      (setf (width window) w)
      (setf (height window) h))
    (glfw:set-window-size-callback 'ws-callback)
  
    ;; TODO where to put this code?
    (glfw:set-input-mode :cursor :disabled)
    (gl:viewport 0 0 width height)
    (gl:clear-color 0.2 0.3 0.3 1.0)
    (gl:enable :depth-test))
  (call-next-method))

(defun )

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

(defmethod canvas-predraw ((canvas window))
  (glfw:make-context-current (%handle-of canvas))
  (setf *current-context-index*
        (cdr (assoc (%handle-of canvas) *context-map* :test #'cffi:pointer-eq)))
  (gl:viewport 0 0 (width canvas) (height canvas)))

(defmethod canvas-postdraw ((canvas window))
  (glfw:swap-buffers (%handle-of canvas))
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


;; 
