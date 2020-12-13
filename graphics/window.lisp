;;;; Graphics for the ecs - both windowing & rendering
;;; includes - these need to be moved elsewhere...

;;; Window functionality
(defvar *aspect* nil)
(defvar *window* nil)
(defvar *initialized* nil)

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (setf *aspect* (/ w h))
  (gl:viewport 0 0 w h))

(defun new-window (width height)
  (glfw:initialize)
  (setf *aspect* (/ width height))
  (glfw:create-window :width width :height height :title "Euclidia")
  (setf *window* (glfw:get-current-context))
  (glfw:set-input-mode :cursor :disabled)

  (gl:viewport 0 0 width height)
  (gl:clear-color 0.2 0.3 0.3 1.0)
  (gl:enable :depth-test))


(defun delete-window ()
  (glfw:destroy-window *window*)
  (setf *window* nil))

(defun display ()
  (glfw:swap-buffers *window*)
  (gl:clear :color-buffer :depth-buffer))


;;; can we re-export symbols? - relevant to this entire section!
(defun key-is-pressed-p (key)
  (eq (glfw:get-key key *window*) :press))

(defun window-should-close-p ()
  (glfw:window-should-close-p *window*))

(defun poll-events ()
  (glfw:poll-events))

(defun get-cursor-pos ()
  (glfw:get-cursor-position *window*))


;;; Shader functionality
(defun new-shader-program (program_name)
  ;; initialize variables
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (shader-program (gl:create-program))
        (vertex-shader-path (concatenate 'string program_name ".vert"))
        (fragment-shader-path (concatenate 'string program_name ".frag")))

    ;; compile the vertex & fragment shaders
    (with-open-file (vert-file vertex-shader-path
                               :direction :input)
      (gl:shader-source vertex-shader (uiop:read-file-string vert-file))
      (gl:compile-shader vertex-shader)
      (format t "~a~%" (gl:get-shader-info-log vertex-shader)))

    (with-open-file (frag-file fragment-shader-path
                               :direction :input)
      (gl:shader-source fragment-shader (uiop:read-file-string frag-file))
      (gl:compile-shader fragment-shader)
      (format t "~a~%" (gl:get-shader-info-log fragment-shader)))

    ;; link the shaders into a program
    (gl:attach-shader shader-program vertex-shader)
    (gl:attach-shader shader-program fragment-shader)
    (gl:link-program shader-program)
    (format t "~a~%" (gl:get-program-info-log shader-program))

     ;; cleanup
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    ;; return value
    shader-program))
    

(defun get-uniform (shader uniform-name)
  (gl:get-uniform-location shader uniform-name))

