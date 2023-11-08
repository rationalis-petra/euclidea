;;; SHADER
;; these functiosn are simple interfaces to the OpenGL facilities which allow
;; interfacing with shaders

(defun new-shader-program (program_name)
  "Creates a new shader program given a string/path, automatically appends '.vert' and '.frag'"
  ;; initialize variables
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (shader-program (gl:create-program))
        (vertex-shader-path
          (asdf:system-relative-pathname :euclidea (concatenate 'string program_name ".vert")))
        (fragment-shader-path
          (asdf:system-relative-pathname :euclidea (concatenate 'string program_name ".frag"))))

    ;; compile the vertex & fragment shaders
    (with-open-file (vert-file vertex-shader-path
                               :direction :input)
      (gl:shader-source vertex-shader (uiop:read-file-string vert-file))
      (gl:compile-shader vertex-shader)
      ;; report any errors
      (let ((log (gl:get-shader-info-log vertex-shader)))
        (unless (equal "" log) (error log))))

    (with-open-file (frag-file fragment-shader-path
                               :direction :input)
      (gl:shader-source fragment-shader (uiop:read-file-string frag-file))
      (gl:compile-shader fragment-shader)
      ;; report any errors
      (let ((log (gl:get-shader-info-log fragment-shader)))
        (unless (equal "" log) (error log))))

    ;; link the shaders into a program
    (gl:attach-shader shader-program vertex-shader)
    (gl:attach-shader shader-program fragment-shader)
    (gl:link-program shader-program)
    ;; output the info log so errors are reported
    (let ((log (gl:get-program-info-log shader-program)))
      (unless (equal "" log) (error log)))

     ;; cleanup
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    ;; return value
    shader-program))
    

(defun set-uniform (uniform value)
  "A generic set-uniform function which will detect the type of the value passed"
  ;; TODO: expand this to use more types, currently is basically a convenient wrapper
  ;;       to avoid having to place matrix::matrix-data everywhere...
  (cond ((typep value 'matrix:matrix)
         (gl:uniform-matrix-4fv uniform (matrix::matrix-data value)))
        (t (error (format nil "called set-uniform with unsupported value: ~a ~%" value)))))


(defun get-uniform (shader uniform-name)
  "Given handles to a shader, and a string uniform-name, returns a handle to a uniform"
  (gl:get-uniform-location shader uniform-name))
