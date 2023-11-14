(require 'asdf)

(defsystem :euclidea
  :name "Euclidea"
  :version "0.0.1"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "MIT"
  ;; (ql:quickload '(bodge-host bodge-ui bodge-ui/renderer))
  :depends-on (; graphics & windows
               :cl-glfw3
               :cl-opengl
               ;; system
               :bordeaux-threads
               ;; util
               :split-sequence
               :array-operations)
  :components
  ((:module
    "app"
    :pathname "app"
    :depends-on ("src")
    :components
    ((:file "main" :depends-on ("entities" "systems"))
     (:file "window")
     (:module
      "entities"
      :pathname "entities"
      :depends-on ("systems")
      :components
      ((:file "portal")
       (:file "box")
       (:file "hollow-cube")
       (:file "rotating-cube")))
     (:module
      "systems"
      :pathname "systems"
      :components
      ((:file "render-system")
       (:file "input-system")))))

   (:module
    "src"
    :pathname "src"
    :components
    ((:file "engine" :depends-on ("graphics" "input" "math"))
     (:module
      "input"
      :depends-on ("math" "graphics")
      :components
      ((:file "input")))
     (:module
      "graphics"
      :pathname "graphics"
      :depends-on ("math")
      :components
      ((:module
        "shaders"
        :pathname "shaders"
        :components
        ((:file "shader")))
       (:file "camera")
       (:file "mesh-loader")
       (:file "transform")
       (:file "window" :depends-on ("canvas"))
       (:file "canvas")))
     (:module
      "math"
      :pathname "math"
      :components
      ((:file "matrix" :depends-on ("vector"))
       (:file "vector")))))))


