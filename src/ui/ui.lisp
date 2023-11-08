

;; bodge-ui vs bodge-nuclear
;(ql:quickload '(:bodge-nuklear :bodge-ui :bodge-ui/renderer :bodge-host))
(ql:quickload '(:bodge-nuklear :bodge-ui :bodge-ui/renderer)) ;  :bodge-host))

(cl:defpackage :euclidea-ui
  (:export :run)
  (:use :cl :bodge-ui :bodge-ui.renderer))

(in-package :euclidea-ui)

(defpanel (demo-window
            (:title "Hello Bodge UI")
            (:origin 200 50)
            (:width 400) (:height 400)
            (:options :movable :resizable
                      :minimizable :scrollable
                      :closable)
            (:style :panel-padding (bodge-math:vec2 10 10)))
  (label :text "Nested:")
  (horizontal-layout
   (radio-group
    (radio :label "Option 1")
    (radio :label "Option 2" :activated t))
   (vertical-layout
    :style `(:panel-padding ,(bodge-math:vec2 10 10))
    (check-box :label "Check 1" :width 100)
    (check-box :label "Check 2"))
   (vertical-layout
    (label :text "Awesomely" :align :left)
    (label :text "Stacked" :align :centered)
    (label :text "Labels" :align :right)))
  (label :text "Expand by width:")
  (horizontal-layout
   (button :label "Dynamic")
   (button :label "Min-Width" :width 80)
   (button :label "Fixed-Width" :expandable nil :width 100))
  (label :text "Expand by ratio:")
  (horizontal-layout
   (button :label "1.0" :expand-ratio 1.0)
   (button :label "0.75" :expand-ratio 0.75)
   (button :label "0.5" :expand-ratio 0.5))
  (label :text "Rest:")
  (button :label "Top-Level Button"))





(defvar *window-width* 800)
(defvar *window-height* 600)

;; Main class of our application
(defclass euclidea-ui-app (bodge-host:window)
  ;; UI context
  ((context :initform nil)
   ;; Renderer context
   (renderer :initform nil)
   ;; and some state we need later
   (enabled-p :initform t)
   (mouse-actions :initform (list))
   (cursor :initform (bodge-math:vec2)))
  (:default-initargs
   ;; For the example we use OpenGL 3.3.
   ;; That's what default renderer requires.
   :opengl-version '(3 3)
   :title "Bodge UI Example"
   :width *window-width*
   :height *window-height*
   :autoscaled nil))


(defmethod bodge-host:on-mouse-action ((this bodge-ui-app) button action)
  (with-slots (mouse-actions) this
    (alexandria:nconcf mouse-actions (list (cons button action)))))


(defmethod bodge-host:on-cursor-movement ((this bodge-ui-app) x y)
  (with-slots (cursor) this
    (setf (bodge-math:x cursor) x
          (bodge-math:y cursor) y)))


(defun setup-rendering-context (application)
  ;; This will bind GL context of a window to the thread of our choice.
  (bodge-host:bind-main-rendering-context application)
  ;; Following small titbit is required to run our renderer
  (glad:init))


(defun initialize-ui (application)
  (with-slots (context renderer) application
    (setf renderer (make-nuklear-renderer *window-width* *window-height*)
          context (make-ui renderer :input-source application))
    ;; A bit of a spoiler here: we are adding our UI window described later in the example
    (add-panel context 'demo-window)))

;; Well, we also need to cleanup after ourselves
(defun release-ui (application)
  (with-slots (context renderer) application
    (bodge-memory:dispose context)
    (destroy-nuklear-renderer renderer)))


(defun render-example-ui (app)
  (with-slots (context) app
    ;; Clear our background with direct OpenGL commands
    (gl:clear-color 0.8 0.8 0.8 0.1)
    (gl:clear :color-buffer-bit)
    ;; Compose and render the UI
    (compose-ui context)
    ;; Bring rendered buffer to the front
    (bodge-host:swap-buffers app)))


(defun run-rendering-loop (application)
  (with-slots (enabled-p) application
    (loop while enabled-p
          do (render-example-ui application))))


(defun start-rendering-thread (application)
  (with-slots (context renderer enabled-p) application
    ;; Start thread which we will use for rendering
    (bodge-concurrency:in-new-thread ("rendering-thread")
      (unwind-protect
           (progn
             ;; Setup rendering context
             (setup-rendering-context application)
             ;; Initialize renderer and UI context
             (initialize-ui application)
             ;; Loop while we can!
             (run-rendering-loop application)
             ;; Release resources after leaving the loop
             (release-ui application))
        ;; Be sure to shutdown whole application before exiting the thread
       (bodge-host:close-window application)))))


(defmethod bodge-host:on-init ((this bodge-ui-app))
  (with-slots (context renderer enabled-p) this
    (setf enabled-p t)
    (start-rendering-thread this)))

(defmethod bodge-host:on-hide ((this bodge-ui-app))
  (with-slots (enabled-p) this
    (setf enabled-p nil)))

(defmethod bodge-host:on-destroy ((this bodge-ui-app))
  (with-slots (enabled-p) this
    (setf enabled-p nil)))


(defmethod next-mouse-interaction ((this bodge-ui-app))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))

(defmethod last-cursor-position ((this bodge-ui-app) &optional result-vec2)
  (with-slots (cursor) this
    (if result-vec2
        (progn
          (setf (bodge-math:x result-vec2) (bodge-math:x cursor)
                (bodge-math:y result-vec2) (bodge-math:y cursor))
          result-vec2)
        cursor)))

(export 'run)
(defun run ()
  (bodge-host:open-window (make-instance 'bodge-ui-app)))
