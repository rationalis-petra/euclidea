;;;; MAIN
;; This file mostly contains setup functions and simple object/entity
;; definitions
(defvar *engine*)

(setf *engine* (make-instance 'engine))

;; Use the "engine": on initialization, create many objects/portals and link them together
(defun main ()
  (setf *engine* (make-instance 'engine))

  (add-cleanup-func *engine* #'glfw:terminate)
  (add-cleanup-func *engine* (lambda () (mapcar #'delete-window (windows *engine*))))

  (add-system-func *engine* #'input-system)
  (add-system-func *engine* #'render-system)

  ;; TODO: add ability to add window /before init funcs??
  ;;(add-init-func *engine*)
  (add-init-func *engine* #'(lambda ()
                              (make-instance
                               'ec-window
                               :app *engine*
                               :title "Euclidea"
                               :width 1280
                               :height 720)))

  (add-init-func *engine* #'render-init)
  (add-init-func *engine* #'render-init)
  (add-init-func *engine* #'make-portal-shader)
  (add-init-func
   *engine* 
   (lambda ()
     ;; create a bunch of portals & connect them
     (let ((portal-1 (make-instance 'portal
                                    :position #(49.0 0.0 0.0)
                                    :rotation (vector 0.0 0.0 0.0)))
           (portal-2 (make-instance 'portal
                                    :position #(-1.666 0.0 0.0)
                                    :rotation (vector 0.0 0.0 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-3 (make-instance 'portal
                                    :position #(-49.0 0.0 0.0)
                                    :rotation (vector 0.0 pi 0.0)))
           (portal-4 (make-instance 'portal
                                    :position #(1.666 0.0 0.0)
                                    :rotation (vector 0.0 pi 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-5 (make-instance 'portal
                                    :position #(0.0 0.0 49.0)
                                    :rotation (vector 0.0 (/ pi -2) 0.0)))
           (portal-6 (make-instance 'portal
                                    :position #(0.0 0.0 -1.666)
                                    :rotation (vector 0.0 (/ pi -2) 0.0)
                                    :scale #(1.33334 1.33334 1.33334)))
           (portal-7 (make-instance 'portal
                                    :position #(0.0 0.0 -49.0)
                                    :rotation (vector 0.0 (/ pi 2) 0.0)))
           (portal-8 (make-instance 'portal
                                    :position #(0.0 0.0 1.666)
                                    :rotation (vector 0.0 (/ pi 2) 0.0)
                                    :scale #(1.33334 1.33334 1.33334))))
       (connect-portals portal-1 portal-2)
       (connect-portals portal-3 portal-4)
       (connect-portals portal-5 portal-6)
       (connect-portals portal-7 portal-8)


       ;; the list of entities in this scene 
       (setf (world-entities *engine*)
             (list
              (make-instance 'rotating-cube :position #(-50.0 0.0 0.0)
                                            :color #(0.8 0.0 0.0)) ; red
              (make-instance 'rotating-cube :position #(50.0 0.0 0.0)
                                            :color #(0.0 0.8 0.0)) ; green
              (make-instance 'rotating-cube :position #(0.0 0.0 50.0)
                                            :color #(0.0 0.0 0.8)) ; blue
              (make-instance 'rotating-cube :position #(0.0 0.0 -50.0)
                                            :color #(0.8 0.0 0.8)) ; purple
              portal-1
              portal-2
              portal-3
              portal-4
              portal-5
              portal-6
              portal-7
              portal-8
              (make-box #(50.0 0.0 0.0) (vector 0.0 (/ pi -2) 0.0))
              (make-box #(-50.0 0.0 0.0) (vector 0.0 (/ pi 2) 0.0))
              (make-box #(0.0 0.0 50.0) (vector 0.0 pi 0.0))
              (make-box #(0.0 0.0 -50.0) #(0.0 0.0 0.0))
              (make-hollow-cube))))))
   (run *engine*))


;; make an executable depending on implementation/ox
(defun make-executable ()
  (let ((name (if (eql (uiop:detect-os) :os-windows)
                  "euclidea.exe"
                  "euclidea"))
        (implementation (uiop:implementation-type)))
    (cond
      ((eql implementation :sbcl)
       (sb-ext:save-lisp-and-die
        name
        :toplevel #'main
        :executable t))
      ;; TODO: other implementations...
      ;; clozure, abcl, ...
      ((eql implementation :clozure)
       ()))))
