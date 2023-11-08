
;; functions which are essentially re-exporting symbols this will
;; be redone when I use a proper system definition facility/packages
(defun key-is-pressed-p (key)
  (eq (glfw:get-key key *window*) :press))

(defun window-should-close-p ()
  (glfw:window-should-close-p *window*))

(defun poll-events ()
  (glfw:poll-events))

(defun get-cursor-pos ()
  "Return cursor position as a 2-vector"
  (make-array 2 :initial-contents
              (glfw:get-cursor-position *window*)))
