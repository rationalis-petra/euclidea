
(defun is-player (entity)
  (with-components entity
    (entity-info info)
    (when info
      (when (info-tags & PlayerTag)
        t))))

;; TODO: find out how to use bits for flags!
(defun is-enemy (entity)
  (let ((entity-info (get-component 'info entity)))
    (when info
      (when (info-tags & EnemyTag)
        t))))

(defun is-static (entity)
  (and (has-component 'rigidbody entity)
       (has-component 'model entity)))

;; Stringify-state not needed?
