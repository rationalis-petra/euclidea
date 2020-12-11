(defun enemy-system ()
  (loop
    with player-rbody = (get-rigidbody (first-match 'is-player))
    and  enemies      = (predicate-mask #'is-enemy)
    for enemy in enemies
    for diff = (- (get-rigidbody enemy) player-rbody) do
      (unless (= 0 (magnitude diff))
        (setf (get-rigidbody enemy) (normalize diff)))))

(defun enemy-init ())
(defun enemy-clean ())
