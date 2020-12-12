;; list = (a b c) -> (a b c)
;; list = (a b c d) -> (a b c c d a) (for square faces)
;; TODO: fix for n-dimensional... currently only works for 3 & 4
(defun to-triples (data)
  "function which takes a face of n vertices & decomposes it into triangles"
  (if (= (length data) 3) data
      (list (elt data 0)
            (elt data 1)
            (elt data 2)
            (elt data 2)
            (elt data 3)
            (elt data 0))))

(defmacro add-data (data mesh)
  `(cond
     ((string= (car ,data) "v")
      (dolist (vertex (cdr ,data))
        (vector-push-extend (read-from-string vertex) (getf ,mesh :vertices) 1)))
     ((string= (car ,data) "f")
      (dolist (index (to-triples (cdr ,data)))
        (vector-push-extend
         (read-from-string (car (split-sequence:split-sequence #\/ index)))
         (getf ,mesh :indices) 1)))))

(defun load-obj (filename)

  (let ((mesh (list
               :vertices (make-array 1 :fill-pointer 0)
               :indices (make-array 1 :fill-pointer 0))))

    (with-open-file (obj-file filename
                              :direction :input)
      (loop for line = (read-line obj-file nil)
            while line do
              (let ((data (split-sequence:split-sequence #\Space line)))
                (add-data data mesh)))
      mesh)))

