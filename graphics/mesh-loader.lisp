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

(defun load-obj (filename)
  "Function for loading in Waveform (.obj) 3D meshes"

  (let ((mesh (list
               :vertices (make-array 1 :fill-pointer 0)
               :normals (make-array 1 :fill-pointer 0)
               :indices (make-array 1 :fill-pointer 0)
               :normal-indices (make-array 1 :fill-pointer 0)))
        (return-mesh (list
                      :vertices (make-array 1 :fill-pointer 0)
                      :indices (make-array 1 :fill-pointer 0))))

    (with-open-file (obj-file filename
                              :direction :input)
      (loop for line = (read-line obj-file nil)
            while line do
              (let ((data (split-sequence:split-sequence #\Space line)))
                (cond
                  ((string= (car data) "v")
                   (dolist (vertex (cdr data))
                     (vector-push-extend (read-from-string vertex) (getf mesh :vertices) 1)))
                  ((string= (car data) "vn")
                   (dolist (normal (cdr data))
                     (vector-push-extend (read-from-string normal) (getf mesh :normals) 1)))
                  ((string= (car data) "f")
                   (dolist (index (to-triples (cdr data)))
                     (let ((indexes (split-sequence:split-sequence #\/ index)))
                       (vector-push-extend
                        ;; subtract 1 because vertex references start at 1
                        (- (read-from-string (car indexes)) 1)
                        (getf mesh :indices) 1)
                       (vector-push-extend
                        (- (read-from-string (caddr indexes)) 1)
                        (getf mesh :normal-indices) 1))))))))


      ;; for each combination of position/normal indices, we create a /new/ index, and a new vertex
      ;; if that position/normal combination is already in the vertex list, then use it's index
    (loop for vindex across (getf mesh :indices)
          for nindex across (getf mesh :normal-indices)
          with next-index = 0
          with map = (make-hash-table :test #'equal) do
            (if (gethash (cons vindex nindex) map)
                ;; if the index is already in the hash-table, add to indices & continue
                (vector-push-extend (gethash (cons vindex nindex) map) (getf return-mesh :indices) 1)

                ;; otherwise, make a new index, and a new vertex which concatenates (vertex, normal) 
                (progn
                  (vector-push-extend next-index (getf return-mesh :indices) 1)
                  (incf next-index)
                  (loop for i from 0 to 2 do (vector-push-extend
                                              (aref (getf mesh :vertices) (+ (* vindex 3) i))
                                              (getf return-mesh :vertices)
                                              1))
                  (loop for i from 0 to 2 do (vector-push-extend
                                              (aref (getf mesh :normals) (+ (* nindex 3) i))
                                              (getf return-mesh :vertices)
                                              1)))))
    return-mesh))

