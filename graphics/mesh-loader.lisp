(ql:quickload :split-sequence)

(defun load-obj (filename)
  (macrolet
      ((add-data (data mesh)
         `(cond
           ((string= ,data "v")
            (dolist (vertex data)
              (vector-push (read-from-string vertex) (getf mesh :vertices))))
              
           ((string= ,data "f") 
            (dolist (index data))))))

  (let ((mesh (list
               :vertices (vector)
               :indices (vector))))

    (with-open-file (obj-file filename
                              :direction :input)
      (loop for line = (read-line obj-file nil)
            while line do
              (let ((data (split-sequence:split-sequence #\Space line)))
                (add-data data mesh)
                (print data)))))))

