(ql:quickload :fiveam)

(in-package :matrix)

(fiveam:test matrix-inverse
  ;(matrix:rotate (vector 0.0 0.0 (/pi 3)))
  (fiveam:is (= )))

(fiveam:test matrix*-2)

(fiveam:test matrix*-3)
