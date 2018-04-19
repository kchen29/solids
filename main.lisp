(defun main (filename)
  "Sets up then parses FILENAME. See parser.lisp."
  (let ((edges (make-matrix))
        (polygons (make-matrix))
        (stack (list (make-transform-matrix))))
    (parse-file filename edges polygons stack)))
