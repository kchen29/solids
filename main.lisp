(defun main (filename)
  "Sets up for parsing FILENAME. See parser.lisp."
  (parse-file filename (make-matrix) (make-matrix) (list (make-transform-matrix))))
