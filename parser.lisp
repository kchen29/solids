;;;; Parse a script.

(defun parse-file (filename edges polygons stack)
  "Parses FILENAME. Uses EDGES and POLYGONS matrices to store edges
   and polygons. STACK is the stack of coordinate systems. Commands write to *SCREEN*.
   The file follows the following format:
     Every command is a single string that takes up a line
     Any command that requires arguments must have those arguments in the second line.
     The commands are as follows:
         push: push a copy of the current top of the coordinate system stack
         pop: removes the top of the cs stack

         line: add a line to the edge matrix -
	    takes 6 arguments (x0 y0 z0 x1 y1 z1)         
         circle: add a circle to the edge matrix - 
	    takes 4 arguments (cx, cy, cz, r)
	 hermite: add a hermite curve to the edge matrix -
            takes 8 arguments (x0, y0, x1, y1, rx0, ry0, rx1, ry1)
	 bezier: add a bezier curve to the edge matrix -
	    takes 8 arguments (x0, y0, x1, y1, x2, y2, x3, y3)

         box: adds a rectangular prism (box) to the edge matrix -
            takes 6 parameters (x, y, z, width, height, depth)
         sphere: adds a sphere to the edge matrix - takes 4 parameters (x, y, z, radius)
         torus: adds a torus to the edge matrix - takes 5 parameters (x, y, z, radius1, radius2)
            radius1 is the radius of the cross-section circles
            radius2 is the radius of the center of those circles rotated around the center by

	 scale: create a scale matrix,
	    then multiply the transform matrix by the scale matrix -
	    takes 3 arguments (sx sy sz)
	 move: create a translation matrix,
	    then multiply the transform matrix by the translation matrix -
	    takes 3 arguments (tx ty tz)
	 rotate: create a rotation matrix,
	    then multiply the transform matrix by the rotation matrix -
	    takes 2 arguments (axis theta) axis should be x, y or z.
            Theta is in degrees
          
	 display: draw the lines of the edge matrix to the screen, then display the screen
	 save: draw the lines of the edge matrix to the screen
	    save the screen to a file -
	    takes 1 argument (filename)
	 quit: end parsing."
  (with-open-file (stream filename)
    (do ((line (next-line stream) (next-line stream)))
        ((string= line "quit"))
      (if (valid-command line)
          (parse-line line stream edges polygons stack)
          (format t "Unknown command: ~a~%" line)))))

(defun parse-line (line stream edges polygons stack)
  "Parses LINE according to parse-file."
  (switch line #'string=
    ("display" (display t))
    ("push" (setf (cdr stack) (cons (copy-matrix (car stack)) (cdr stack))))
    ("pop" (setf (car stack) (cadr stack)
                 (cdr stack) (cddr stack)))
    (otherwise
     (parse-line-args line edges polygons stack (parse-args (next-line stream))))))

(defun parse-line-args (line edges polygons stack args)
  "Parses LINE with arg commands."
  (flet ((post-add-lines ()
           (matrix-multiply (car stack) edges)
           (draw-lines edges '(255 0 255))
           (clear-matrix edges))
         (post-add-polygons ()
           (matrix-multiply (car stack) polygons)
           (draw-polygons polygons '(255 0 255))
           (clear-matrix polygons))
         (update-current-stack (transform)
           (setf (car stack) (matrix-multiply (car stack) transform))))
    (switch line #'string=
      ("line" (apply #'add-edge edges args)
              (post-add-lines))
      ("circle" (apply #'add-circle edges .01 args)
                (post-add-lines))
      ("hermite" (apply #'add-hermite edges .01 args)
                 (post-add-lines))
      ("bezier" (apply #'add-bezier edges .01 args)
                (post-add-lines))
      
      ("box" (apply #'add-box polygons args)
             (post-add-polygons))
      ("sphere" (apply #'add-sphere polygons 20 args)
                (post-add-polygons))
      ("torus" (apply #'add-torus polygons 20 args)
               (post-add-polygons))
      
      ("scale" (update-current-stack (apply #'make-scale args)))
      ("move" (update-current-stack (apply #'make-translate args)))
      ("rotate" (update-current-stack (apply #'make-rotate args)))
      
      ("save" (save (string-downcase (symbol-name (first args))))))))

(defun valid-command (line)
  "Returns t if line is a valid command. Nil otherwise."
  (member line
          '("pop" "push" "line" "circle" "hermite" "bezier" "box"
            "sphere" "torus" "scale" "move" "rotate" "display" "save")
          :test #'string=))

(defun next-line (stream)
  "Reads the next line in stream. Returns \"quit\" if eof is reached."
  (read-line stream nil "quit"))

(defun parse-args (line)
  "Given LINE (a string), parse it into a list of args."
  (read-from-string (concatenate 'string "(" line ")")))
