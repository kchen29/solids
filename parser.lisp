;;;; Parse a script.

(defun parse-file (filename edges polygons stack)
  "Parses FILENAME. Uses EDGES and TRANSFORM matrices to store edges
   and the transform matrix. Commands write to *SCREEN*.
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

	 ident: set the transform matrix to the identity matrix
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
	 apply: apply the current transformation matrix to the edge matrix
          
	 display: draw the lines of the edge matrix to the screen, then display the screen
	 save: draw the lines of the edge matrix to the screen
	    save the screen to a file -
	    takes 1 argument (filename)
         clear: clears the edge matrix of all points
	 quit: end parsing."
  (with-open-file (stream filename)
    (do ((line (next-line stream) (next-line stream)))
        ((string= line "quit"))
      (if (valid-command line)
          (parse-line line stream edges polygons stack)
          (format t "Unknown command: ~a~%" line)))))

(defun parse-line (line stream edges polygons stack)
  "Parses line according to parse-file."
  (switch line #'string=
    ("display" (display t)
               (clear-screen))
    ("push" (push (copy-matrix (caar stack)) (car stack)))
    ("pop" (pop (car stack)))
    (otherwise
     (let ((args (parse-args (next-line stream))))
       (switch line #'string=
         ("line" (apply #'add-edge edges args)
                 (matrix-multiply (caar stack) edges)
                 (draw-lines edges '(255 0 255))
                 (clear-matrix edges))
         ("circle" (apply #'add-circle edges .01 args)
                   (matrix-multiply (caar stack) edges)
                   (draw-lines edges '(255 0 255))
                   (clear-matrix edges))
         ("hermite" (apply #'add-hermite edges .01 args)
                    (matrix-multiply (caar stack) edges)
                    (draw-lines edges '(255 0 255))
                    (clear-matrix edges))
         ("bezier" (apply #'add-bezier edges .01 args)
                   (matrix-multiply (caar stack) edges)
                   (draw-lines edges '(255 0 255))
                   (clear-matrix edges))
         
         ("box" (apply #'add-box polygons args)
                (matrix-multiply (caar stack) polygons)
                (draw-polygons polygons '(255 0 255))
                (clear-matrix polygons))
         ("sphere" (apply #'add-sphere polygons 20 args)
                   (matrix-multiply (caar stack) polygons)
                   (draw-polygons polygons '(255 0 255))
                   (clear-matrix polygons))
         ("torus" (apply #'add-torus polygons 20 args)
                  (matrix-multiply (caar stack) polygons)
                  (draw-polygons polygons '(255 0 255))
                  (clear-matrix polygons))
         
         ("scale" (apply #'scale (caar stack) args))
         ("move" (apply #'translate (caar stack) args))
         ("rotate" (apply #'rotate (caar stack) args))
         
         ("save" (save (string-downcase (symbol-name (first args))))
                 (clear-screen)))))))

(defun valid-command (line)
  "Returns t if line is a valid command. Nil otherwise."
  (member line
          '("pop" "push" "line" "circle" "hermite" "bezier" "box" "sphere" "torus"
            "scale" "move" "rotate" "display" "save")
          :test #'string=))

(defun next-line (stream)
  "Reads the next line in stream. Returns \"quit\" if eof is reached."
  (read-line stream nil "quit"))

(defun parse-args (line)
  "Given LINE (a string), parse it into a list of args."
  (read-from-string (concatenate 'string "(" line ")")))
