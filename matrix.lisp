;;;; Matrices and transformations.

;;matrix struct definition
;;somewhat hacky
(defstruct (matrix (:conc-name m-)
                   (:constructor m-matrix)
                   (:copier m-copy-matrix)
                   (:print-object print-matrix))
  rows
  cols
  ;;last-col is how many columns there are
  last-col
  array)

(defmacro mref (matrix x y)
  "Accesses array of MATRIX at X and Y."
  `(aref (m-array ,matrix) ,x ,y))

(defun make-matrix (&key (rows 4) (cols 4) (last-col 0))
  "Makes a matrix."
  (m-matrix :rows rows :cols cols :last-col last-col
            :array (make-array (list rows cols) :adjustable t)))

(defun copy-matrix (matrix)
  "Copies a matrix."
  (let ((copy (m-copy-matrix matrix)))
    (setf (m-array copy) (copy-array (m-array copy)))
    copy))

;;other matrix functions
(defun adjust-matrix (matrix rows cols)
  "Adjusts MATRIX to ROWS and COLS.
   Keeps last-col."
  (adjust-array (m-array matrix) (list rows cols))
  (setf (m-rows matrix) rows)
  (setf (m-cols matrix) cols))

(defun clear-matrix (matrix)
  "Clears MATRIX."
  (adjust-matrix matrix 4 4)
  (setf (m-last-col matrix) 0))

(defun print-matrix (matrix stream)
  "Prints out MATRIX to STREAM."
  (format stream "~{~%~{~a~4,4T~}~}~%" (matrix-to-list matrix)))

(defun matrix-to-list (matrix)
  "Turns MATRIX into a list."
  (loop for x below (m-rows matrix)
        collect (loop for y below (m-last-col matrix)
                      collect (mref matrix x y))))

;;identity, and multiply
(defun to-identity (matrix)
  "Turns MATRIX into an identity matrix. Returns the matrix."
  (dotimes (x (m-rows matrix))
    (dotimes (y (m-last-col matrix))
      (if (= x y)
          (setf (mref matrix x y) 1)
          (setf (mref matrix x y) 0))))
  matrix)

(defun matrix-multiply (m1 m2)
  "A specific matrix multiplication routine. M1 is square.
   Multiplies M1 with M2. Modifies M2 to hold the result. Returns M2."
  (let* ((dimension (m-rows m1))
         (temp (make-array dimension)))
    (dotimes (col (m-last-col m2))
      (dotimes (i dimension)
        (setf (svref temp i) (mref m2 i col)))
      (dotimes (row dimension)
        (setf (mref m2 row col) (dot row m1 temp)))))
  m2)

(defun dot (row m1 temp)
  "Dots the ROW of M1 with TEMP.
   They should have the same corresponding sizes."
  (loop for i below (m-cols m1)
        sum (* (mref m1 row i) (svref temp i))))

;;;transformations
(defun make-transform-matrix ()
  (to-identity (make-matrix :last-col 4)))

(defmacro deftransform (transform-name args &body body)
  "Defuns make-transform given TRANSFORM-NAME, using ARGS and BODY.
   Requires docstring as part of BODY.
   Also defuns transform, applying make-transform to another matrix."
  (let ((make-symbol (concat-symbol "MAKE-" transform-name)))
    `(progn
       (defun ,make-symbol ,args
         ,(concatenate 'string "Makes a matrix that " (pop body))
         (let ((transform (make-transform-matrix)))
           ,@body
           transform))
       (defun ,transform-name (matrix ,@args)
         ,(concat-string "Applies make-" transform-name " to MATRIX.")
         (matrix-multiply (,make-symbol ,@args) matrix)))))

(deftransform translate (delx dely delz)
  "translates by DELX, DELY, and DELZ."
  (setf (mref transform 0 3) delx
        (mref transform 1 3) dely
        (mref transform 2 3) delz))

(deftransform scale (x-scale y-scale z-scale)
  "scales x by X-SCALE, y by Y-SCALE, and z by Z-SCALE."
  (setf (mref transform 0 0) x-scale
        (mref transform 1 1) y-scale
        (mref transform 2 2) z-scale))

(defmacro defrotation (rotate-axis axis-0 axis-1)
  "Defines a rotation around ROTATE-AXIS. AXIS-0 and AXIS-1 mark the value of the axes,
   where x corresponds to 0, y 1, and z 2. Rotates from AXIS-0 to AXIS-1."
  `(deftransform ,(concat-symbol "ROTATE-" rotate-axis) (degrees)
     ,(concat-string "rotates by DEGREES counter-clockwise using "
                     rotate-axis " as the axis.")
     (let ((radians (/ (* degrees pi) 180)))
       (setf (mref transform ,axis-0 ,axis-0) (cos radians)
             (mref transform ,axis-0 ,axis-1) (- 0 (sin radians))
             (mref transform ,axis-1 ,axis-0) (sin radians)
             (mref transform ,axis-1 ,axis-1) (cos radians)))))

(defrotation z 0 1)
(defrotation x 1 2)
(defrotation y 2 0)

(defun make-rotate (axis degrees)
  "Makes a matrix that rotates by DEGREES counter-clockwise using AXIS."
  (case axis
    (x (make-rotate-x degrees))
    (y (make-rotate-y degrees))
    (z (make-rotate-z degrees))
    (otherwise (format t "Unknown axis: ~a~%" axis))))

(defun rotate (matrix axis degrees)
  "Rotate MATRIX by the rotation matrix with AXIS by DEGREES."
  (matrix-multiply (make-rotate axis degrees) matrix))
