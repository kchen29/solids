;;;;General utility functions/macros.

;;;macros
;;general macros
(defmacro macrolet-helper (&body body)
  "Defines a single macrolet with BODY and evaluates it."
  (let ((temp (gensym)))
    `(macrolet ((,temp () ,@body))
       (,temp))))

;;it's possible to allow the user to choose whether to explicitly name the variable or not
;;not implemented though
(defmacro collect-to (&body body)
  "Defines a flet to collect objects. The reverse of the collected is returned."
  (let ((var (gensym)))
    `(let (,var)
       (flet ((collect (obj) (push obj ,var)))
         ,@body
         (nreverse ,var)))))

;;doesn't actually handle multiple bases; it's currently there for aesthetics.
;;the iteration value for each of the loops should be a list or positive integer
;;ideally should be a function
(defmacro generate (loops &body bases)
  "Generates nested loops and collects BASES forms."
  `(collect-to
     ,(do ((head `(collect ,(car bases)) (list (if (listp (cadar forms)) 'dolist 'dotimes)
                                               (car forms) head))
           (forms (nreverse loops) (cdr forms)))
          ((null forms) head))))

;;control constructs
(defmacro do-step-max ((var step max) &body body)
  "Iterate for VAR STEP times from 0 to MAX, inclusive."
  (let ((temp (gensym)))
    `(loop for ,temp upto ,step
           for ,var = (* ,max (/ ,temp ,step))
           do ,@body)))

(defmacro do-step ((var count step) &body body)
  "Iterate for VAR from 0 below COUNT with STEP interval."
  `(do ((,var 0 (+ ,step ,var)))
       ((>= ,var ,count))
     ,@body))

(defmacro do-pairwise ((var1 var2 list) &body body)
  "Iterates over LIST pairwise, with VAR1 and VAR2."
  (let ((list-move (gensym)))
    `(do* ((,list-move ,list (cdr ,list-move))
           (,var1 (car ,list) (car ,list-move)))
          ((not ,list-move))
       (dolist (,var2 (cdr ,list-move))
         ,@body))))

(defmacro switch (value test &body cases)
  "Macro for switch-case statements.
   TESTs VALUE with the first element in each case of CASES.
   If otherwise is the first element, then it acts as the default case."
  `(cond
     ,@(loop for case in cases
             for test-value = (first case)
             for return-value = (rest case)
             if (eql 'otherwise test-value)
               collect `(t ,@return-value)
             else
               collect `((funcall ,test ,value ,test-value) ,@return-value))))

;;symbol manipulation
(defmacro roundify (&rest args)
  "Rounds each symbol."
  `(setf ,@(loop for arg in args
                 collect arg
                 collect `(round ,arg))))

(defmacro sortify (index &rest cases)
  "Sorts each case via when and rotatef. Checks the symbol at INDEX.
   Has the first case be least, and last case be greatest."
  `(progn
     ,@(collect-to
         (do-pairwise (case1 case2 cases)
           (collect `(when (> ,(nth index case1) ,(nth index case2))
                       ,@(loop for x in case1
                               for y in case2
                               collect `(rotatef ,x ,y))))))))

;;;functions
(defun evaluate-polynomial (x &rest coefficients)
  "Evaluates a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  (loop for coeff in coefficients
        for product = 1 then (* x product)
        sum (* coeff product)))

(defun fround-to (number decimals)
  "Rounds NUMBER to DECIMALS places."
  (/ (fround (* number (expt 10 decimals)))
     (expt 10 decimals)))

(defun diff-quot (a b c d)
  "Calculates the difference quotient of A minus B divided by C minus D."
  (/ (- a b) (- c d)))

(defun copy-array (array)
  "Copies an array."
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims :displaced-to array) dims)))

(defun concat-symbol (&rest args)
  "Takes symbols and strings to form a new symbol."
  (intern (apply #'concatenate 'string
                 (mapcar (lambda (x) (string-upcase (string x))) args))))

(defun concat-string (&rest args)
  "Takes symbols and strings to form a string."
  (apply #'concatenate 'string
         (mapcar (lambda (x)
                   (if (symbolp x)
                       (string-downcase (string x))
                       x))
                 args)))

