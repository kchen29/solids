;;;;General utility functions/macros.

(defmacro do-step-max ((var step max) &body body)
  "Iterate for VAR STEP times from 0 to MAX, inclusive."
  (let ((temp (gensym)))
    `(loop for ,temp upto ,step
           for ,var = (* ,max (/ ,temp ,step))
           do ,@body)))

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

(defun evaluate-polynomial (x &rest coefficients)
  "Evaluates a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  (loop for coeff in coefficients
        for product = 1 then (* x product)
        sum (* coeff product)))

(defmacro roundify (&rest args)
  "Rounds each symbol."
  `(setf ,@(loop for arg in args
                 collect arg
                 collect `(round ,arg))))

(defmacro sortify (index &rest cases)
  "Sorts each case via when and rotate. Checks the symbol at INDEX.
   Has the first case be least, and last case be greatest."
  (let (forms)
    (do* ((case-move cases (cdr case-move))
          (case1 (car cases) (car case-move)))
         ((not case-move))
      (dolist (case2 (cdr case-move))
        (push `(when (> ,(nth index case1) ,(nth index case2))
                 ,@(loop for x in case1
                         for y in case2
                         collect `(rotatef ,x ,y)))
              forms)))
    `(progn ,@(nreverse forms))))

(defmacro macrolet-helper (&body body)
  "Defines a macrolet with BODY and evaluates it."
  (let ((temp (gensym)))
    `(macrolet ((,temp () ,@body))
       (,temp))))
