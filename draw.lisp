;;;; Draw to screen.

(defmacro draw-line-base (x0 y0 x1 y1 plot-1 plot-2)
  "Base code for octant 1. Other octants can be gotten from transformations."
  `(do* ((x ,x0 (1+ x))
         (y ,y0)
         (A (- ,y1 ,y0))
         (B (- ,x0 ,x1))
         (2A (* 2 A))
         (2B (* 2 B))
         (d (+ 2A B) (+ d 2A)))
        ((> x ,x1))
     (plot ,plot-1 ,plot-2 color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 x1 y1 color)
  "Draws a line from (x0, y0) to (x1, y1) on *SCREEN* using COLOR."
  (sortify 0 (x0 y0) (x1 y1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-line-base x0 y0 x1 y1 x y)
            (draw-line-base y0 x0 y1 x1 y x))
        (if (minusp (+ ydif xdif))
            (draw-line-base y0 x0 (- y0 ydif) x1 y (- (* 2 y0) x))
            (draw-line-base x0 y0 x1 (- y0 ydif) x (- (* 2 y0) y))))))

(defmacro draw-line-in-helper ()
  (let (forms)
    (dotimes (in 2)
      (dotimes (co 2)
        (push `(mref edges ,co (+ ,in index)) forms)))
    `(draw-line ,@(nreverse forms) color)))

(defun draw-line-index (edges index color)
  "Draws the line starting from INDEX in EDGES."
  (draw-line-in-helper))

(defun draw-lines (edges color)
  "Draws the lines from EDGES to *SCREEN* with COLOR."
  (do ((index 0 (+ 2 index)))
      ((>= index (m-last-col edges)))
    (draw-line-index edges index color)))

;;;3d shapes
(defun draw-polygon (x0 y0 x1 y1 x2 y2)
  "Draws the polygon to *SCREEN*."
  (let ((color (mapcar (lambda (x y) (mod (round (* x y)) 256))
                       (list x0 x1 x2)
                       (list y0 y1 y2))))
    (draw-line x0 y0 x1 y1 color)
    (draw-line x0 y0 x2 y2 color)
    (draw-line x1 y1 x2 y2 color)
    (scanline x0 y0 x1 y1 x2 y2 color)))

(defmacro draw-polygon-in-helper ()
  (let (forms)
    (dotimes (in 3)
      (dotimes (co 2)
        (push `(mref polygons ,co (+ ,in index)) forms)))
    `(draw-polygon ,@(nreverse forms))))

(defun draw-polygon-index (polygons index)
  "Draws the polygon starting from INDEX in POLYGONS"
  (draw-polygon-in-helper))

(defun draw-polygons (polygons)
  "Draws the polygons from POLYGONS to *SCREEN*."
  (do ((index 0 (+ 3 index)))
      ((>= index (m-last-col polygons)))
    (when (forward-facing-p polygons index)
      (draw-polygon-index polygons index))))

;;closure, for efficiency
(let ((temp1 (make-array 3))
      (temp2 (make-array 3)))
  (defun forward-facing-p (polygons index)
    "Returns true if the surface in POLYGONS starting 
     at INDEX is forward-facing."
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (plusp (- (* (svref temp1 0)
                 (svref temp2 1))
              (* (svref temp2 0)
                 (svref temp1 1))))))

(defun scanline (x0 y0 x1 y1 x2 y2 color)
  "Does scanline conversion."
  (roundify y0 y1 y2)
  ;;have y0 be the bottom, y1 the middle, and y2 the top
  (sortify 1 (x0 y0) (x1 y1) (x2 y2))
  (do ((y y0 (1+ y))
       (a x0 (+ a (/ (- x2 x0) (- y2 y0))))
       (b x0)
       changed)
      ((>= y y2))
    (draw-line a y b y color)
    (cond
      ((< y y1) (incf b (/ (- x1 x0) (- y1 y0))))
      (changed (incf b (/ (- x2 x1) (- y2 y1))))
      (t (setf b x1
               changed t)))))
