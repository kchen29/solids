;;;; Draw to screen.

(defmacro draw-line-base (x0 y0 z0 x1 y1 z1 plot-1 plot-2)
  "Base code for octant 1. Other octants can be gotten from transformations."
  `(do* ((x ,x0 (1+ x))
         (y ,y0)
         (z ,z0 (+ z (diff-quot ,z1 ,z0 ,x1 ,x0)))
         (A (- ,y1 ,y0))
         (B (- ,x0 ,x1))
         (2A (* 2 A))
         (2B (* 2 B))
         (d (+ 2A B) (+ d 2A)))
        ((>= x ,x1) (plot x1 y1 z1 color))
     (plot ,plot-1 ,plot-2 z color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 z0 x1 y1 z1 color)
  "Draws a line from (x0, y0) to (x1, y1) on *SCREEN* using COLOR."
  (roundify x0 y0 x1 y1)
  (sortify 0 (x0 y0 z0) (x1 y1 z1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-line-base x0 y0 z0 x1 y1 z1 x y)
            (draw-line-base y0 x0 z0 y1 x1 z1 y x))
        (if (minusp (+ ydif xdif))
            (draw-line-base y0 x0 z0 (- y0 ydif) x1 z1 y (- (* 2 y0) x))
            (draw-line-base x0 y0 z0 x1 (- y0 ydif) z1 x (- (* 2 y0) y))))))

(defun draw-line-index (edges index color)
  "Draws the line starting from INDEX in EDGES."
  (macrolet-helper
    `(draw-line
      ,@(generate ((in 2) (co 3))
          `(mref edges ,co (+ ,in index)))
      color)))

(defun draw-lines (edges color)
  "Draws the lines from EDGES to *SCREEN* with COLOR."
  (do-step (index (m-last-col edges) 2)
    (draw-line-index edges index color)))

;;;3d shapes
(defun draw-polygon (x0 y0 z0 x1 y1 z1 x2 y2 z2)
  "Draws the polygon to *SCREEN*."
  (let ((color (mapcar (lambda (a b c) (mod (round (* a b c)) 256))
                       (list x0 y0 z0)
                       (list x1 y1 z1)
                       (list x2 y2 z2))))
    (draw-line x0 y0 z0 x1 y1 z1 color)
    (draw-line x0 y0 z0 x2 y2 z2 color)
    (draw-line x1 y1 z1 x2 y2 z2 color)
    (scanline x0 y0 z0 x1 y1 z1 x2 y2 z2 color)))

(defun draw-polygon-index (polygons index)
  "Draws the polygon starting from INDEX in POLYGONS"
  (macrolet-helper
    `(draw-polygon
      ,@(generate ((in 3) (co 3))
          `(mref polygons ,co (+ ,in index))))))

(defun draw-polygons (polygons)
  "Draws the polygons from POLYGONS to *SCREEN*."
  (do-step (index (m-last-col polygons) 3)
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

(defun scanline (x0 y0 z0 x1 y1 z1 x2 y2 z2 color)
  "Does scanline conversion."
  (roundify y0 y1 y2)
  ;;have y0 be the bottom, y1 the middle, and y2 the top
  (sortify 1 (x0 y0 z0) (x1 y1 z1) (x2 y2 z2))
  (do ((y y0 (1+ y))
       (a x0 (+ a (diff-quot x2 x0 y2 y0)))
       (b x0)
       (c z0 (+ c (diff-quot z2 z0 y2 y0)))
       (d z0))
      ((>= y y2))
    (cond
      ((< y0 y y1) (incf b (diff-quot x1 x0 y1 y0)) (incf d (diff-quot z1 z0 y1 y0)))
      ((< y1 y y2) (incf b (diff-quot x2 x1 y2 y1)) (incf d (diff-quot z2 z1 y2 y1)))
      ((= y y1) (setf b x1 d z1)))
    (draw-line a y c b y d color)))
