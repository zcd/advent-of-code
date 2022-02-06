(ql:quickload 'arnesi)
(ql:quickload 'arrow-macros)
(defpackage day18
  (:use :arrow-macros :cl)
  (:import-from :arnesi :compose :curry))

(defstruct grid
  height
  width
  data)

(defun read-grid (lines)
  (make-grid :height (length lines)
             :width (length (car lines))
             :data (mapcar (lambda (line)
                             (map 'list (lambda (c)
                                          (if (equal c #\#) 1 0))
                                  line))
                           lines)))

(defmacro raw-get-grid (grid x y)
  `(nth ,x
       (nth ,y
            (grid-data ,grid))))

(defun get-grid (grid x y)
  (if (or (> 0 x) (>= x (grid-width grid))
          (> 0 y) (>= y (grid-height grid)))
      0
      (raw-get-grid grid x y)))

(defun step-light (grid x y)
  (let ((light-val (get-grid grid x y))
        (lit-neighbors (reduce #'+ (list (get-grid grid (1- x) (1- y))
                                         (get-grid grid (1- x) y)
                                         (get-grid grid (1- x) (1+ y))
                                         (get-grid grid x (1- y))
                                         (get-grid grid x (1+ y))
                                         (get-grid grid (1+ x) (1- y))
                                         (get-grid grid (1+ x) y)
                                         (get-grid grid (1+ x) (1+ y))))))
    (if (= 1 light-val)
        (if (or (= 2 lit-neighbors) (= 3 lit-neighbors))
            1
            0)
        (if (= 3 lit-neighbors)
            1
            0))))

(defun step-grid (grid)
  (make-grid :height (grid-height grid)
             :width (grid-width grid)
             :data
             (loop for j from 0 to (1- (grid-height grid))
                   collecting (loop for i from 0 to (1- (grid-width grid))
                                    collecting (step-light grid i j)))))

(defun num-lit (grid)
  (reduce #'+
          (mapcar (curry #'reduce #'+)
                  (grid-data grid))))

(defun day18-a (filename)
  (num-lit (loop with initial-grid = (read-grid (with-open-file (fs filename)
                                         (loop for line = (read-line fs nil)
                                               while line
                                               collecting line)))
                 repeat 100
                 for grid = (step-grid initial-grid) then (step-grid grid)
                 finally (return grid))))

(defun force-corners (grid)
  (setf (raw-get-grid grid 0 0) 1)
  (setf (raw-get-grid grid 0 99) 1)
  (setf (raw-get-grid grid 99 0) 1)
  (setf (raw-get-grid grid 99 99) 1)
  grid)

(defun day18-b (filename)
  (let ((initial-grid (read-grid (with-open-file (fs filename)
                                   (loop for line = (read-line fs nil)
                                         while line
                                         collecting line))))
        (new-step (compose #'force-corners
                           #'step-grid
                           #'force-corners)))
    (num-lit (loop repeat 100
                   for grid = (funcall new-step initial-grid)
                     then (funcall new-step grid)
                   finally (return grid)))))
