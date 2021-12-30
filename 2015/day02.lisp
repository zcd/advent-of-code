(defun wrapping-paper (l w h)
  (let ((side-lh (* l h))
        (side-lw (* l w))
        (side-wh (* w h)))
    (+ (* 2 side-lw)
       (* 2 side-lh)
       (* 2 side-wh)
       (min side-lw side-lh side-wh))))

(defun read-dimensions (line)
  (mapcar #'parse-integer
          (uiop:split-string line
                             :separator "x"
                             :max 3)))

(defun day02-a (filename)
  (with-open-file (stream filename)
    (do ((total 0)
         (row (read-line stream)
              (read-line stream nil)))
        ((null row)
         total)
      (setf total
            (+ total (apply #'wrapping-paper
                            (read-dimensions row)))))))

(defun ribbon (l w h)
  (let ((side-lh (+ l h))
        (side-lw (+ l w))
        (side-wh (+ w h)))
    (+ (* l w h)
       (* 2 (min side-lh side-lw side-wh)))))

(defun day02-b (filename)
  (with-open-file (stream filename)
    (do ((total 0)
         (row (read-line stream)
              (read-line stream nil)))
        ((null row)
         total)
      (setf total
            (+ total (apply #'ribbon
                            (read-dimensions row)))))))
