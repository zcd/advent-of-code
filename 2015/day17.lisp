(ql:quickload 'arrow-macros)
(defpackage day17
  (:use :arrow-macros :cl))

(defun fill-eggnog (total containers)
  (cond ((= 0 total) 1)        
        ((null containers) 0)
        ((> (car containers) total) (fill-eggnog total (cdr containers)))
        (t (+ (fill-eggnog (- total (car containers)) (cdr containers))
              (fill-eggnog total (cdr containers))))))

(defun day17-a (filename)
  (fill-eggnog 150
               (sort (with-open-file (fs filename)
                       (loop for line = (read-line fs nil)
                             while line
                             collecting (parse-integer line)))
                     #'>)))

(defun enumerate-containers (total containers solution)
  (cond ((= 0 total) (list solution))        
        ((null containers) nil)
        ((> (car containers) total) (enumerate-containers total
                                                          (cdr containers)
                                                          solution))
        (t (append (enumerate-containers (- total (car containers))
                                         (cdr containers)
                                         (cons (car containers) solution))
                   (enumerate-containers total
                                         (cdr containers)
                                         solution)))))

(defun group-by (l)
  (labels ((reducer (element fold)
             (cond ((null fold) (list (list element 1)))
                   ((= (caar fold) element) (cons (list element (1+ (cadar fold)))
                                                  (cdr fold)))
                   (t (cons (list element 1) fold)))))
    (reduce #'reducer l
            :from-end t
            :initial-value nil)))

(defun day17-b (filename)
  (as-> filename xyz
    (with-open-file (fs xyz)
      (loop for line = (read-line fs nil)
            while line
            collecting (parse-integer line)))
    (sort xyz #'>)
    (enumerate-containers 150 xyz nil)
    (mapcar #'length xyz)
    (sort xyz #'<)
    (group-by xyz)
    (cadar xyz)))
