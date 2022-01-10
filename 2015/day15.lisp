(ql:quickload 'alexandria)
(ql:quickload 'arnesi)
(ql:quickload 'arrow-macros)
(use-package 'arrow-macros)
(ql:quickload 'str)
(ql:quickload 'trivia)

(defun read-ingredient (line)
  (trivia:match (->> line
                  (str:split ":")
                  (mapcan (arnesi:curry #'str:split ","))
                  (mapcan #'str:words))
    ((list _ _ cap _ dura _ flav _ tex _ calories)
     `(,(parse-integer cap)
       ,(parse-integer dura)
       ,(parse-integer flav)
       ,(parse-integer tex)
       ,(parse-integer calories)))))

(defun score-cookies (pantry recipe)
  (reduce #'*   
          (->> pantry
            (mapcar #'funcall
                    (mapcar (lambda (k)
                              (arnesi:curry #'mapcar
                                            (arnesi:curry #'* k)))
                            recipe))
            (apply (arnesi:curry #'mapcar #'+))
            (mapcar (arnesi:curry #'max 0)))
          :end (1- (length (car pantry)))
          :initial-value 1))

(defun gen-perturbances (vec-size)
  (loop for i from 0 to (1- vec-size)
        nconc (loop for j from (1+ i) to (1- vec-size)
                    for arr-plus = (make-array vec-size)
                    for arr-minus = (make-array vec-size)
                    do (setf (aref arr-plus i) 1)
                       (setf (aref arr-plus j) -1)
                       (setf (aref arr-minus i) -1)
                       (setf (aref arr-minus j) 1)
                    collect (coerce arr-plus 'list)
                    collect (coerce arr-minus 'list))))

(defun hill-step (x score-fn)
  (-> (loop for dx in (gen-perturbances (length x))
            for candidate = (mapcar #'+ dx x)
            when (notany #'minusp candidate)
              collect (list candidate (funcall score-fn candidate)))
    (sort (lambda (left right)
            (> (cadr left) (cadr right))))
    (car)
    (values-list)))

(defun hill-climb (x score-fn &key (debug nil))
  (loop for pos = x then new-pos
        for score = (funcall score-fn pos)
          then new-score
        for (new-pos new-score) = (multiple-value-list (hill-step pos score-fn))
        when debug
          do (format t "~s (~s)"
                     new-pos new-score)
             (terpri)
        while (> new-score score)
        finally (return (values pos score))))

(defun random-recipe (num-ingredients total)
  (loop with recipe = nil
        while (< (length recipe)
                 (1- num-ingredients))
        do (push (random (- (1+ total)
                            (reduce #'+ recipe)))
                 recipe)
        finally (push (- total
                         (reduce #'+ recipe))
                      recipe)
                (return recipe)))

(defun day15-a (filename &key (debug nil) (num-resets 100))
  (loop with pantry = (->> filename
                        (uiop:read-file-lines)
                        (mapcar #'read-ingredient))
        repeat num-resets
        for (nil maxima) = (multiple-value-list
                            (hill-climb (random-recipe (length pantry)
                                                       100)
                                        (arnesi:curry #'score-cookies pantry)
                                        :debug debug))
        maximizing maxima))

(defun cookie-calories (pantry recipe)
  (->> pantry
    (mapcar (arnesi:curry #'nth 4))
    (mapcar #'* recipe)
    (reduce #'+)))

(defun cartesian-product (&rest lists)
  (if (endp lists)
      '(())
      (mapcan (lambda (tails)
                (mapcar (lambda (v)
                          (cons v tails))
                        (car lists)))
              (apply #'cartesian-product (cdr lists)))))

(defun full-traverse (desired-sum dimensions)
  (loop for xyz in (apply #'cartesian-product
                          (loop repeat (1- dimensions)
                                collecting (alexandria:iota (1+ desired-sum))))
               when (<= (reduce #'+ xyz) desired-sum)
               collecting (cons (- desired-sum (reduce #'+ xyz)) xyz)))

(defun day15-b (filename)
  (loop with pantry = (->> filename
                        (uiop:read-file-lines)
                        (mapcar #'read-ingredient))
        for recipe in (full-traverse 100 (length pantry))
        when (= (cookie-calories pantry recipe) 500)
          maximize (score-cookies pantry recipe) into best-score
        finally (return best-score)))
