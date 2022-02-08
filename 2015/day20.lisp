(defun divisors (n)
  (sort (remove-duplicates (loop with ret = nil
                                 for i from 1 to (floor (sqrt n))
                                 when (= 0 (mod n i))
                                   do (push i ret)
                                      (push (/ n i) ret)
                                 finally (return ret)))
        #'<))

(defun day20-a (n)
  (loop for i from 1
        for divs = (divisors i)
        while (< (* 10 (reduce #'+ divs)) n)
        finally (return i)))

(defun day20-b (n)
  (loop for i from 1
        for divs = (remove-if-not (lambda (d)
                                    (> (* d 50) i))
                                  (divisors i))
        for sum = (* 11 (reduce #'+ divs))
        while (< sum n)
        finally (return i)))
