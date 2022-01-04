(use-package 'arrow-macros)

(defun increment-char (c)
  (let ((aa (char-code #\a))
        (zz (char-code #\z)))
    (multiple-value-bind (quotient remainder) 
        (floor (+ 1
                  (- (char-code c) aa))
               (- zz aa -1))
      (values (code-char (+ remainder aa))
              (not (zerop quotient))))))

(defun increment-passwd (passwd)
  (loop with pass-len = (length passwd)
        for j from 0 to (1- pass-len)
        for i = (- (1- pass-len)
                   (mod j pass-len))
        for (newch carry) = (multiple-value-list (increment-char (char passwd i)))
        do (setf (char passwd i) newch)
        while carry
        finally (return passwd)))

(defun triple-rule-p (passwd)
  (some (lambda (x y z)
          (multiple-value-bind (a b c)
              (values-list (mapcar #'char-code (list x y z)))
            (= (1+ a) b (1- c))))
        passwd
        (subseq passwd 1)
        (subseq passwd 2)))

(defun group-by (slist)
  (reduce (lambda (x l)
            (cond ((null l) (list (list x)))
                  ((eq x (caar l)) (cons (cons x (car l))
                                         (cdr l)))
                  (t (cons (list x)
                           l))))
          slist
          :initial-value '()
          :from-end t))

(defun double-rule-p (passwd)
  (let ((nontrivial-groups (->> passwd
                             (group-by)
                             (mapcar (lambda (subseq)
                                       (list (length subseq)
                                             (car subseq))))
                             (remove-if-not (lambda (xy)
                                              (>= (car xy)
                                                  2))))))
    (and (>= (length nontrivial-groups) 2)
         (not (apply #'eq
                     (mapcar #'cadr nontrivial-groups))))))

(defun find-next-passwd (current-passwd)
  (loop for passwd = (increment-passwd current-passwd)
        until (and (double-rule-p passwd)
                   (triple-rule-p passwd)
                   (notany (lambda (c) (find c "ilo")) passwd))
        finally (return passwd)))

(defun day11-a (filename)
  (find-next-passwd (uiop:read-file-string filename)))

(defun day11-b (filename)
  (find-next-passwd (day11-a filename)))
