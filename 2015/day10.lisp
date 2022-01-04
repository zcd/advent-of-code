(ql:quickload 'arrow-macros)
(use-package 'arrow-macros)

(defun describe-seq (slist)
  (mapcan (lambda (subseq)
            (cons (length subseq) (list (car subseq))))
          (reduce (lambda (x l)
                    (cond ((null l) (list (list x)))
                          ((eq x (caar l)) (cons (cons x (car l))
                                                 (cdr l)))
                          (t (cons (list x)
                                   l))))
                  slist
                  :initial-value '()
                  :from-end t)))

(defun day10-a (filename &key n)
  (loop repeat (1+ n)
        for sq = (->> filename
                   (uiop:read-file-string)
                   (map 'list #'digit-char-p))
        then (describe-seq sq)
        finally (return (length sq))))
