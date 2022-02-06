(ql:quickload 'alexandria)
(ql:quickload 'arrow-macros)
(ql:quickload 'str)
(ql:quickload 'trivia)

(defpackage day16
  (:use :arrow-macros :cl)
  (:import-from :alexandria :assoc-value :make-keyword)
  (:import-from :str :split :trim)
  (:import-from :trivia :match))

(defun nth-sue (label)
  (parse-integer (cadr (split " " label :limit 2))))

(defun parse-attr (attr)
  (match (split ":" attr :limit 2)
    ((list k v)
     (values (read-from-string (trim k)) (parse-integer (trim v))))))

(defstruct sue
  n
  attrs)

(defun parse-sue (line)
  (let* ((sue-def (split ":" line :limit 2))
         (attrs (loop for raw-attr in (split "," (cadr sue-def))
                      for (k v) = (multiple-value-list (parse-attr (trim raw-attr)))
                      collecting (cons k v))))
    (make-sue :n (nth-sue (car sue-def))
              :attrs attrs)))

(defun satisfies-sue (benchmark candidate)
  (loop for (k . v) in (sue-attrs benchmark)
        for (sue-v found) = (multiple-value-list (assoc-value (sue-attrs candidate)
                                                              k))
        never (and found (/= sue-v v))))

(defvar *mystery-sue*
  (parse-sue "Sue 0: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"))

(defun day16-a (filename)
  (with-open-file (fs filename)
    (loop for line = (read-line fs nil)
          while line
          for sue = (parse-sue line)
          when (satisfies-sue *mystery-sue* sue)
            collect sue)))

(defun satisfies-sue-with-recalibration (benchmark candidate)
  (loop for (k . v) in (sue-attrs benchmark)
        for (sue-v found) = (multiple-value-list (assoc-value (sue-attrs candidate)
                                                              k))
        never (and found
                   (cond ((eq 'cats k) (<= sue-v v))
                         ((eq 'trees k) (<= sue-v v))
                         ((eq 'goldfish k) (>= sue-v v))
                         ((eq 'pomeranians k) (>= sue-v v))
                         (t (/= sue-v v))))))

(defun day16-b (filename)
  (with-open-file (fs filename)
    (loop for line = (read-line fs nil)
          while line
          for sue = (parse-sue line)
          when (satisfies-sue-with-recalibration *mystery-sue* sue)
            collect sue)))
