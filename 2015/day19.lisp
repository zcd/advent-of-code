(ql:quickload 'arnesi)
(ql:quickload 'arrow-macros)
(ql:quickload 'priority-queue)
(ql:quickload 'str)
(ql:quickload 'uiop)

(defpackage day19
  (:use :cl :arrow-macros)
  (:import-from :arnesi :curry)
  (:import-from :str :emptyp :split)
  (:import-from :uiop :read-file-lines))

(defun read-spec (filename)
  (let ((lines (->> filename
                 (read-file-lines)
                 (remove-if #'emptyp))))
    (values (->> lines
              (butlast)
              (mapcar (curry #'split " => ")))
            (->> lines
              (last)
              (car)))))


(defun drug-step (drug-spec medicine)
  (loop with boundary = nil
        for i from 0 to (1- (length medicine))
        do (loop for (prefix newfix) in (remove-if-not (lambda (p)
                                                         (eq i (search p medicine :start2 i)))
                                                       drug-spec
                                                       :key #'car)
                 do (let* ((drug-prefix (subseq medicine 0 i))
                           (drug-remainder (subseq medicine i))
                           (subbed (str::concat drug-prefix
                                                (str:replace-first prefix
                                                                   newfix
                                                                   drug-remainder))))
                      (unless (find subbed boundary :test #'equalp)
                        (push subbed boundary))))
        finally (return boundary)))

(defun day19-a (filename)
  (length (multiple-value-bind (spec medicine)
              (read-spec filename)
            (drug-step spec medicine))))

; Cheated and used https://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4etju/?utm_source=share&utm_medium=web2x&context=3
(defun num-substrings (main-string substring)
  (loop for left = 0 then (+ (length substring) found)
        for found = (search substring main-string :start2 left)
        while found
        summing 1))

(defun day19-b (filename)
  (multiple-value-bind (spec drug)
           (read-spec filename)
         (declare (ignore spec))
         (- (count-if #'upper-case-p drug)
            (+ 1
               (* 2 (num-substrings drug "Y"))
               (num-substrings drug "Ar")
               (num-substrings drug "Rn")))))

