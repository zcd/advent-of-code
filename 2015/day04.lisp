(ql:quickload 'ironclad)

(defun advent-md5 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 
                             (ironclad:ascii-string-to-byte-array str))))

(defun find-good-coin (good-coin-fn code)
  (loop for k = 0 then (1+ k)
        until (funcall good-coin-fn
                       (advent-md5 (concatenate 'string
                                                code
                                                (write-to-string k))))
        finally (return k)))

(defun day04-a (filename)
  (with-open-file (stream filename)
    (find-good-coin (lambda (str)
                      (string= "00000"
                               str
                               :start1 0 :end1 5
                               :start2 0 :end2 5))
                    (read-line stream))))

(defun day04-b (filename)
  (with-open-file (stream filename)
    (find-good-coin (lambda (str)
                      (string= "000000"
                               str
                               :start1 0 :end1 6
                               :start2 0 :end2 6))
                    (read-line stream))))
