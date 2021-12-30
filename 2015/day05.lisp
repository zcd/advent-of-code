(defun rotate-string (str n)
  (concatenate 'string
               (subseq str n)
               (subseq str 0 n))) 

(defun nice-string-p (str)
  (loop with vowels = 0
        with paired-flag = nil
        for (a b) in (map 'list #'list
                          str
                          (concatenate 'string
                                       (subseq str 1)
                                       " "))
        never (or (and (eq a #\a) (eq b #\b))
                  (and (eq a #\c) (eq b #\d))
                  (and (eq a #\p) (eq b #\q))
                  (and (eq a #\x) (eq b #\y)))
        finally (return (and (>= vowels 3)
                             paired-flag))
        do
           (when (eq a b)
             (setf paired-flag t))
           (when (find a "aeiou")
             (incf vowels))))

(mapcar #'nice-string-p
        '("ugknbfddgicrmopn"
          "aaa"
          "jchzalrnumimnmhp"
          "haegwjzuvuyypxyu"
          "dvszwmarrgswjxmb"))

(defun day05-a (filename)
  (with-open-file (stream filename)
    (do ((nice-count 0)
         (line (read-line stream)
               (read-line stream nil)))
        ((null line)
         nice-count)
      (when (nice-string-p line)
        (incf nice-count)))))

(defun nice-string-alt-p (str)
  (loop with padded = (concatenate 'string str ".,")
        with paired-flag = nil
        with triplet-flag = nil
        for i from 0 to (1- (length str))
        finally (return (and paired-flag
                             triplet-flag))
        do (when (search (subseq padded i (+ i 2))
                         padded
                         :start2 (+ i 2))
             (setf paired-flag t))
           (let ((a (char padded i))
                 (b (char padded (1+ i)))
                 (c (char padded (+ i 2))))
             (when (and (eq a c)
                        (not (eq a b)))
               (setf triplet-flag t)))))

(mapcar #'nice-string-alt-p '("qjhvhtzxzqqjkmpb"
                              "xxyxx"
                              "uurcxstgmygtbstg"
                              "ieodomkazucvgmuy"))

(defun day05-b (filename)
  (with-open-file (stream filename)
    (do ((nice-count 0)
         (line (read-line stream)
               (read-line stream nil)))
        ((null line)
         nice-count)
      (when (nice-string-alt-p line)
        (incf nice-count)))))
