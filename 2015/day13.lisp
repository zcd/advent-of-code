(ql:quickload 'arrow-macros)
(use-package 'arrow-macros)
(ql:quickload 'str)
(ql:quickload 'trivia)

(defun read-graph (filename)
  (with-open-file (stream filename)
    (loop with graph = (make-hash-table :test #'equalp)
          with people = '()
          for line = (read-line stream nil)
          while line
          do (trivia:match (str:words (subseq line 0 (1- (length line))))
               ((list person-a _ delta n _ _ _ _ _ _ person-b)
                (let ((happiness (* (if (string= "gain" delta) 1 -1)
                                    (parse-integer n))))
                  (setf (gethash (cons person-a
                                       person-b)
                                 graph)
                        happiness)
                  (push person-a people))))
          finally (return (values graph
                                  (remove-duplicates people :test #'equalp))))))

(defun permutations (items)
  (if items
      (mapcan (lambda (item)
                (mapcar (lambda (subperm)
                          (cons item subperm))
                        (permutations (remove item items
                                              :test #'equalp))))
              items)
      '(())))

(defun eval-happiness (graph seating)
  (reduce #'+
          (mapcar (lambda (u v)
                    (+ (gethash (cons u v) graph)
                       (gethash (cons v u) graph)))
                  (cons (car (last seating)) seating)
                  seating)))

(defun day13-a (filename)
  (multiple-value-bind (graph people)
      (read-graph filename)
    (apply #'max
           (mapcar (lambda (seating)
                     (eval-happiness graph seating))
                   (permutations people)))))

(defun day13-b (filename)
  (multiple-value-bind (graph people)
      (read-graph filename)
    (dolist (person people)
      (setf (gethash (cons "me" person) graph) 0)
      (setf (gethash (cons person "me") graph) 0))
    (push "me" people)
    ; naive recursive definition of permutation causes stack overflow somehow
    (loop with perm-gen = (cl-permutation:make-perm-generator (length people))
          for perm = (funcall perm-gen)
          while perm
          maximize (->> perm
                     (cl-permutation:perm-to-list)
                     (mapcar (lambda (n)
                               (nth (1- n) people)))
                     (eval-happiness graph)) into max-happiness
          finally (return max-happiness))))

