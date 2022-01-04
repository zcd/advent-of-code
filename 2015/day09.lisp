(ql:quickload 'arrow-macros)
(import 'arrow-macros:->>)

(defun read-graph (lines)
  (loop with graph = (make-hash-table :test #'equalp)
        with nodes = '()
        for line in lines
        do (let* ((def-parts (str:split "=" line))
                  (path-parts (str:split "to" (car def-parts)))
                  (src (str:trim (car path-parts)))
                  (dst (str:trim (cadr path-parts)))
                  (weight (parse-integer (cadr def-parts))))
             (setf (gethash (cons src dst) graph) weight)
             (setf (gethash (cons dst src) graph) weight)
             (push src nodes)
             (push dst nodes))
        finally (return (values graph
                                (remove-duplicates nodes :test #'equalp)))))

(defun gen-all-paths (nodes)
  (if nodes
      (->> nodes
        (mapcar (lambda (src)
                  (mapcar (lambda (path)
                            (cons src path))
                          (gen-all-paths (remove src nodes :test #'equalp)))))
        (reduce #'append))
      '(())))

(defun path-len (path graph &key (debug nil))
  (loop with distance = 0
        and hops = 0
        for (src dst) in (mapcar #'list path (cdr path))
        do (multiple-value-bind (weight found?)
               (gethash (cons src dst) graph)
             (unless found?
               (return (values nil hops)))
             (setf distance (+ distance weight))
             (incf hops))
        when debug do
          (format t "~s -> ~s (~s) ~s" src dst path hops)
          (terpri)
        finally (return (values distance hops))))

(defun day09-a (filename &key (debug nil))
  (multiple-value-bind (graph nodes)
      (-> filename
        #'uiop:read-file-lines
        #'read-graph)
    (loop for (path distance hops) in     
                        (->> (gen-all-paths nodes)
                          (mapcar (lambda (path)
                                    (multiple-value-bind (distance hops)
                                        (path-len path graph)
                                      (list path distance hops))))
                          (remove-if-not (lambda (ll)
                                           (multiple-value-bind (path distance hops)
                                               (values-list ll)
                                             (declare (ignore path))
                                             (declare (ignore hops))
                                             (and distance)))))
          when debug
            do (format t "~s (distance: ~s) (hops: ~s)"
                       path distance hops)
          minimizing distance into cmin
          maximizing distance into cmax
          finally (return (values cmin cmax)))))

(defun factorial (n)
  (reduce #'* (alexandria:iota n :start 1)))
