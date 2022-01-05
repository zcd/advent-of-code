(defun naked-sum (input)
  (->> (uiop:split-string input
                          :separator '(#\" #\: #\,
                                       #\{ #\}
                                       #\[ #\]))
    (mapcar (lambda (s)
              (parse-integer s :junk-allowed t)))
    (remove-if-not #'identity)
    (reduce #'+)))

(defun day12-a (filename)
  (naked-sum (uiop:read-file-string filename)))

(defun json-obj? (thing)
  (and (typep thing 'list)
       (json-property? (car thing))))

(defun json-property? (thing)
  (and (typep thing 'cons)
       (typep (car thing) 'symbol)))

(defun json-list? (thing)
  (and (typep thing 'list)
       (not (json-obj? thing))
       (not (json-property? thing))))

(defun reduce-json-obj (thing &key (debug nil))
  (if (some (lambda (pair)
              (equalp
               (cdr pair)
               "red"))
            thing)
      0
      (loop initially (when debug
                        (format t "(obj: ~s" thing)
                        (terpri))
            for token in thing
            for value = (cdr token)
            when debug
              do (format t "obj-token: ~s" token)
                 (terpri)
            when (numberp value)
              sum value into total
            when (json-obj? value)
              sum (reduce-json-obj value :debug debug) into total
            when (json-list? value)
              sum (reduce-json-list value :debug debug) into total
            finally
               (when debug
                 (format t "obj-sum: ~s)" total)
                 (terpri))
               (return total))))

(defun reduce-json-list (thing &key (debug nil))
  (loop initially (when debug
                    (format t "(list: ~s" thing)
                    (terpri))
    for token in thing
        when debug
          do (format t "list-token: ~s" token)
             (terpri)
        when (numberp token)
          sum token into total
        when (json-obj? token)
          sum (reduce-json-obj token :debug debug) into total
        when (json-list? token)
          sum (reduce-json-list token :debug debug) into total
        finally
           (when debug
             (format t "list-sum: ~s)" total)
             (terpri))
           (return total)))

(ql:quickload 'cl-json)
(defun day12-b (filename &key (debug nil))
  (with-open-file (stream filename)
    (let ((contents (json:decode-json stream)))
      (cond ((json-obj? contents)
             (reduce-json-obj contents :debug debug))
            ((json-list? contents)
             (reduce-json-list contents :debug debug))))))
