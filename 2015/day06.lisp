(defstruct command
  verb
  bottom-left
  upper-right)

(defun read-pair (str)
  (let ((parts (uiop:split-string str :separator ",")))
    (cons (parse-integer (car parts))
          (parse-integer (cadr parts)))))

(defun read-command (str)
  (let* ((subcmd (subseq str 
                         (position-if #'digit-char-p str)))
         (subcmd-tokens (uiop:split-string subcmd))
         (bottom-left (read-pair (car subcmd-tokens)))
         (upper-right (read-pair (caddr subcmd-tokens))))
    (cond ((uiop:string-prefix-p "toggle" str)
           (make-command :verb :toggle
                           :bottom-left bottom-left
                           :upper-right upper-right))
          ((uiop:string-prefix-p "turn on" str)
           (make-command :verb :on
                           :bottom-left bottom-left
                           :upper-right upper-right))
          ((uiop:string-prefix-p "turn off" str)
           (make-command :verb :off
                           :bottom-left bottom-left
                           :upper-right upper-right)))))

(defun run-verb-on-cell (verb i j grid side)
  (let ((index (+ i (* j side))))
    (cond ((eq :toggle verb)
           (setf (aref grid index) (- 1 (aref grid index))))
          ((eq :on verb)
           (setf (aref grid index) 1))
          ((eq :off verb)
           (setf (aref grid index) 0)))))

(defun run-command (verb-fn command grid side)
  (loop for i from (car (command-bottom-left command)) 
          to (car (command-upper-right command))
        doing
           (loop for j from (cdr (command-bottom-left command))
                   to (cdr (command-upper-right command))
                 do (funcall verb-fn (command-verb command) i j grid side))))

(defun day06-a (filename side &key (verbose nil))
  (with-open-file (stream filename)
    (do ((grid (make-array (* side side)
                           :fill-pointer nil))
         (line (read-line stream)
               (read-line stream nil)))
        ((null line)
         (return (- (length grid)
                    (count-if #'zerop grid))))
      (let ((cmd (read-command line)))
        (run-command #'run-verb-on-cell cmd grid side)
        (when verbose
          (print cmd)
          (print grid))))))

(defun day06-b (filename side &key (verbose nil))
  (with-open-file (stream filename)
    (do ((grid (make-array (* side side)
                           :fill-pointer nil))
         (line (read-line stream)
               (read-line stream nil)))
        ((null line)
         (return (reduce #'+ grid)))
      (let ((cmd (read-command line)))
        (run-command (lambda (verb i j grid side)
                       (let ((index (+ i (* j side))))
                         (cond ((eq :toggle verb)
                                (setf (aref grid index)
                                      (+ 2 (aref grid index))))
                               ((eq :on verb)
                                (incf (aref grid index)))
                               ((eq :off verb)
                                (setf (aref grid index)
                                      (max 0 (1- (aref grid index))))))))
                     cmd
                     grid
                     side)
        (when verbose
          (print cmd)
          (print grid))))))
