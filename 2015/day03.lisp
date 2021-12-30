(defun move-santa (direction loc)
  (cond ((eq direction #\^) (cons (car loc) (1+ (cdr loc))))
        ((eq direction #\v) (cons (car loc) (1- (cdr loc))))
        ((eq direction #\>) (cons (1+ (car loc)) (cdr loc)))
        ((eq direction #\<) (cons (1- (car loc)) (cdr loc)))))

(defun day03-a (filename &key (verbose nil))
  (hash-table-count
   (let ((visited (make-hash-table :test 'equal))
         (origin (cons 0 0)))
     (incf (gethash origin visited 0))
     (with-open-file (stream filename)
       (do ((loc origin
                 (move-santa c loc))
            (c (read-char stream)
               (read-char stream nil)))
           ((null c)
            (return visited))
         (incf (gethash loc visited 0))
         (when verbose
           (print-object visited *standard-output*)
           (terpri)))))))

(defun day03-b (filename &key (verbose nil))
  (hash-table-count
   (let ((visited (make-hash-table :test 'equal))
         (origin (cons 0 0)))
     (incf (gethash origin visited 0))
     (with-open-file (stream filename)
       (do ((real origin)
            (robo origin)
            (step 0
                  (incf step))
            (c (read-char stream)
               (read-char stream nil)))
           ((null c)
            (return visited))
         (if (evenp step)
             (progn
               (setf real (move-santa c real))
               (incf (gethash real visited 0)))
             (progn
               (setf robo (move-santa c robo))
               (incf (gethash robo visited 0))))
         (when verbose
           (print-object visited *standard-output*)
           (terpri)))))))
