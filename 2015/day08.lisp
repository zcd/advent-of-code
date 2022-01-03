(defun hexanumeric? (c)
  (and (find c "0123456789abcdef")
       t))

(defun barf-char (clist)
  (when clist
    (list (string(car clist)) (cdr clist))))

(defun barf-backslash (clist)
  (when (and (eq #\\ (car clist))
             (eq #\\ (cadr clist)))
    (list (coerce (list (car clist)
                          (cadr clist))
                    'string)
            (cddr clist))))

(defun barf-quote (clist)
  (when (and (eq #\\ (car clist))
             (eq #\" (cadr clist)))
    (list (coerce (list (car clist)
                          (cadr clist))
                    'string)
            (cddr clist))))

(defun barf-hex (clist)
  (when (and (eq #\\ (car clist))
             (eq #\x (cadr clist))
             (hexanumeric? (caddr clist))
             (hexanumeric? (cadddr clist)))
    (list (coerce (list (car clist)
                          (cadr clist)
                          (caddr clist)
                          (cadddr clist))
                    'string)
          (cddddr clist))))

(defun barf-through (clist)
  (let ((hex (barf-hex clist))
        (qt (barf-quote clist))
        (bs (barf-backslash clist))
        (c (barf-char clist)))
    (cond (hex
           (multiple-value-bind (token next)
               (values-list hex)
             (cons token (barf-through next))))
          (qt
           (multiple-value-bind (token next)
               (values-list qt)
             (cons token (barf-through next))))
          (bs
           (multiple-value-bind (token next)
               (values-list bs)
             (cons token (barf-through next))))
          (c
           (multiple-value-bind (token next)
               (values-list c)
             (cons token (barf-through next))))
          (t nil))))

(defun day08-a (filename &key (debug nil))
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          for diff = (- (length line)
                        (- (length (barf-through (coerce line
                                                         'list)))
                           2))
          while line
          when debug do
            (format t "~s: ~s - ~s"
                    line
                    (length line)
                    (- (length (barf-through (coerce line 'list)))
                       2))
            (terpri)
          summing diff)))

(defun encode (s)
  (str:join ""
            (map 'list (lambda (c)
                         (cond ((eq #\\ c)
                                "\\\\")
                               ((eq #\" c)
                                "\\\"")
                               (t
                                (string c))))
                 s)))

(defun day08-b (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          summing (- (+ 2 (length (encode line)))
                     (length line)))))
