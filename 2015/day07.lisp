(ql:quickload 'hash-set)

(defstruct edge
  u v)

(defstruct graph
  edges
  vertices)

(defun make-connected-dag-from-edges (edges)
  (let ((us (hash-set:list-to-hs (mapcar #'edge-u edges)))
        (vs (hash-set:list-to-hs (mapcar #'edge-v edges))))
    (make-graph :edges edges
                :vertices (sort (hash-set:hs-to-list (hash-set:hs-union us vs))
                                #'<))))

(defstruct wire
  name
  op
  input1
  input2)

(defun read-wire-input (token)
  (if (every #'digit-char-p token)
      (parse-integer token)
      token))

(defun read-wire-def (line)
  (let* ((assignment (mapcar #'str:trim (str:split "->" line)))
         (wire-def (str:words (car assignment)))
         (wire-name (cadr assignment)))
    (cond ((= (length wire-def) 1)
           (make-wire :name wire-name
                      :input1 (read-wire-input (car wire-def))))
          ((= (length wire-def) 2)
           (assert (string= "NOT" (car wire-def))
                   (line)
                   "Unexpected unary wire: ~s" line)
           (make-wire :name wire-name
                      :input1 (read-wire-input (cadr wire-def))
                      :op (car wire-def)))
          ((= (length wire-def) 3)
           (make-wire :name wire-name
                      :input1 (read-wire-input (car wire-def))
                      :op (cadr wire-def)
                      :input2 (read-wire-input (caddr wire-def)))))))

(defun read-circuit (filename)
  (with-open-file (stream filename)
    (loop with circuit = (make-hash-table :test ##'equalp)
          for line = (read-line stream nil)
          while line
          finally (return circuit)
          do
             (let ((parsed-wire (read-wire-def line)))
               (multiple-value-bind (_ present)
                   (gethash (wire-name parsed-wire) circuit)
                 (declare (ignore _))
                 (assert (not present)
                         (parsed-wire)
                         "Violated invariant: multiple definitions for wire ~S" parsed-wire))
               (setf (gethash (wire-name parsed-wire) circuit) parsed-wire)))))

(defun build-depgraph (circuit)
  (with-hash-table-iterator (generator circuit)
    (loop with edges = '()
          with vertices = '()
          for (more? key value) = (multiple-value-list (generator))
          while more?
          when (stringp (wire-input1 value)) 
            do (push (make-edge :u (wire-input1 value)
                                :v key)
                     edges)
          when (stringp (wire-input2 value))
            do (push (make-edge :u (wire-input2 value)
                                :v key)
                     edges)
          do (push key vertices)
          finally (return (make-graph :edges edges
                                      :vertices (remove-duplicates vertices))))))

(defun root-vertices (graph)
  (hash-set:hs-to-list (hash-set:hs-difference
                        (hash-set:list-to-hs (graph-vertices graph))
                        (hash-set:list-to-hs (mapcar #'edge-v (graph-edges graph))))))

(defun build-edge-map (graph)
  (loop with fwd = (make-hash-table :test #'equalp)
        with bck = (make-hash-table :test #'equalp)
        for edge in (graph-edges graph)
        do (push (edge-v edge) (gethash (edge-u edge) fwd '()))
           (push (edge-u edge) (gethash (edge-v edge) bck '()))
        finally (return (values fwd bck))))

(defvar *sample-graph*
  (make-connected-dag-from-edges (list (make-edge :u 5 :v 11)
                                       (make-edge :u 7 :v 11)
                                       (make-edge :u 7 :v 8)
                                       (make-edge :u 3 :v 8)
                                       (make-edge :u 3 :v 10)
                                       (make-edge :u 11 :v 2)
                                       (make-edge :u 11 :v 9)
                                       (make-edge :u 11 :v 10)
                                       (make-edge :u 8 :v 9))))

(defun topo-sort (graph)
  (multiple-value-bind (fwd-edges back-edges)
      (build-edge-map graph)
    (loop with sorted-vertices = '()
          with horizon = (root-vertices graph)
          while horizon
          for n = (pop horizon)
          do (push n sorted-vertices)
             (loop for tail in (gethash n fwd-edges)
                   do (unless (setf (gethash tail back-edges)
                                    (delete n (gethash tail back-edges)
                                            :test #'equalp))
                        (push tail horizon)
                        (remhash tail back-edges)))
          finally (assert (= (hash-table-count back-edges) 0)
                          (back-edges)
                          "Violated invariant: found cycle")
                  (return (reverse sorted-vertices)))))

(defun eval-wire (wire-def wire-env)
  (let* ((op (wire-op wire-def))
         (input1 (wire-input1 wire-def))
         (v1 (if (numberp input1)
                 input1
                 (gethash input1 wire-env)))
         (input2 (wire-input2 wire-def))
         (v2 (if (numberp input2)
                 input2
                 (gethash input2 wire-env))))
    (cond ((null op)
           v1)
          ((string= "AND" op)
           (logand v1 v2))
          ((string= "OR" op)
           (logior v1 v2))
          ((string= "NOT" op)
           (- 65535 v1))
          ((string= "LSHIFT" op)
           (logand 65535 (ash v1 v2)))
          ((string= "RSHIFT" op)
           (ash v1 (- v2))))))

(defun day05-a (filename &key (debug nil))
  (gethash "a"
           (loop with circuit = (read-circuit filename)
                 with deps = (build-depgraph circuit)
                 with env = (make-hash-table :test #'equalp)
                 for node in (topo-sort deps)
                 do (when debug
                      (format t "~s: ~s (env: ~s)" node (gethash node circuit) env)
                      (terpri))
                    (setf (gethash node env) 
                          (eval-wire (gethash node circuit)
                                     env))
                 finally (return env))))

(defun day05-b (filename)
  (let ((initial-value (day05-a filename))
        (circuit (read-circuit filename)))
    (gethash "a"
             (loop initially (setf (gethash "b" circuit)
                                   (make-wire :name "b"
                                              :input1 initial-value))
                   with deps = (build-depgraph circuit)
                   with env = (make-hash-table :test #'equalp)
                   for node in (topo-sort deps)
                   do (setf (gethash node env) 
                            (eval-wire (gethash node circuit)
                                       env))
                   finally (return env)))))
