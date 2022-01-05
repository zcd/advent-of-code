(ql:quickload 'arrow-macros)
(use-package 'arrow-macros)

(ql:quickload 'str)
(ql:quickload 'trivia)

(defstruct reindeer
  name
  speed
  sustain-period
  rest-period)

(defun parse-reindeer (line)
  (trivia:match (str:words line)
    ((list name _ _ speed _ _ sustain-period _
           _ _ _ _ _ rest-period _)
     (make-reindeer :name name
                    :speed (parse-integer speed)
                    :sustain-period (parse-integer sustain-period)
                    :rest-period (parse-integer rest-period)))))

(defun assemble-racers (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collecting (parse-reindeer line) into racers
          finally (return (coerce racers 'vector)))))

(defstruct reindeer-state
  run-remaining
  rest-remaining)

(defun new-state (reindeer)
  (make-reindeer-state :run-remaining (reindeer-sustain-period reindeer)
                       :rest-remaining (reindeer-rest-period reindeer)))

(defun tick-reindeer (reindeer state distance)
  (cond ((> (reindeer-state-run-remaining state) 0)
         (progn
           (decf (reindeer-state-run-remaining state))
           (+ distance (reindeer-speed reindeer))))
        ((> (reindeer-state-rest-remaining state) 0)
         (progn
           (decf (reindeer-state-rest-remaining state))
           distance))
        (t
         (progn
           (setf (reindeer-state-run-remaining state)
                 (1-
                  (reindeer-sustain-period reindeer)))
           (setf (reindeer-state-rest-remaining state)
                 (reindeer-rest-period reindeer))
           (+ distance
              (reindeer-speed reindeer))))))

(defun run-race (racers duration-seconds &key (debug nil))
  (loop with states = (map 'vector
                           #'new-state
                           racers)
        with distances = (make-array (length racers))
        for ticks upto duration-seconds
        when debug
          do (terpri)
             (format t "tick: ~s" ticks)
             (print (map 'vector #'cons states distances))
        do (setf distances (map 'vector
                                #'tick-reindeer
                                racers states distances))
        finally (return distances)))

(defun day14-a (filename &key (debug nil))
  (apply #'max
         (coerce (run-race (assemble-racers filename)
                           2503
                           :debug debug)
                 'list)))

(defun run-points-race  (racers duration-seconds &key (debug nil))
  (flet ((all-positions (val collection)
           (loop for thing across collection
                 and position from 0
                 when (equalp thing val)
                   collect position)))
    (loop with states = (map 'vector
                             #'new-state
                             racers)
          with distances = (make-array (length racers))
          with scores = (make-array (length racers))
          for ticks upto duration-seconds
          when debug
            do (terpri)
               (format t "tick: ~s" ticks)
               (print (map 'vector #'list states distances scores))
          do (setf distances (map 'vector
                                  #'tick-reindeer
                                  racers states distances))
             (let ((lead-score (apply #'max (coerce distances 'list))))
               (dolist (leader (all-positions lead-score distances))
                 (incf (aref scores leader))))
          finally (return scores))))
)
(defun day14-b (filename &key (debug nil))
  (apply #'max
         (coerce (run-points-race (assemble-racers filename)
                           2503
                           :debug debug)
                 'list)))
