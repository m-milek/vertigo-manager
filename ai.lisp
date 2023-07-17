(defclass ai-round-logic ()
  ((strat :initform nil :accessor strat)
   (defend-sites/initiated :initform nil :accessor defend-sites/initialized)
   (defend-sites/b :initform nil :accessor defend-sites/b)
   (defend-sites/stairs :initform nil :accessor defend-sites/stairs)
   (defend-sites/mid :initform nil :accessor defend-sites/mid)
   (defend-sites/short :initform nil :accessor defend-sites/short)
   (defend-sites/a :initform nil :accessor defend-sites/a)
   (rush-mid/initialized :initform nil :accessor rush-mid/initialized))) ;; unused

(defun ai/reset-round-logic ()
  (defparameter *ai-round-logic* (make-instance 'ai-round-logic)))

(defun ai/seed-round-logic ()
  (ai/reset-round-logic)
  (if (eq (other-side *current-side*) :CT)
      (ai/seed-ct-round-logic)
      (ai/seed-t-round-logic)))

(defun ai/seed-ct-round-logic ()
  ;; Pick a strat and set it
  (let* ((strats
           '(defend-sites))
             ;;rush-mid ct-random sites-and-toward-t-spawn defend-mid))
        (picked-strat (nth (random (length strats)) strats)))
    (setf (strat *ai-round-logic*) picked-strat)))

(defun ai/ct-begin-defuse ())

(defun ai/seed-t-round-logic ()
  ;; Pick a strat and set it
  (let* ((strats '(push-a push-b push-mid t-random))
        (picked-strat (nth (random (length strats)) strats)))
    (setf (strat *ai-round-logic*) picked-strat)))

(defun enemy-turn ()
  (funcall (strat *ai-round-logic*)))

(defun path-between (a b)
  (graph-algorithms:reconstruct-path
   (graph-algorithms:shortest-paths a vertigo-nodes #'neighbors-of)
   b))
