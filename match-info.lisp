(defclass match-info ()
  ((scores :initform '(:T 0 :CT 0) :accessor scores)
   (turn :initform ':T :accessor turn)))

(defparameter *match-info*
  (make-instance 'match-info))

(defun ct-score ()
  (getf (scores *match-info*) :CT))

(defun add-ct-score ()
  (setf (getf (scores *match-info*) :CT)
        (+ 1 (getf (scores *match-info*) :CT))))

(defun add-t-score ()
  (setf (getf (scores *match-info*) :T)
        (+ 1 (getf (scores *match-info*) :T))))

(defun t-score ()
  (getf (scores *match-info*) :T))

(defun round-count ()
  (+ (getf (scores *match-info*) :CT)
     (getf (scores *match-info*) :T)))

(defun toggle-turn ()
  (setf (turn *match-info*)
        (other-side (turn *match-info*))))

(defun whose-turn ()
  (turn *match-info*))

