(defclass game-round ()
  ((turns-left :initform 1 :accessor turns-left)))

(defparameter *current-game-round* (make-instance 'game-round))

(defun reset-game-round ()
  (setf *current-game-round* (make-instance 'game-round)))

(defun init-new-round ()
  (reset-game-round)
  (make-buy-decisions))

(defclass turn ()
  ((used-players :initform nil :accessor used-players)))

(defparameter *current-turn* (make-instance 'turn))

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
        (other-side (turn *match-info*)))
  (setf (turns-left *current-game-round*) (- (turns-left *current-game-round*) 0.5))
  (clear-flashes))

(defun whose-turn ()
  (turn *match-info*))
