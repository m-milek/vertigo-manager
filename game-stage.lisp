;; Define the UI for a normal round

(defun get-score-counter ()
  ;; Place the player's team score on the left, enemy score on the right
  (format nil "~A : ~A" (ct-score) (t-score)))

(defun render-player-list (&key frame)
  (draw-box frame)
  (put-text frame 0 3 "PLAYERS")
  (let ((line-num 1))
    (loop for name in *team* do
      (put-text frame line-num 1 (string name))
      (incf line-num))))

(defun render-match-info (&key frame)
  (draw-box frame)
  (put-text frame 0 3 "MATCH-INFO")
  (let* ((width 25)
         (counter (get-score-counter))
         (counter-begin-col (centered-starting-col counter width))
         (turn-str (string (whose-turn)))
         (score-title "SCORE")
         (turn-title "TURN")
         (turns-left-title "TURNS LEFT IN ROUND")
         (turns-left (write-to-string (ceiling (turns-left *current-game-round*)))))
    (put-text frame 2 (centered-starting-col score-title width) score-title)
    (put-text frame 3 counter-begin-col counter)
    (put-text frame 5 (centered-starting-col turn-title width) turn-title)
    (put-text frame 6 (centered-starting-col turn-str width) turn-str)
    (put-text frame 8 (centered-starting-col turns-left-title width) turns-left-title)
    (put-text frame 9 (centered-starting-col turns-left width) turns-left)))

(defun node-debug-properties-str (vertex)
  (let* ((val (cl-graph::value vertex))
         (data (get-node-data val)))
    (format nil "~A: ~A, F?: ~A"
            (string val) ;; Name of the node
            (players data)
            (flashed? data)))) ;; List of players

(defun render-map-frame (&key frame)
  (draw-box frame)
  (put-text frame 0 3 "MAP")
  (let ((line-num 1))
    (cl-graph:iterate-vertexes
     *MAP*
     (lambda (vertex)
       (let ((about-node (node-debug-properties-str vertex)))
         (put-text frame line-num 1 about-node)
         (incf line-num))))))

(define-frame gameplay-container (container-frame :split-type :horizontal))

;; left panel
(define-frame left-panel-container (container-frame :split-type :vertical) :on gameplay-container :w 25)
(define-frame right-panel-container (container-frame :split-type :vertical) :on gameplay-container)
;; upper left
(define-frame left-upper-container (container-frame :split-type :vertical) :on left-panel-container)
(define-frame match-info-frame (simple-frame :render #'render-match-info) :on left-upper-container)
;; lower left
(define-frame left-lower-container (container-frame :split-type :vertical) :on left-panel-container)
(define-frame player-list (simple-frame :render #'render-player-list) :on left-lower-container)

;; right panel
(define-frame map-container (container-frame :split-type :vertical) :on right-panel-container)
(define-frame map-frame (simple-frame :render #'render-map-frame) :on map-container)
(define-frame log (log-frame) :on map-container :h 20)
(define-frame input (edit-frame :prompt "> ") :on map-container :h 1)

(defun other-side (side)
  (case side
    (:CT :T)
    (:T :CT)))

(defparameter *current-side* :T)

(defun switch-side ()
  (setf *current-side* (other-side *current-side*)))

(defun cmd/move (who where)
  ;; Check if there is an enemy on that location. if there is, cancel.
  (when (enemy-present-on where)
    (append-line 'log (format nil "There is one or more enemies present at ~A. Peek them first!" where))
    (return-from cmd/move))
  (append-line 'log (format nil "Moving ~A to ~A" (string who) (string where)))
  (move-player who where)
  t)

(defun cmd/attack (who where)
  (when (not (enemy-present-on where))
    (append-line 'log (format nil "There is no enemy to attack on ~A. Just push!" where))
    (return-from cmd/attack))
  ;; Pick an enemy to attack on that location
  (let* ((node-data (get-node-data where))
         (enemies (players node-data))
         (defender (nth (random (length enemies)) enemies)))
    (append-line 'log (format nil "~A is attacking ~A" who defender))
    (attack who defender (get-node-data where))
    )
  )

(defun cmd/flash (who where)
  ;; Roll to see if the flash works
  (append-line 'log "Player ~A flashed ~A" who where)
  (setf (flashed? (get-node-data where)) t))

(defun cmd/smoke (args)
  (format t "SMOKE")
  (print args))

(defun cmd/molly (args)
  (format t "MOLLY")
  (print args))

(defun cmd/nade (args)
  (format t "NADE")
  (print args))

(defun cmd/defend (args)
  (format t "DEFEND")
  (print args))

(defun cmd/unknown (args)
  (append-line 'log "Unknown command: ~A" (car args)))

(defun mark-used (who)
  ;; Mark that this player has used up their move in current turn
  (push who (used-players *current-turn*)))

(defun clear-used ()
  (setf (used-players *current-turn*) nil))

(defun enemy-present-on (location)
  (some
   (lambda (enemy)
     (and (player-alive? enemy)
          (contains (players (get-node-data location)) enemy)))
   *enemy-team*))

(defun process-gameplay-cmd (cmd)
  (let* ((split-cmd (uiop:split-string cmd :separator " "))
         (who (make-keyword (string-upcase (first split-cmd))))
         (action (string-downcase (second split-cmd)))
         (where (make-keyword (string-upcase (third split-cmd)))))
    ;; all variables defined
    
    ;; Check if that player exists
    (when (not (contains (hash-table-keys *players*) who))
      (append-line 'log (format nil "ERROR: Player ~A is not in your team." (string who)))
      (return-from process-gameplay-cmd))

    ;; Check if the player is alive
    (when (< (hp (get-round-status who)) 0)
      (append-line 'log (format nil "ERROR: Player ~A is dead." (string who)))
      (return-from process-gameplay-cmd))

    ;; Check if that player can be moved
    (when (contains (used-players *current-turn*) who)
      (append-line 'log (format nil "ERROR: Player ~A has already used up their move in this turn." who))
      (return-from process-gameplay-cmd))

    ;; Check if that location exists
    (when (not (contains vertigo-nodes where))
      (append-line 'log (format nil "ERROR: Location ~A not found on Vertigo." (string where)))
      (return-from process-gameplay-cmd))

    ;; Location is valid (adjacent to (player-location who))
    (when (not (contains (neighbors-of (player-location who)) where))
      (append-line 'log (format nil "ERROR: Player ~A is not in a location adjacent to ~A" (string who) (string where)))
      (return-from process-gameplay-cmd))

    ;; Action can be performed
    (let ((cmd-result
            (alexandria:switch (action :test #'equal)
              ("move" (cmd/move who where))
              ("attack" (cmd/attack who where))
              ("flash" (cmd/flash who where))
              ;; ("defend" 'cmd/defend)
              ;; ("smoke" 'cmd/smoke)
              ;; ("molly" 'cmd/molly)
              ;; ("nade" 'cmd/nade)))
              )))
      (unless (eq nil cmd-result)
        ;; mark the player as used this turn
        (mark-used who)))))

(defun finish-gameplay-input ()
  (let ((text (get-text 'input)))
    (clear-text 'input)
    (process-gameplay-cmd text))
  ;; All players have been used
  (when (eq (length *team*) (length (used-players *current-turn*)))
    (append-line 'log "All players have been used. Ending turn...")
    (clear-used)
    (toggle-turn)))

(defun make-buy-decisions ()
  ;; Check funds and buy guns
  ;; for now, give everyone an AK
  (mapcar (lambda (name)
            (setf (gun (round-status (get-player-info name))) :AK-47))
          *team*)
  (mapcar (lambda (name)
            (setf (gun (round-status (get-player-info name))) :AK-47))
          *enemy-team*))

(defun gameplay-stage ()
  (with-screen ()
    (display 'gameplay-container)
    (append-line 'log "This is the game log.")
    (init-new-round)
    (loop
      (refresh)
      (if (eq (whose-turn) *current-side*)
          (let ((key (read-key)))
            (case key
              (#\Esc (return))
              (#\Newline (finish-gameplay-input))
              (t (handle-key 'input key))))
          (progn
            (sleep 1)
            (enemy-turn)
            (toggle-turn))
          )
      (when (zerop (turns-left *current-game-round*))
        (init-new-round))
      )))
