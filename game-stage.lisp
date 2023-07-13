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
         (turn-title "TURN"))
    (put-text frame 2 (centered-starting-col score-title width) score-title)
    (put-text frame 3 counter-begin-col counter)
    (put-text frame 5 (centered-starting-col turn-title width) turn-title)
    (put-text frame 6 (centered-starting-col turn-str width) turn-str)))

(defun node-debug-properties-str (vertex)
  (let* ((val (cl-graph::value vertex))
         (data (get-node-data val)))
    (format nil "~A: ~A"
            (string val) ;; Name of the node
            (players data)))) ;; List of players

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
(define-frame log (log-frame) :on map-container :h 5)
(define-frame input (edit-frame :prompt "> ") :on map-container :h 1)

(defun other-side (side)
  (case side
    (:CT :T)
    (:T :CT)))

(defparameter *current-side* :T)

(defun switch-side ()
  (setf *current-side* (other-side *current-side*)))

(defun enemy-turn ()
  (append-line 'log "ENEMY TURN" 1))

(defclass game-round ()
  ((turns-left :initform 20 :accessor turns-left)))

(defparameter *current-game-round* (make-instance 'game-round))

(defun reset-game-round ()
  (setf *current-game-round* (make-instance 'game-round)))

(defclass turn ()
  ((used-players :initform nil :accessor used-players)))

(defparameter *current-turn* (make-instance 'turn))

(defun cmd/move (who where)
  ;; Correct player name
  
  ;; Correct location name
  
  ;; Player can actually move to that location
  (when (not (contains (neighbors-of (player-location who)) where))
    (append-line 'log (format nil "ERROR: Player ~A is not in a location adjacent to ~A" (string who) (string where)))
    (return-from cmd/move nil))
  (append-line 'log (format nil "Moving ~A to ~A" (string who) (string where)))
  (move-player who where)
  t
  )

(defun cmd/attack (args)
  (format t "ATTACK")
  (print args))

(defun cmd/flash (args)
  (format t "FLASH")
  (print args))

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

(defun process-gameplay-cmd (cmd)
  (let* ((split-cmd (uiop:split-string cmd :separator " "))
         (who (make-keyword (string-upcase (first split-cmd))))
         (action (string-downcase (second split-cmd)))
         (where (make-keyword (string-upcase (third split-cmd)))))
    ;; all variables defined

    ;; check if that player exists
    (when (not (contains (hash-table-keys *players*) who))
      (append-line 'log (format nil "ERROR: Player ~A is not in your team." (string who)))
      (return-from process-gameplay-cmd nil))

    ;; Check if that player can be moved
    (when (contains (used-players *current-turn*) who)
      (append-line 'log (format nil "ERROR: Player ~A has already used up their move in this turn." who))
      (return-from process-gameplay-cmd nil))

    ;; Check if that location exists
    (when (not (contains vertigo-nodes where))
      (append-line 'log (format nil "ERROR: Location ~A not found on Vertigo." (string where)))
      (return-from cmd/move nil))

    ;; Everything went well, action can be performed
    ;; mark the player as used this turn
    (append-line 'log "Player ~A marked as used" who)
    
    (let ((cmd-result (alexandria:switch (action :test #'equal)
                   ("move" (cmd/move who where))
                   ;; ("attack" 'cmd/attack)
                   ;; ("defend" 'cmd/defend)
                   ;; ("flash" 'cmd/flash)
                   ;; ("smoke" 'cmd/smoke)
                   ;; ("molly" 'cmd/molly)
                       ;; ("nade" 'cmd/nade)))
                       )))
      (unless (eq nil cmd-result)
        (mark-used who))
      )
    )
  )

(defun finish-gameplay-input ()
  (let ((text (get-text 'input)))
    (clear-text 'input)
    (process-gameplay-cmd text)))



(defun gameplay-stage ()
  (with-screen ()
    (display 'gameplay-container)
    (append-line 'log "This is the game log.")
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
          ))))
