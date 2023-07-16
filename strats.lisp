(defun defend-sites ()
  ;; Spread over two bombsites randomly
  (unless (defend-sites/initialized *ai-round-logic*)
    (let ((team (copy-list *enemy-team*)))
      (push (nth 0 team) (defend-sites/b *ai-round-logic*))
      (push (nth 1 team) (defend-sites/stairs *ai-round-logic*))
      (push (nth 2 team) (defend-sites/mid *ai-round-logic*))
      (push (nth 3 team) (defend-sites/short *ai-round-logic*))
      (push (nth 4 team) (defend-sites/a *ai-round-logic*)))
    (setf (defend-sites/initialized *ai-round-logic*) t))
  ;; For each enemy assigned to B, direct them there
  (loop for player in (defend-sites/b *ai-round-logic*) do
    (unless (eq (player-location player) :B)
      (move-player player (first (path-between (player-location player) :B)))))
  ;; For each enemy assigned to Stairs, direct them there
  (loop for player in (defend-sites/stairs *ai-round-logic*) do
    (unless (eq (player-location player) :Stairs)
      (move-player player (first (path-between (player-location player) :Stairs)))))
  ;; For each enemy assigned to Middle, direct them there
  (loop for player in (defend-sites/mid *ai-round-logic*) do
    (unless (eq (player-location player) :Middle)
      (move-player player (first (path-between (player-location player) :Middle)))))
  ;; For each enemy assigned to Short, direct them there
  (loop for player in (defend-sites/short *ai-round-logic*) do
    (unless (eq (player-location player) :Short)
      (move-player player (first (path-between (player-location player) :Short)))))
  ;; For each enemy assigned to A, direct them there
  (loop for player in (defend-sites/a *ai-round-logic*) do
    (unless (eq (player-location player) :A)
      (move-player player (first (path-between (player-location player) :A))))))

(defun rush-mid ()
  
  )

(defun sites-and-toward-t-spawn ()
  (append-line 'log "sites and towards t spawn"))

(defun ct-random ()
  (append-line 'log "ct random"))

(defun defend-mid ()
  (append-line 'log "defending mid"))
