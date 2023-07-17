(defparameter *possible-teammates*
  (hash-table-keys *players*))

(defparameter *team*
  '())

(defun render-possible-teammates-frame (&key frame)
  (let ((line-num 1)
        (width (first (size))))
    (loop for name in *possible-teammates* do
      (put-text frame
                line-num
                (- (/ width 2) (floor (/ (length (string name)) 2)))
                (string name))
      (setf line-num (+ 1 line-num))))
  (draw-box frame))

(defun render-title-frame (&key frame)
  (let* ((sizes (size))
         (title "Team Selection")
         (center (/ (first sizes) 2))
         (title-start (- center (/ (length title) 2))))
    (put-text frame 1 title-start title)))

(defun render-team-selection-hint-frame (&key frame)
  (put-text frame 0 0 "Select your squad. Type a name and press Enter to pick them.")
  (put-text frame 1 0 "To undo a pick, press LEFT.")
  (put-text frame 2 0 "To finish, press RIGHT")
  (put-text frame 3 0 (format nil "Current selection (~A/5):~%" (length *team*)))
  (let ((line-num 5))
    (loop for name in *team* do
      (put-text frame line-num 0 (string name))
      (setf line-num (+ 1 line-num)))))

;;(define-frame log (log-frame) :on input-container)
(define-frame team-selection-container (container-frame :split-type :vertical))

(define-frame title-frame (simple-frame :render 'render-title-frame) :on team-selection-container :h 3)

(define-frame possible-teammates-frame
    (simple-frame :render 'render-possible-teammates-frame) :on team-selection-container :h (- (second (size)) 3 15 1 1))

(define-frame input-container (container-frame :split-type :vertical) :on team-selection-container)

(define-frame input (edit-frame :prompt "Player Name: ") :on input-container :h 1)

(define-frame team-selection-hint-frame
    (simple-frame :render 'render-team-selection-hint-frame)
  :on team-selection-container :h 15)

(defun finish-input ()
  (let* ((text (string-upcase (get-text 'input)))
         (keyword-from-input (intern text "KEYWORD")))
    (when (and (< (length *team*) 5)
               (contains *possible-teammates* keyword-from-input))
      (push keyword-from-input *team*)
      (setf *possible-teammates* (remove keyword-from-input *possible-teammates*))))
  (clear-text 'input))

(defun team-selection-undo ()
  (let ((to-remove (first *team*)))
    (when to-remove
      (push (first *team*) *possible-teammates*)
      (setf *team*
            (remove to-remove *team*)))))

(defun team-selection-stage ()
  (with-screen ()
    (display 'team-selection-container)
    (loop
      (refresh)
      (let ((key (read-key)))
        (case key
          (#\Esc (return))
          (#\Newline (finish-input))
          (:key-left (team-selection-undo))
          (:key-right (when (= (length *team*) 5)
                        (return-from team-selection-stage *team*)))
          (t (handle-key 'input key)))))))

