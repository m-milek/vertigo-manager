(define-frame buy-stage-container
    (container-frame :split-type :vertical))
(define-frame player-funds-container
    (container-frame :split-type :horizontal) :on buy-stage-container)
(define-frame buy-stage-input-container
    (container-frame :split-type :vertical) :on buy-stage-container)

(define-frame buy-stage-log (log-frame) :on buy-stage-input-container :h 5)
(define-frame buy-stage-input (edit-frame :prompt ">") :on buy-stage-container :h 1)

(defun render-player-funds (&key frame)
  (draw-box frame))

(define-frame player-funds-frame
    (simple-frame :render #'render-player-funds)
    :on player-funds-container)

(defun finish-buy-stage-input ()
  (let ((text (get-text 'input)))
    (clear-text 'input)))

(defun buy-stage ()
  (with-screen ()
    (display 'buy-stage-container)
    (loop
      (refresh)
      (let ((key (read-key)))
        (case key
          (#\Esc (return))
          (#\Newline (finish-buy-stage-input))
          (t (handle-key 'input key)))))))
