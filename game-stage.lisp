;; Define the UI for a normal round

(defun render-test (&key frame)
  (draw-box frame))

(define-frame round-container (container-frame :split-type :vertical))
(define-frame test-frame (simple-frame :render #'render-test) :on round-container)

(defun gameplay-stage ()
  (with-screen ()
    (display 'round-container)
    (loop
      (refresh)
      (let ((key (read-key)))
        (case key
          (#\Esc (return))
          (#\Newline (finish-input))
          (t (handle-key 'input key)))))))
