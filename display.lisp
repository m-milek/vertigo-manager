(load "~/quicklisp/setup.lisp")
(with-silenced-output '(ql:quickload :cl-tui))
(use-package :cl-tui)

;; (defvar *roster* '("Artnight"
;;                    "OSKKIL"
;;                    "leszmak"
;;                    "Jon Barnacle"
;;                    "Mael"))
;; (defvar *enemies* '("iBUYPOWER"
;;                    "Mali09"
;;                    "Dude"
;;                    "gollum fart"
;;                     "Ivan99"))

;; (defun draw-roster (&key frame)
;;   (draw-box frame)
;;   (put-text frame 0 3 "Nisza")
  
;;   (loop for name in *roster*
;;         for row upfrom 1
;;         do (put-text frame row 1 name)))

;; (defun draw-enemies (&key frame)
;;   (draw-box frame)
;;   (put-text frame 0 3 "Enemies")
  
;;   (loop for name in *enemies*
;;         for row upfrom 1
;;         do (put-text frame row 1 name)))

;; (defun draw-map (&key frame)
;;   (draw-box frame)
;;   (put-text frame 0 3 "Map"))

;; (define-frame main (container-frame :split-type :horizontal) :on :root)

;; (define-frame roster
;;     (simple-frame :render #'draw-roster) :on main :w 20)

;; (define-frame chat (container-frame :split-type :vertical) :on main)
;; (define-frame map-frame (container-frame :split-type :vertical) :on chat :h 30)
;; (define-frame map (simple-frame :render #'draw-map) :on map-frame)

;; (define-frame enemies
;;     (simple-frame :render #'draw-enemies) :on main :w 20)

;; (define-frame log (log-frame) :on chat)

;; ;; Edit-frame implements a single-line text editor.
;; ;; It will misbehave if its height is not 1.
;; (define-frame input (edit-frame :prompt "> ") :on chat :h 1)

;;(defvar n 0)

;; (define-children :root ()
;;   (team-selection-frame (simple-frame :render #'render-team-selection-frame)))

;; (defun finish-input ()
;;   ;; Get text from edit-frame
;;   (let ((text (get-text 'input)))
;;     ;; Append it to the log-frame
;;     (append-line 'log text)
;;     (when (< n (length *roster*))
;;       (setf (nth n *roster*) (format nil "~A" n))
;;       (setf (nth n *enemies*) (format nil "~A" n))
;;       (setf n (+ 1 n)))
;;     ;; And clear the text in edit-frame
;;     (clear-text 'input)))

;; (defun team-selection-stage ()
;;   (format t "Hello")
;;   (with-screen ()
;;     (append-line 'log "Enter some text.")
;;     (append-line 'log "Esc to quit")
;;     (loop
;;       (refresh)
;;       (let ((key (read-key)))
;;         (case key
;;           ;; Esc and Newline are handled here
;;           (#\Esc (return))
;;           (#\Newline (finish-input))
;;           (:key-up (cl-tui:scroll-log 'log 1))
;;           (:key-down (cl-tui:scroll-log 'log -1))
;;           ;; Everything else is sent to the edit-frame.
;;           (t (handle-key 'input key)))))))

  ;; (with-screen ()
  ;;   (append-line 'log "Enter some text.")
  ;;   (append-line 'log "Esc to quit")
  ;;   (loop
  ;;     (refresh)
  ;;     (let ((key (read-key)))
  ;;       (case key
  ;;         ;; Esc and Newline are handled here
  ;;         (#\Esc (return))
  ;;         (#\Newline (finish-input))
  ;;         (:key-up (cl-tui:scroll-log 'log 1))
  ;;         (:key-down (cl-tui:scroll-log 'log -1))
  ;;         ;; Everything else is sent to the edit-frame.
  ;;         (t (handle-key 'input key))))))
