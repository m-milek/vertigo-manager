(load "~/quicklisp/setup.lisp")
(load "util.lisp")
(with-silenced-output '(ql:quickload :cl-graph))
(load "display.lisp")
(load "vertigo.lisp")
(load "node-data.lisp")
(load "guns.lisp")
(load "player.lisp")
(load "player-info.lisp")
(load "round.lisp")
(load "size.lisp")
(load "team-selection.lisp")
(load "game-stage.lisp")

(defun weight (edge) (cl-graph::value edge))

(defun print-with-tab (edge)
  (format t "   ~s ~d"
          (cl-graph::value (cl-graph::target-vertex edge))
          (weight edge))
  (terpri))

(defun print-node-and-children (v)
  (progn
    (format t "Node: ~s" (cl-graph::value v))
    (terpri)
    (print-node-data (get-node-data (cl-graph::value v)))
    (format t "Neighbors:")
    (terpri)
    (cl-graph:iterate-source-edges v 'print-with-tab)
    (terpri)))

(defun print-map (g)
  (cl-graph:iterate-vertexes g 'print-node-and-children))

(defun move-player (player-name dst-node)
  (let ((src-node (player-location player-name)))
    (add-player player-name dst-node)
    (remove-player player-name src-node)))

(defun remove-player (player-name node-name)
  (setf (players (gethash node-name *node-data-map*))
        (delete player-name
                (players (gethash node-name *node-data-map*)))))

(defun add-player (player-name node-name)
  (push player-name (players (gethash node-name *node-data-map*))))
  
  ;; (let ((team (get-player-team))
  ;;       (enemy-team (get-enemy-team))
  ;;       (map (make-map-graph)))
  ;;   (mapcar (lambda (player) (add-player player :T-Spawn)) team)
  ;;   (mapcar (lambda (enemy) (add-player enemy :CT-Spawn)) enemy-team)
  ;;   (print-player-infos)
  ;;   (print-map map)
  ;;   (print team)
  ;;   (print enemy-team)))
;;  (format t "Hello")
;;  (team-selection-stage)

;; (defun render-scene (&key frame)
;;   (draw-box frame)
;;   (put-text frame 0 4 "~A" frame)
;;   (put-text frame 1 1 "Push 1, 2, 3, 4 to switch between frames ")
;;   (put-text frame 2 1 "Space to show all frames tiled")
;;   (put-text frame 3 1 "q to quit")
;;   (when (eq frame 'scene-4)
;;     (put-text frame 4 1 "This frame is not a part of :root frame")))
;; Three frames are on top of :root. By default if the :root frame is
;; displayed they'll be positioned horizontally.

;; define-children defines several frames placing them on the same other frame in order
;; Ensures that frames order won't be messed up if you redefine them individually
;; (define-children :root ()
;;   (team-selection-scene (simple-frame :render #'render-team-selection-scene)))
  ;; (scene-2 (simple-frame :render #'render-scene))
  ;; (scene-3 (simple-frame :render #'render-scene)))
;; Fourth frame is not positioned on any frame. It can only be displayed directly with a `display` call.
;;(define-frame scene-4 (simple-frame :render #'render-scene))

;; (defun start ()
;;   (team-selection-stage))
  ;; (with-screen ()
  ;;   ;; `display` displays a frame and its children on the whole screen.
  ;;   ;; Before the first `display` call the :root frame is displayed
  ;;   (display 'team-selection-scene)
  ;;   (loop (case (read-key)
  ;;           (#\q (return))
  ;;           ;; (#\2 (display 'scene-2))
  ;;           ;; (#\3 (display 'scene-3))
  ;;           ;; (#\4 (display 'scene-4))
  ;;           ))))



(defun main ()
  (let ((team (team-selection-stage))
        (enemy-team (get-enemy-team)))
    (print team)
    (print enemy-team)
    (gameplay-stage)
    )
  )


(main)
