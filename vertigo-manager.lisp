(load "~/quicklisp/setup.lisp")
(load "util.lisp")
(with-silenced-output '(ql:quickload :cl-graph))
(with-silenced-output '(ql:quickload :uiop))
(with-silenced-output '(ql:quickload :alexandria))
(load "display.lisp")
(load "vertigo.lisp")
(load "node-data.lisp")
(load "guns.lisp")
(load "player.lisp")
(load "player-info.lisp")
(load "round.lisp")
(load "size.lisp")
(load "team-selection.lisp")
(load "match-info.lisp")
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
    (remove-player player-name src-node)
    (add-player player-name dst-node)))

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


;; (defpackage cl-tui.examples
;;   (:use :cl :cl-tui))

;; (in-package cl-tui.examples)

;; ;; Colors can be initialized before init-screen
;; ;; `color` creates a color object for later use.
;; (defvar rainbow (list (color 900 500 00)
;;                       (color 1000 1000 0)
;;                       (color 0 1000 0)
;;                       (color 0 1000 1000)
;;                       (color 0 0 1000)
;;                       (color 1000 0 1000)
;;                       ))
;; ;; `color-pair` creates a color pair (foreground-background)
;; (defvar rainbow-pairs (loop :for i :below 6
;;                          :collect (color-pair
;;                                    (elt rainbow (mod i 6))
;;                                    (elt rainbow (mod (1+ i) 6)))))

;; (defparameter color-pairs
;;   '(:red-green
;;     (color-pair
;;      (elt rainbow 1)
;;      (elt rainbow 0))))

;; (print (elt rainbow-pairs 0))
;; (print (eval (getf color-pairs :red-green)))

;; (defun main-render (&key)
;;     ;; `with-attributes` modifies all calls putting characters or
;;     ;; strings to apply the specified attributes
;;     (with-attributes ((:color COLOR-PAIR)) 'callback
;;       ;; This `put-char` call will have the color specified above
;;       (put-char 'callback 1 1 #\X))
;;   ;; (dotimes (i 20)
;;   ;;   (let* ((intensity (* 50 i))
;;   ;;          (fg (gray (+ 50 intensity)))
;;   ;;          (bg (gray intensity)))
;;   ;;     (with-attributes ((:color (color-pair fg bg))) 'callback
;;   ;;       (dotimes (j 3)
;;   ;;         (put-char 'callback (+ 2 j) (1+ i) #\X)))))
;;   )

;; (define-frame callback (simple-frame :render 'main-render) :on :root)

;; (defun start ()
;;   (with-screen (:colors)
;;     (refresh)
;;     (read-key)))

;; (start)

(defun neighbors-of (vertex-val)
  (let ((neighbors nil)
        (vertex (cl-graph:find-vertex *MAP* vertex-val)))
    (cl-graph:iterate-neighbors vertex (lambda (v) (push (cl-graph::value v) neighbors)))
    neighbors))

(defun main ()
  (defparameter *team* (team-selection-stage))
  (defparameter *enemy-team* (get-enemy-team))
  (mapcar (lambda (player) (add-player player :T-Spawn)) *team*)
  (mapcar (lambda (enemy) (add-player enemy :CT-Spawn)) *enemy-team*)
  (move-player :MONESY :TUNNELS)
  (gameplay-stage))

(main)
