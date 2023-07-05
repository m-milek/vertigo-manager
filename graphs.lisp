(load "~/quicklisp/setup.lisp")
(load "util.lisp")
(load "player.lisp")
(load "player-info.lisp")
(load "vertigo.lisp")
(load "node-data.lisp")
(load "guns.lisp")

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
    (iterate-source-edges v 'print-with-tab)
    (terpri)))

(defun print-map (g)
  (iterate-vertexes g 'print-node-and-children))

(defun remove-element (element list)
  (if (null list) 0
      (if (= element (car list))
          (delete (car list))
          (remove-it (cdr list)))))

(defun move-player (player dst-node))

(defun spawn-player (player-name node-name)
  (push player-name (players (gethash node-name *node-data-map*))))

(defun run ()
  (let ((team '(:Mael :JonBarnacle :Kataszynka :leszmak :OSKKIL))
        (map (make-map-graph)))
    (mapcar (lambda (player) (spawn-player player :T-Spawn)) team)
    (print-map map)
    ;;(print-player-infos)
    ))

(run)


