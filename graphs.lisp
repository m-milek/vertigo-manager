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

(defun run ()
  (print-map (make-map-graph)))

(run)


