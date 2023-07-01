(load "~/quicklisp/setup.lisp")
(load "util.lisp")
(load "player.lisp")
(load "vertigo.lisp")

(defun weight (edge) (cl-graph::value edge))
(defun print-with-tab (edge) (format t "   ~s ~d" (cl-graph::value (cl-graph::target-vertex edge)) (weight edge)) (terpri))
(defun print-node-and-children (v)
  (progn
    (terpri)
    (format t "Node: ~s   " (cl-graph::value v))
    (terpri)
    (format t "Neighbors:")
    (terpri)
    (iterate-source-edges v 'print-with-tab)
    (terpri)))
(defun mm/pprint-graph (g)
  (iterate-vertexes g 'print-node-and-children))

(defun main ()
  (mm/pprint-graph (make-map-graph)))

(main)

(getf '(:key1  1
        :key2  128
        :key3  3)
      :key2)


;; (print-colored-text "This is red text!" 1)
;; (print-colored-text "This is green text!" 2)
;; (print-colored-text "This is blue text!" 4)
