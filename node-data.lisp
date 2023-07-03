(defclass node-data ()
  ;; Describes what's going on at a location
  ((players :initform nil :accessor players)
   (smoked? :initform nil :accessor smoked?)
   (flashed? :initform nil :accessor flashed?)
   (bombsite? :initarg :bombsite? :initform nil :accessor bombsite?)))

(defparameter *node-data-map*
  (let ((nodes
          (flatten
           (mapcar
            (lambda (node) (list node (make-instance 'node-data)))
            vertigo-nodes))))
    (setf (bombsite? (getf nodes :A)) t)
    (setf (bombsite? (getf nodes :B)) t)
    nodes))

(defun node-data-to-string (data)
  (format nil "Players: ~A~%Smoked?:~A Flashed?:~A~%Bombsite?: ~A"
          (players data) (smoked? data) (flashed? data) (bombsite? data)))

(defun print-node-data (data)
  (format t "~A" (node-data-to-string data))
  (terpri))

(defun print-node-data-map ()
  (loop for (node data) in (pair-list *node-data-map*) do
    (progn
      (terpri)
      (format t "~A" node)
      (terpri)
      (format t "~A" (node-data-to-string data))
      (terpri))))

(defun get-node-data (key)
  (getf *node-data-map* key))
