(defclass node-data ()
  ;; Describes what's going on at a location
  ((players :initform nil :accessor players)
   (smoked? :initform nil :accessor smoked?)
   (flashed? :initform nil :accessor flashed?)
   (bombsite? :initarg :bombsite? :initform nil :accessor bombsite?)))

(defparameter *node-data-map*
  ;; Hash table mapping location names to node data objects
  (let ((map (make-hash-table)))
    (loop for place in vertigo-nodes do
      (setf (gethash place map) (make-instance 'node-data)))
    ;; Mark bombsites as bombsites
    (setf (bombsite? (gethash :A map)) t)
    (setf (bombsite? (gethash :B map)) t)
    map))

(defun node-data-to-string (data)
  (format nil "Players: ~A~%Smoked?:~A Flashed?:~A~%Bombsite?: ~A"
          (players data) (smoked? data) (flashed? data) (bombsite? data)))

(defun print-node-data (data)
  (format t "~A" (node-data-to-string data))
  (terpri))

(defun print-node-data-map ()
  (maphash (lambda (node data)
             (progn
               (terpri)
               (format t "~A" node)
               (terpri)
               (format t "~A" (node-data-to-string data))
               (terpri)))
           *node-data-map*))

;;(print-node-data-map)

(defun get-node-data (key)
  (gethash key *node-data-map*))

