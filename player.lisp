(defclass player ()
    ((name
      :initarg :name
      :accessor name)
     (hp
      :initform 100
      :accessor hp)
     (gun
      :initarg :gun
      :accessor gun)
     (utility
      :initarg :utility
      :accessor utility)))

(defmethod print-player ((player player))
  (format t "My name is ~A and my HP is ~A. I have ~A and ~A"
          (name player) (hp player) (gun player) (utility player))
  (terpri))

(defun key-val-pairs (hash-table)
  (let (result)
    (maphash
     (lambda (key value) (push (list key value) result))
     hash-table)
    result))

(defun player-location (player-name)
  (loop for (node-name node-data) in (key-val-pairs *node-data-map*) do
    (if (contains (players node-data) player-name)
        (return node-name))))
