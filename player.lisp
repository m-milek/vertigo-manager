(defclass player-round-status ()
    ((hp
      :initform 100
      :accessor hp)
     (gun
      :initform nil
      :accessor gun)
     (utility
      :initform nil
      :accessor utility)))

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
