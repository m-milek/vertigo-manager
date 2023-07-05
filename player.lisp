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

(defmethod location ((player player))
  ;; Using *node-data-map* see where a player is
  ()
  )
