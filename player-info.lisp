(defun player-info-to-string (info)
  (format nil "Accuracy: ~A ~%Eco: (T: ~A, CT: ~A)~%Half: (T: ~A, CT: ~A)~%Full: (T: ~A, CT: ~A)~%"
          (accuracy info)
          (getf (eco-buy info) :T) (getf (eco-buy info) :CT)
          (getf (half-buy info) :T) (getf (half-buy info) :CT)
          (getf (full-buy info) :T) (getf (full-buy info) :CT)))

(defclass player-info ()
  ((accuracy :initarg :accuracy :accessor accuracy)
   (eco-buy :initarg :eco-buy :accessor eco-buy)
   (half-buy :initarg :half-buy :accessor half-buy)
   (full-buy :initarg :full-buy :accessor full-buy)))

(defparameter *players*
  ;; Hashtable containing player names and information about them
  (let ((table (make-hash-table))
        (list '(:Ainsleyy    (make-instance 'player-info
                              :accuracy 0.44
                              :eco-buy '(:T :Tec-9 :CT :USP-S)
                              :half-buy '(:T :XM1014 :CT :XM1014)
                              :full-buy '(:T :AK-47 :CT :AWP))
                :Artnight    (make-instance 'player-info
                              :accuracy 0.42
                              :eco-buy '(:T :Desert-Eagle :CT :Desert-Eagle)
                              :half-buy '(:T :MP5-SD :CT :Nova)
                              :full-buy '(:T :AK-47 :CT :AUG))
                :Gauss       (make-instance 'player-info
                              :accuracy 0.28
                              :eco-buy '(:T :Glock-18 :CT :P2000)
                              :half-buy '(:T :Galil-AR :CT :MP9)
                              :full-buy '(:T :AK-47 :CT :M4A1-S))
                :JonBarnacle (make-instance 'player-info
                              :accuracy 0.39
                              :eco-buy '(:T :Tec-9 :CT :Dual-Berettas)
                              :half-buy '(:T :Galil-AR :CT :FAMAS)
                              :full-buy '(:T :AK-47 :CT :M4A1-S))
                :Julieci     (make-instance 'player-info
                              :accuracy 0.32
                              :eco-buy '(:T :Glock-18 :CT :USP-S)
                              :half-buy '(:T :MP5-SD :CT :MP5-SD)
                              :full-buy '(:T :AK-47 :CT :M4A1-S))
                :Kataszynka  (make-instance 'player-info
                              :accuracy 0.3
                              :eco-buy '(:T :Glock-18 :CT :USP-S)
                              :half-buy '(:T :Galil-AR :CT :FAMAS)
                              :full-buy '(:T :AK-47 :CT :M4A1-S))
                :Mael        (make-instance 'player-info
                              :accuracy 0.38
                              :eco-buy '(:T :Tec-9 :CT :Dual-Berettas)
                              :half-buy '(:T :Desert-Eagle :CT :Mag-7)
                              :full-buy '(:T :AK-47 :CT :M4A1-S))
                :OSKKIL      (make-instance 'player-info
                              :accuracy 0.61
                              :eco-buy '(:T :Desert-Eagle :CT :Desert-Eagle)
                              :half-buy '(:T :SSG-08 :CT :SSG-08)
                              :full-buy '(:T :AWP :CT :AWP))
                :leszmak     (make-instance 'player-info
                              :accuracy 0.32
                              :eco-buy '(:T :Glock-18 :CT :USP-S)
                              :half-buy '(:T :MAC-10 :CT :MP9)
                              :full-buy '(:T :AK-47 :CT :M4A1-S)))))
    (loop for (key value) in (pair-list list) do
      (setf (gethash key table) (eval value)))
    table))

(defun print-player-infos ()
  (maphash (lambda (name info)
             (progn
               (format t "~%Name: ~A~%~A" name (player-info-to-string info))
               ))
           *players*))

(defun get-player-info (symbol) (getf *players* symbol))
