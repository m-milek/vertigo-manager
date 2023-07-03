(defclass gun ()
  ((name
    :initarg :name
    :accessor name)
   (damage ;; damage with no armor on enemy
    :initarg :damage
    :accessor damage)
   (ammo ;; ammo in a magazine
    :initarg :ammo
    :accessor ammo)
   (recoil ;; Difficulty of controlling recoil, 0-10
    :initarg :recoil
    :accessor recoil)
   (range ;; 1 = long range, 0 = close range, 0-1
    :initarg :range
    :accessor range)
   (armor-penetration ;; Percentage of damage going through armor
    :initarg :armor-penetration
    :accessor armor-penetration)))

(defmethod print-gun ((gun gun))
  (format t "Gun name: ~A" (name gun))
  (terpri))

(defparameter guns
  '(:AK-47 (make-instance 'gun :name "AK-47" :damage 36 :ammo 30 :recoil 7 :range 0.8 :armor-penetration 0.77)
    :M4A1-S (make-instance 'gun :name "M4A1-S" :damage 38 :ammo 20 :recoil 4 :range 0.7 :armor-penetration 0.7)
    :AUG (make-instance 'gun :name "AUG" :damage 28 :ammo 30 :recoil 6 :range 0.8 :armor-penetration 0.9)
    :Galil-AR (make-instance 'gun :name "Galil AR" :damage 30 :ammo 30 :recoil 6 :range 0.7 :armor-penetration 0.77)
    :AWP (make-instance 'gun :name "AWP" :damage 115 :ammo 5 :recoil 0 :range 1.0 :armor-penetration 0.97)
    :SSG-08 (make-instance 'gun :name "SSG-08" :damage 88 :ammo 10 :recoil 0 :range 1.0 :armor-penetration 0.85)
    :USP-S (make-instance 'gun :name "USP-S" :damage 35 :ammo 12 :recoil 3 :range 0.6 :armor-penetration 0.5)
    :Glock-18 (make-instance 'gun :name "Glock-18" :damage 30 :ammo 20 :recoil 4 :range 0.4 :armor-penetration 0.45)
    :Desert-Eagle (make-instance 'gun :name "Desert Eagle" :damage 53 :ammo 7 :recoil 8  :range 0.8 :armor-penetration 0.93)
    :Tec-9 (make-instance 'gun :name "Tec-9" :damage 33 :ammo 18 :recoil 5 :range 0.6 :armor-penetration 0.9)
    :Dual-Berettas (make-instance 'gun :name "Dual Berettas" :damage 30 :ammo 30 :recoil 6 :range 0.4 :armor-penetration 0.57)
    :XM1014 (make-instance 'gun :name "XM1014" :damage 30 :ammo 7 :recoil 4 :range 0.3 :armor-penetration 0.8)
    :Mag-7 (make-instance 'gun :name "Mag-7" :damage 30 :ammo 5 :recoil 2 :range 0.2 :armor-penetration 0.75)
    :Nova (make-instance 'gun :name "Nova" :damage 30 :ammo 8 :recoil 2 :range 0.3 :armor-penetration 0.5)
    :MP9 (make-instance 'gun :name "MP9" :damage 26 :ammo 30 :recoil 6 :range 0.5 :armor-penetration 0.6)
    :MAC-10 (make-instance 'gun :name "MAC-10" :damage 29 :ammo 30 :recoil 7 :range 0.4 :armor-penetration 0.57)
    :MP5-SD (make-instance 'gun :name "MP5-SD" :damage 27 :ammo 30 :recoil 5 :range 0.4 :armor-penetration 0.62)))

(defun get-gun (symbol) (eval (getf guns symbol)))
