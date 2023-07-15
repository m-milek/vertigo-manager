(defun rand-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun nor (a b)
  (not (or a b)))

(defun bullet-advantage (attacker-gun location possible-bullets)
  ;; number of bullets that the attacker can shoot until defender responds
  (let ((flashed (flashed? location))
        (smoked (smoked? location)))
    (ceiling (* possible-bullets
                (cond
                  ((and flashed smoked) 0.1)
                  ((nor flashed smoked) 0.2)
                  ((and (not flashed) smoked) 0.0)
                  ((and flashed (not smoked)) 0.4))))))

(defun shoot-bullet (attacker defender bullet-num)
  (when (or (not (player-alive? defender)) (not (player-alive? attacker)))
    (return-from shoot-bullet))
  
  (let* ((from (player-location attacker))
         (toward (player-location defender))
         (attacker-gun (get-gun (gun (round-status (get-player-info attacker)))))
         (gun-range (* 10 (range attacker-gun)))
         (angle-range (cl-graph::value (cl-graph:find-edge-between-vertexes *MAP* from toward)))
         (range-difference (abs (- angle-range gun-range)))
         (range-accuracy-penalty (+ 1 (if (> 0.4 range-difference) range-difference 0.0)))
         (gun-recoil-penalty (float (* (/ (recoil attacker-gun) 300) bullet-num)))
         (roll (float (/ (rand-in-range 0 100) 100)))
         (roll-after-penalties (float (+ (* roll range-accuracy-penalty) gun-recoil-penalty)))
         (potential-damage (floor (* (damage attacker-gun) (armor-penetration attacker-gun)))))
    (if (< roll-after-penalties (accuracy (get-player-info attacker)))
        (progn (append-line 'log (format nil "~A hit ~A for ~A damage" attacker defender potential-damage))
               (setf (hp (get-round-status defender))
                     (alexandria:clamp (- (hp (get-round-status defender)) potential-damage) 0 100)))
        (append-line 'log (format nil "~A missed ~A."  attacker defender))))
  (when (not (player-alive? defender))
    (append-line 'log (format nil "~A has been killed by ~A." defender attacker))))

(defun attack (attacker defender node-data)
  (let* ((attacker-gun (get-gun (gun (round-status (get-player-info attacker)))))
         (defender-gun (get-gun (gun (round-status (get-player-info defender)))))
         (attacker-bullets-to-shoot
           (ceiling (* (ammo attacker-gun) (/ (rand-in-range 20 45) 100))))
         (defender-bullets-to-shoot
           (ceiling (* (ammo defender-gun) (/ (rand-in-range 20 45) 100))))
         (attacker-bullet-advantage
           (bullet-advantage attacker-gun node-data attacker-bullets-to-shoot))
         (attacker-normal-bullets
           (- attacker-bullets-to-shoot attacker-bullet-advantage))
         (attacker-bullets-shot 0)
         (defender-bullets-shot 0))
    
    (dotimes (bullet-num attacker-bullet-advantage)
      (shoot-bullet attacker defender attacker-bullets-shot)
      (refresh)
      (sleep 0.5)
      (incf attacker-bullets-shot))
    
    (loop while
          (or (not (= attacker-bullets-shot attacker-normal-bullets))
              (not (= defender-bullets-shot defender-bullets-to-shoot)))
          do
             (when (> defender-bullets-to-shoot defender-bullets-shot)
               (shoot-bullet defender attacker defender-bullets-shot)
               (refresh)
               (sleep 0.5)
               (incf defender-bullets-shot)
               )
             (when (> attacker-normal-bullets attacker-bullets-shot)
               (shoot-bullet attacker defender attacker-bullets-shot)
               (refresh)
               (sleep 0.5)
               (incf attacker-bullets-shot)))
    (append-line 'log (format nil "Attacker HP: ~A; Defender HP: ~A"
                              (hp (get-round-status attacker))
                              (hp (get-round-status defender))))))

