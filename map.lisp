(defun get-map-string ()
  (uiop:read-file-lines "map.txt"))

(defparameter *map-node-order*
  '(
    :B
    :CT-Spawn
    :Boost
    :Window
    :Stairs
    :Tunnels
    :Middle
    :Connector
    :S-Corridor
    :Ramp
    :A
    :Ladder
    :Generator
    :T-Spawn
    :Short
    ))
