(defparameter vertigo-nodes
  ;; Nodes and node-data describing locations on the map
  '(:T-Spawn
    :S-Corridor
    :Tunnels
    :Generator
    :Middle
    :Ladder
    :Boost
    :Connector
    :Ramp
    :Stairs
    :B
    :A
    :Window
    :Short
    :CT-Spawn))

(defparameter vertigo-edges
  ;; List describing edges on the map graph.
  '((:T-Spawn ((:Stairs 6)
               (:Tunnels 5)
               (:Generator 4)))
    
    (:S-Corridor ((:Tunnels 3)
                  (:Stairs 3)))
    
    (:Tunnels ((:Ramp 8)
               (:Ladder 2)
               (:T-Spawn 5)))
    
    (:Generator ((:Middle 4)
                 (:Ladder 3)
                 (:T-Spawn 6)))
    
    (:Middle ((:Connector 6)
              (:Boost 3)
              (:CT-Spawn 4)
              (:Generator 6)))
    
    (:Ladder ((:Tunnels 2)
              (:Generator 3)))
    
    (:Boost ((:Middle 6)
             (:B 8)
             (:CT-Spawn 4)))

    (:Connector ((:Middle 8)
                 (:A 7)
                 (:Window 2)
                 (:CT-Spawn 6)))

    (:Ramp ((:Short 1)
            (:Tunnels 4)
            (:A 7)))
    
    (:Stairs ((:B 4)
              (:S-Corridor 1)
              (:T-Spawn 6)))

    (:B ((:Stairs 4)
         (:Boost 8)
         (:CT-Spawn 9)))
    
    (:A ((:Short 7)
         (:Connector 7)
         (:Window 3)))

    (:Window ((:A 9)
              (:CT-Spawn 4)
              (:Connector 2)))

    (:Short ((:A 5)
             (:Ramp 9)))
    
    (:CT-Spawn ((:Window 3)
                (:Connector 5)
                (:Boost 3)
                (:B 9)))))

(defun make-map-graph ()
  :export t
  (mm/add-edges (mm/make-graph-from-nodes vertigo-nodes) vertigo-edges))

(defun mm/make-graph-from-nodes (list)
  ;; Add all nodes, no edges yet
  (let ((g (cl-graph::make-container 'cl-graph:graph-container)))
    (loop
      for node in list do
        (cl-graph:add-vertex g node))
    g))

(defun mm/add-edges (graph edge-list)
  ;; Add edges between nodes
  (loop for (src dst-list) in edge-list do
      (loop for (dst weight) in dst-list do
        (cl-graph:add-edge-between-vertexes
         graph src dst
         :edge-type :directed
         :value weight)))
  graph)

(defparameter *MAP* (make-map-graph))
