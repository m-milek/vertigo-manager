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
               (:Tunnels 2)
               (:Generator 5)))
    
    (:S-Corridor ((:Tunnels 3)
                  (:Stairs 6)
                  (:Generator 5)))
    
    (:Tunnels ((:Ramp 7)
               (:Ladder 7)))
    
    (:Generator ((:Middle 2)
                 (:Ladder 4)
                 (:T-Spawn 2)))
    
    (:Middle ((:Connector 5)
              (:Boost 4)
              (:CT-Spawn 5)
              (:Generator 8)))
    
    (:Ladder ((:Tunnels 3)
              (:Generator 2)))
    
    (:Boost ((:Middle 6)
             (:B 3)
             (:CT-Spawn 2)))

    (:Connector ((:Middle 6)
                 (:A 3)
                 (:Window 6)
                 (:CT-Spawn 5)))

    (:Ramp ((:Short 5)
            (:Tunnels 3)
            (:A 2)))
    
    (:Stairs ((:B 3)
              (:S-Corridor 5)
              (:T-Spawn 8)))

    (:B ((:Stairs 7)
         (:Boost 6)
         (:CT-Spawn 3)))
    
    (:A ((:Short 6)
         (:Connector 7)
         (:Window 5)))

    (:Window ((:A 4)
              (:CT-Spawn 2)
              (:Connector 7)))

    (:Short ((:A 6)
             (:Ramp 3)))
    
    (:CT-Spawn ((:Window 3)
                (:Connector 5)
                (:Boost 4)
                (:B 8)))))

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
