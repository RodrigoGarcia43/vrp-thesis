(let* ((depot (client 0))
       (c1 (client 1))
       (c2 (client 2))
       (r (route 1 `(,depot ,c1 ,c2 ,depot) nil))
       (sol (solution `(,r)))
       (act (action-distance 0))
       (graph (init-graph sol)))
    (format t "~a~%" graph))

(let* ((depot (client 0))
       (c1 (client 1))
       (c2 (client 2))
       (r (route 1 `(,c1 ,c2 ,depot) depot))
       (sol (solution `(,r)))
       (act (action-distance 0))
       (*graph* (init-graph sol))
       (on (output-graph-node 0))
       (mat #2A((0 1 2) (1 0 2) (2 2 0))))
    (progn
        (setf (gethash 'total-distance (slot-to-output *graph*)) on)
        (loop for r in (routes sol) do
            (loop for c in (clients r) do
               (progn 
                   (move-from-to (prev-client r) c 'total-distance *graph* mat)
                   (setf (prev-client r) c))))
        (format t "EVALUATION (should be 5): ~a~%" (output-value on))))

(let* ((depot (client 0))
       (c1 (client 1))
       (c2 (client 2))
       (r (route 1 `(,c1 ,c2 ,depot) depot))
       (sol (solution `(,r)))
       (act (action-distance 0))
       (*graph* (init-graph sol))
       (mat #2A((0 1 2) (1 0 2) (2 2 0))))
    (progn
        (defallocable 'total-distance 0 *graph*)
        (loop for r in (routes sol) do
            (progn
                (defallocable 'route-distance 0 *graph*)
                (loop for c in (clients r) do
                   (progn 
                       (move-from-to (prev-client r) c 'route-distance *graph* mat)
                       (setf (prev-client r) c)))
                (increment-with 'total-distance 'route-distance *graph*)))
        (format t "EVALUATION (should be 5): ~a~%" (output-value 
                                                        (gethash 'total-distance (slot-to-output *graph*))))))
