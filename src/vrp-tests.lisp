(let* ((*vrp-logging* 1))
  (vrp-log '(unload) "unloading"
           '((vehicle unconditionally-unload-vehicle)
             (client demand-client)
             (route route-for-simulation)
             (solution basic-solution)
             (problem distance-problem)
             (action log-action))
           t))

(let* ((*vrp-logging* 1))
  (vrp-log '(unload :after) "unloading standard vehicle"
           '((vehicle unconditionally-unload-vehicle)
             (client demand-client)
             (route route-for-simulation)
             (solution basic-solution)
             (problem distance-problem)
             (action log-action))
           t))

(progn
  (setf *add-calls-to-vrp-log-in-defbehavior* nil)
  (pp-expand (defbehavior unload ((vehicle  unconditionally-unload-vehicle)
                        (client   demand-client)
                        (route    t)
                        (solution t)
                        (problem  t)
                        (action simulate-load-action))
     :log-str "Downloading the client's demand from the vehicle" 
     (decf (cargo vehicle) (demand client))))
  (setf *add-calls-to-vrp-log-in-defbehavior* nil))

(progn
  (setf *add-calls-to-vrp-log-in-defbehavior* t)
  (pp-expand (defbehavior unload ((vehicle  unconditionally-unload-vehicle)
                        (client   demand-client)
                        (route    t)
                        (solution t)
                        (problem  t)
                        (action simulate-load-action))
     :log-str "Downloading the client's demand from the vehicle" 
     (decf (cargo vehicle) (demand client))))
  (setf *add-calls-to-vrp-log-in-defbehavior* nil))

(pp-expand (defbehavior visit-client :before ((client basic-client)
                                              (c2 basic-client)
                                              v
                                              (vehicle basic-vehicle)
                                              action)
             (this is the generated code)
             (hello world)
             (print t)))

(pp-expand (defbehavior visit-client :before ((client basic-client)
                                              (c2 basic-client)
                                              v
                                              (vehicle basic-vehicle)
                                              action)
              :log-str "testing defbehavior logging functionality"
             (this is the generated code)
             (hello world)
             (print t)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (a (simulate-load-action)))
  (format t "Testing unload:~%")
  (check-= 60 (cargo v1))
  (format t "unload at client ~a~%" c1)
  (unload v1 c1 t t t a)
  (check-= 30 (cargo v1))
  (format t "unload at client ~a~%" c2)
  (unload v1 c2 t t t a)
  (check-= 10 (cargo v1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (a (simulate-load-action))
       (*vrp-logging* 1))
  (format t "~%===========================
Testing *logging* at unload
===========================~2%")
  (unload v1 c1 t t t a)
  (unload v1 c2 t t t a))

(let* ((*vrp-logging* 1))
  (format t "Testing *logging* at visit-client:~%")
  (visit-client t t t t t t)
  (visit-client t t t t t t))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (c4 (basic-cvrp-client 4 40))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2 c3 c4)))
       (*vrp-logging* 0))
  (format t "===================================
Testing :after method visit-client:
===================================~%")
  (setf (previous-client r1) (depot r1))
  (format t "  Check that the depot is the previous-client:~%")
  (check-obj= d0 (previous-client r1))
  (loop for c in (list c1 c2 c3 c4)
        doing (visit-client t c r1 t t t)
        doing (format t "  Check that the previous-client is updated:~%")
        doing (check-obj= c (previous-client r1))
        doing (format t "     previous-client: ~a~%"
                      (previous-client r1))))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (c4 (basic-cvrp-client 4 40))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2 c3 c4)))
       (*vrp-logging* 1))
  (format t "===================================
Testing :after method visit-client:
===================================~%")
  (format t "Applicable methods:~%   ~a~2%"
          (compute-applicable-methods #'visit-client
                                      (list t c1 r1 t t t)))

  (setf (previous-client r1) (depot r1))
  (loop for c in (list c1 c2 c3 c4)
        doing (visit-client t c r1 t t t)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (a (simulate-load-action))
       (*vrp-logging* 1))
  (format t "Testing visit-client:~%")
  (visit-client v1 c1 t t t a)
  (visit-client v1 c2 t t t a))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (*vrp-logging* 0))
  (format t "Testing when-route-begins:~%")
  (check-nil (previous-client r1))
  (when-route-begins v1 r1 t t t)
  (check-non-nil (previous-client r1))
  (check-obj= d0 (previous-client r1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (a1 (route-distance-action 1 50))
       (*vrp-logging* 0))
  (format t "Testing when-route-begins for route-distance-action:~%")
  (check-= 50 (current-distance a1))
  (when-route-begins v1 r1 t t a1)
  (check-= 0 (current-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (a1 (route-distance-action 1 50))
       (*vrp-logging* 1))
  (format t "Testing *logging* when-route-begins for route-distance-action:~%")
  (when-route-begins v1 r1 t t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (c4 (basic-cvrp-client 4 40))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 50 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (a1 (simulate-load-action))
       (*vrp-logging* 0))
  (format t "~%===========================================================
Testing when-route-begins for simulate-load-action
===========================================================~2%")
  (format t "  Testing when-route-begins with route r1:~%")
  (check-= 0 (cargo v1))
  (when-route-begins v1 r1 t t a1)
  (check-= 50 (cargo v1))
  (format t "  Testing when-route-begins with route r2:~%")
  (check-= 0 (cargo v2))
  (when-route-begins v2 r2 t t a1)
  (check-= 70 (cargo v2)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (c4 (basic-cvrp-client 4 40))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 50 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (a1 (simulate-load-action))
       (*vrp-logging* 1))
  (format t "~%===========================================================
Testing when-route-begins for simulate-load-action
===========================================================~2%")
  (format t "--- Testing when-route-begins with route r1:~%")
  (when-route-begins v1 r1 t t a1)
  (format t "--- Testing when-route-begins with route r2:~%")
  (when-route-begins v2 r2 t t a1)

  (format t "~%Now testing without the simulate-load-action.~%")

  (format t "--- Testing when-route-begins with route r1 [no load]:~%")
  (when-route-begins v1 r1 t t t)
  (format t "--- Testing when-route-begins with route r2 [no load]:~%")
  (when-route-begins v2 r2 t t t))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (c4 (basic-cvrp-client 4 40))
       (v1 (cvrp-vehicle 1 60 60))
       (v2 (cvrp-vehicle 1 50 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (a1 (basic-vehicle-capacity-action 1))
       (a2 (basic-vehicle-capacity-action 2))
       (*vrp-logging* 0))
  (format t "~%===========================================================
Testing when-route-begins for basic-vehicle-capacity-action
===========================================================~2%")
  (check-= 0 (capacity-violation a1))
  (when-route-begins v1 r1 t t a1)
  (check-= 0 (capacity-violation a1))
  (format t "  Testing when-route-begins for basic-vehicle-capacity-action~%")
  (check-= 0 (capacity-violation a2))
  (when-route-begins v2 r2 t t a2)
  (check-= 20 (capacity-violation a2)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (a1 (basic-vehicle-capacity-action 1))
       (*vrp-logging* 1))
  (format t "Testing *logging* when-route-begins for route-distance-action:~%")
  (when-route-begins r1 t t a1))

(let* ((*vrp-logging* 1))
  (format t "==================================
Testing *logging* at move-from-to:
==================================~2%")
  (move-from-to t t t t t t t)
  (move-from-to t t t t t t t))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 30))
       (d0 (basic-depot))
       (list (list d0 c1 c2 c3))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (problem (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (route-distance-action 1))
       (*vrp-logging* 0))
  (format t "================================================
Testing move-from-to with route-distance-action:
================================================~2%")

  (check-= 0 (current-distance a1))
  (move-from-to t d0 c1 t t problem a1)
  (check-= 1 (current-distance a1))

  (setf (current-distance a1) 0)
  (loop for previous-client in (butlast list)
        for current-client in (rest list)
        doing (move-from-to t previous-client current-client t t problem a1))
  (check-= 11 (current-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (a1 (basic-vehicle-capacity-action 1))
       (*vrp-logging* 1))
  (format t "Testing *logging* when-route-begins for route-distance-action:~%")
  (when-route-begins r1 t t a1))

(let* ((*vrp-logging* 1))
  (format t "========================
Testing when-route-ends:
========================~2%")
  (when-route-ends t t t t))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (route-distance-action 1))
       (*vrp-logging* 1))
  (bformat t "Testing when-route-ends for route-for-simulation")
  (setf (previous-client r1) c2)
  (format t "Applicable methods: ~a~%"
          (compute-applicable-methods #'when-route-ends
                                      (list r1 t dp a1)))
  (when-route-ends r1 t dp a1)
  (check-= 2 (current-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (d1 (basic-depot 3))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients nil))
       (distance #2A ((0 1 2 3 4)
                      (1 0 4 5 3)
                      (2 4 0 6 5)
                      (3 5 6 0 1)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (route-distance-action 1))
       (*vrp-logging* 1))
  (bformat t "Testing when-route-ends for route-for-simulation")

  (setf (previous-client r1) c2)
  (format t "Applicable methods: ~a~%"
          (compute-applicable-methods #'when-route-ends
                                      (list r1 t dp a1)))
  (when-route-ends r1 t dp a1)
  (check-= 2 (current-distance a1))

  (format t "Testing an empty route~%")
  (setf a1 (route-distance-action 0))
  (setf (previous-client r2) (depot r2))
  (when-route-ends r2 t dp a1)
  (format t "total distance with empty route: ~a~%" (current-distance a1))
  )

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 t)
       (*vrp-logging* 1))
  (format t "======================
Testing simulate-route
======================~2%")
  (simulate-route r1 t t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (simulate-load-action))
       (*vrp-logging* 1))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-route r1 t t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (route-distance-action 1))
       (*vrp-logging* 1))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-route r1 t dp a1)

  (format t "~%  Total distance travelled in the route: ~a~%"
          (current-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-vehicle-capacity-action 1))
       (*vrp-logging* 1))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-route r1 t dp a1)

  (format t "--- testing capacity violations:~%")
  (check-= 0 (capacity-violation a1))
  (setf (demand c1) 50)
  (let* ((*vrp-logging* 0))
    (simulate-route r1 t dp a1))
  (check-= 10 (capacity-violation a1)))

(let* ((*vrp-logging* 1))
  (format t "========================
Testing when-route-ends:
========================~2%")
  (when-route-ends t t t t))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-solution-distance-action 0))
       (*vrp-logging* 1))
  (format t "============================
Testing simulate-route after
============================~2%")
  (simulate-route r1 t t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-solution-distance-action 0))
       (*vrp-logging* 1))
  (format t "Testing simulate-route after")
  (simulate-route r1 t dp a1)
  (check-= 7 (total-distance a1))
  (setf (total-distance a1) 1000)
  (simulate-route r1 t dp a1)
  (check-= 1007 (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (d1 (basic-depot 3))

       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-solution-distance-action 0))
       (*vrp-logging* 1))
  (bformat t "Testing simulate-route after with end-depot")
  (simulate-route r1 t dp a1)
  (format t "end-depot: ~a~%" (end-depot r1))
  (format t "total-distance: ~a~%" (total-distance a1))
  (check-= 11 (total-distance a1))
  (setf (total-distance a1) 1000)
  (simulate-route r1 t dp a1)
  (check-= 1011 (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (v1 (cvrp-vehicle 1 60 60))
       (d0 (basic-depot))
       (d1 (basic-depot 3))

       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients nil))
       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-solution-distance-action 0))
       (*vrp-logging* 1))
  (bformat t "Testing simulate-route after with end-depot")
  (simulate-route r1 t dp a1)
  (format t "end-depot: ~a~%" (end-depot r1))
  (format t "total-distance: ~a~%" (total-distance a1))


  (format t "~%Testing simulate-route after with an empty route~%")
  (setf a1 (basic-solution-distance-action 0))
  (format t "end-depot: ~a~%" (end-depot r2))
  (simulate-route r2 t dp a1)
  (format t "total-distance: ~a~%" (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 t)
       (*vrp-logging* 1))
  (format t "======================
Testing simulate-solution
======================~2%")
  (simulate-solution s1 t a1))

(let* ((a1 (basic-capacity-penalty-action))
       (*vrp-logging* 0))
  (format t "======================================
Testing finish-the-solution-simulation
======================================~2%")
  (setf (capacity-violation a1) 10)
  (finish-the-solution-simulation t t a1)
  (check-= 10 (total-penalty a1))
  (loop for violation in '(1 5 10 100)
        for factor in '(10 20 100 1000)
        doing (progn
                (setf (capacity-violation a1) violation)
                (setf (penalty-factor a1) factor)
                (finish-the-solution-simulation t t a1)
                (check-= (* violation factor) (total-penalty a1)))))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 40))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 10))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-capacity-penalty-action :penalty-factor 100))
       (*vrp-logging* 1))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 t a1)
  (check-= 1000 (total-penalty a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (a1 t)
       (*vrp-logging* 1))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 60))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))             

       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (simulate-load-action))
       (*vrp-logging* 1))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-solution s1 dp a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 60))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))             

       (distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-vehicle-capacity-action 1))
       (*vrp-logging* 1))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-solution s1 dp a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 60))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))             

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                               :distance-matrix distance))
       (a1 (basic-solution-distance-action 1))
       (*vrp-logging* 0))
  (format t "=================================
Testing simulate-route with loads
=================================~2%")
  (simulate-solution s1 dp a1)

  (format t "~%  Total distance travelled in the solution: ~a~%"
          (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-solution-distance-action 100))
       (*vrp-logging* 1))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-solution-distance-action 100))
       (*vrp-logging* 0))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (check-= 100 (total-distance a1))
  (simulate-solution s1 t a1)
  (check-= 0 (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-vehicle-capacity-action 100))
       (*vrp-logging* 1))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 t a1))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-solution-distance-action 100))
       (*vrp-logging* 0))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (check-= 100 (total-distance a1))
  (simulate-solution s1 t a1)
  (check-= 0 (total-distance a1)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (a1 (basic-vehicle-capacity-action 1))
       (*vrp-logging* 0))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 t a1)
  (check-= 20 (capacity-violation a1))
  (loop for c      in '(50 70 80 90 100)
        for expect in '(40 20 10  0   0)
        for a = (basic-vehicle-capacity-action 1 0)
        doing (progn
                (setf (capacity v2) c)
                (simulate-solution s1 t a)
                (check-= expect (capacity-violation a)))))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))
  (format t "=========================
Testing simulate-solution
=========================~2%")
  (simulate-solution s1 dp action)
  (check-= 23 (total-distance action))
  (check-= 20 (capacity-violation action))
  (check-= 20000 (total-penalty action)))

(let* ((c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (v1 (cvrp-vehicle 1 60 0))
       (v2 (cvrp-vehicle 1 70 0))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v2
            :depot d0
            :end-depot d1
            :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 8)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (1 4 6 4 8 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))

  (bformat t "Testing simulate-solution with end depot")
  (format t "end-depot (route 1): ~a~%" (end-depot r1))
  ;; test for route 1
  (simulate-route r1 s1 dp action)
  (format t "distance for route 1: ~a~%" (total-distance action))
  ;; testing for route 2
  (setf action (basic-cvrp-action))
  (simulate-route r2 s1 dp action)
  (format t "distance for route 2: ~a~%" (total-distance action))
  ;; testing for the solution
  (setf action (basic-cvrp-action))
  (simulate-solution s1 dp action)
  (format t "distance in solution: ~a~%" (total-distance action))
  (check-= 25 (total-distance action))
  (check-= 20 (capacity-violation action))
  (check-= 20000 (total-penalty action))
  )
