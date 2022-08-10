(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 5) (3) (4)) p1)
      ;; (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           (criterion-code `((select-route r1)
                             (select-client c1 from r1)
                             (select-route r2)
                             (insert-client c2 into r2)))

           )

      (bformat t "Testing DNS")




      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-best-neighbor  
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution
      ;; (s1 ((1 2 5) (3) (4)) p1)
      (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (insert-client c1 into r1)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (insert-client c1 into r2)))

           (criterion-code  `((select-route r1)
                              (select-client c1 from r1)
                              (select-route r2)
                              (insert-client c1 into r2)))

           )

      (bformat t "Testing DNS with generators")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-best-neighbor  
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (cvrp-action (basic-cvrp-action))

       (criterion-code  `((select-route r1)
                          (select-client c1 from r1)
                          (select-route r2)
                          (insert-client c1 into r2)))

       ;; (criterion-code '((select-route r1)
       ;;                   (select-client c1 from r1)
       ;;                   (select-route r2)
       ;;                   (select-client c2 from r2)
       ;;                   (swap-clients c1 c2)))

       ;; (neighborhood-exhaustive
       ;;  (make-neighborhood-criterion
       ;;   criterion-code
       ;;   +exhaustive-search-strategy+
       ;;   selection-strategy))
       )

      (bformat t "Testing DNS with generators and a-n33")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-best-neighbor  
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (setf results (descent-neighborhood-search
      ;;                p1 s1 neighborhood-exhaustive
      ;;                :max-iter max-iterations
      ;;                :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))
      )

(let* ((s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            ff-a-n33-k6-problem))
       (p1 ff-a-n33-k6-problem)
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+)))

  (bformat t "Testing DNS")

  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (funcall neighborhood-exploration s1 p1 action))

  ;; (format t "Cost: ~a~%" (cost results))
  ;; (pp-solution results t) (terpri)

  (setf results (descent-neighborhood-search  
                 p1 s1 neighborhood-exploration
                 :max-iter 300
                 :action action))

  ;; (format t "Cost: ~a~%" (cost (first results)))
  ;; (pp-solution (first results) t) (terpri)


  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)

        ;; (format t "Best value through Yoel's: ~a~%"
        ;;         (solution-cost best-solution-exhaustive
        ;;                        p1 cvrp-action))
        ;; (format t "action: ~a~%" cvrp-action)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  ;; (if best-solution-first-improvement
  ;;     (then
  ;;       (format t "Best value through 1st: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through 1st:~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-first-improvement) t) (terpri)))

  ;; (if best-solution-random-improvement
  ;;     (then
  ;;       (format t "Best value through random: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through random::~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-random-improvement) t) (terpri)))


  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (*vrp-logging* 0))

  (bformat t "Testing VND with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (descent-neighborhood-search  
                   p1 s1 neighborhood-exploration
                   :max-iter 300
                   :action action))

    (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))


    (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 5) (3) (4)) p1)
      ;; (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           (criterion-code  `((select-route r1)
                              (select-client c1 from r1)
                              (select-route r2)
                              (insert-client c1 into r2)))

           )

      (bformat t "Testing DNS")




      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-first-improvement  
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution
      ;; (s1 ((1 2 5) (3) (4)) p1)
      (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (insert-client c1 into r1)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (insert-client c1 into r2)))

           (criterion-code  `((select-route r1)
                              (select-client c1 from r1)
                              (select-route r2)
                              (insert-client c1 into r2)))

           )

      (bformat t "Testing DNS with generators")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-first-improvement
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (cvrp-action (basic-cvrp-action))

       (criterion-code  `((select-route r1)
                          (select-client c1 from r1)
                          (select-route r2)
                          (insert-client c1 into r2)))

       ;; (criterion-code '((select-route r1)
       ;;                   (select-client c1 from r1)
       ;;                   (select-route r2)
       ;;                   (select-client c2 from r2)
       ;;                   (swap-clients c1 c2)))

       ;; (neighborhood-exhaustive
       ;;  (make-neighborhood-criterion
       ;;   criterion-code
       ;;   +exhaustive-search-strategy+
       ;;   selection-strategy))
       )

      (bformat t "Testing DNS with generators and a-n33")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-first-improvement  
                     p1
                     s1 
                     criterion-code
                     :max-iter 100
                     :action action))

      ;; (setf results (descent-neighborhood-search
      ;;                p1 s1 neighborhood-exhaustive
      ;;                :max-iter max-iterations
      ;;                :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))
      )

(let* ((s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            ff-a-n33-k6-problem))
       (p1 ff-a-n33-k6-problem)
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+)))

  (bformat t "Testing DNS")

  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (funcall neighborhood-exploration s1 p1 action))

  ;; (format t "Cost: ~a~%" (cost results))
  ;; (pp-solution results t) (terpri)

  (setf results (descent-neighborhood-search  
                 p1 s1 neighborhood-exploration
                 :max-iter 300
                 :action action))

  ;; (format t "Cost: ~a~%" (cost (first results)))
  ;; (pp-solution (first results) t) (terpri)


  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)

        ;; (format t "Best value through Yoel's: ~a~%"
        ;;         (solution-cost best-solution-exhaustive
        ;;                        p1 cvrp-action))
        ;; (format t "action: ~a~%" cvrp-action)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  ;; (if best-solution-first-improvement
  ;;     (then
  ;;       (format t "Best value through 1st: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through 1st:~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-first-improvement) t) (terpri)))

  ;; (if best-solution-random-improvement
  ;;     (then
  ;;       (format t "Best value through random: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through random::~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-random-improvement) t) (terpri)))


  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (*vrp-logging* 0))

  (bformat t "Testing VND with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (descent-neighborhood-search  
                   p1 s1 neighborhood-exploration
                   :max-iter 300
                   :action action))

    (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))


    (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 5) (3) (4)) p1)
      ;; (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           (criterion-code  `((select-route r1)
                              (select-client c1 from r1)
                              (select-route r2)
                              (insert-client c1 into r2)))

           )

      (bformat t "Testing DNS")




      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-random-improvement
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Best solution found with cost: ~a~%"
                    (cost best-solution-exhaustive))
            (pp-solution best-solution-exhaustive t) (terpri)))

          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution
      ;; (s1 ((1 2 5) (3) (4)) p1)
      (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action  
                         :penalty-factor 1000))
           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (select-client c2 from r2)
           ;;                   (swap-clients c1 c2)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (insert-client c1 into r1)))

           ;; (criterion-code `((select-route r1)
           ;;                   (select-client c1 from r1)
           ;;                   (select-route r2)
           ;;                   (insert-client c1 into r2)))

           (criterion-code  `((select-route r1)
                              (select-client c1 from r1)
                              (select-route r2)
                              (insert-client c1 into r2)))

           )

      (bformat t "Testing DNS with generators")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-random-improvement
                     p1
                     s1 
                     criterion-code
                     :max-iter 30
                     :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (cvrp-action (basic-cvrp-action))

       (criterion-code  `((select-route r1)
                          (select-client c1 from r1)
                          (select-route r2)
                          (insert-client c1 into r2)))

       ;; (criterion-code '((select-route r1)
       ;;                   (select-client c1 from r1)
       ;;                   (select-route r2)
       ;;                   (select-client c2 from r2)
       ;;                   (swap-clients c1 c2)))

       ;; (neighborhood-exhaustive
       ;;  (make-neighborhood-criterion
       ;;   criterion-code
       ;;   +exhaustive-search-strategy+
       ;;   selection-strategy))
       )

      (bformat t "Testing DNS with generators and a-n33")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (dns-with-generators-random-improvement  
                     p1
                     s1 
                     criterion-code
                     :max-iter 100
                     :action action))

      ;; (setf results (descent-neighborhood-search
      ;;                p1 s1 neighborhood-exhaustive
      ;;                :max-iter max-iterations
      ;;                :action action))

      ;; (format t "Iterations: ~a. Optimum found ~a.~%"
      ;;         (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            (format t "  with cost: ~a~%"
                    (cost best-solution-exhaustive))

            (simulate-solution best-solution-exhaustive p1 cvrp-action)


            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

            (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))
      )

(let* ((s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            ff-a-n33-k6-problem))
       (p1 ff-a-n33-k6-problem)
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+)))

  (bformat t "Testing DNS")

  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (funcall neighborhood-exploration s1 p1 action))

  ;; (format t "Cost: ~a~%" (cost results))
  ;; (pp-solution results t) (terpri)

  (setf results (descent-neighborhood-search  
                 p1 s1 neighborhood-exploration
                 :max-iter 300
                 :action action))

  ;; (format t "Cost: ~a~%" (cost (first results)))
  ;; (pp-solution (first results) t) (terpri)


  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)

        ;; (format t "Best value through Yoel's: ~a~%"
        ;;         (solution-cost best-solution-exhaustive
        ;;                        p1 cvrp-action))
        ;; (format t "action: ~a~%" cvrp-action)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  ;; (if best-solution-first-improvement
  ;;     (then
  ;;       (format t "Best value through 1st: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through 1st:~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-first-improvement) t) (terpri)))

  ;; (if best-solution-random-improvement
  ;;     (then
  ;;       (format t "Best value through random: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through random::~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-random-improvement) t) (terpri)))


  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (*vrp-logging* 0))

  (bformat t "Testing VND with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (descent-neighborhood-search  
                   p1 s1 neighborhood-exploration
                   :max-iter 300
                   :action action))

    (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))


    (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))

           )

      (bformat t "Testing VNS with generators")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-exhaustive-best-neighbor 
                     p1 s1 (list rab
                                 rarb
                                 rarac
                                 )
                     :max-iter 150
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-exhaustive-best-neighbor 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 150
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-exhaustive-best-neighbor 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 200
                 :print-neighborhood-size t 
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t))

      (else
        (format t "Initial solution was optimum!~%")))

  )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))

           )

      (bformat t "Testing VNS with generators")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-exhaustive-best-neighbor-max-neigh-size 
                     p1 s1 (list rab
                                 rarb
                                 rarac
                                 )
                     :max-iter 150
                     :print-neighborhood-size t
                     :max-neighborhood-size 20
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-exhaustive-best-neighbor-max-neigh-size 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 350
                 :print-neighborhood-size t
                 :max-neighborhood-size 5000
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-exhaustive-best-neighbor-max-neigh-size 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 200
                 :print-neighborhood-size t 
                 :print-neighborhood-size t
                 :max-neighborhood-size 1000
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t))

      (else
        (format t "Initial solution was optimum!~%")))

  )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action*))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))

           )

      (bformat t "Testing VNS with generators")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost sc1))
      (pp-solution s1 t)

      (setf results (vns-adaptative-first-improvement-max-neigh-size 
                     p1 s1 (list rab
                                 rarb
                                 rarac
                                 )
                     :max-iter 150
                     :print-neighborhood-size t
                     :max-neighborhood-size 15
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-adaptative-first-improvement-max-neigh-size 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 350
                 :print-neighborhood-size t
                 :max-neighborhood-size 700
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))

       )

  (bformat t "Testing VNS with generators")


  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (vns-adaptative-first-improvement-max-neigh-size 
                 p1 s1 (list rab
                             rarb
                             rarac
                             ref
                             rerf
                             rereg
                             rerehg
                             )
                 :max-iter 550
                 :print-neighborhood-size t
                 :max-neighborhood-size 3000
                 :action action))

  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t))

      (else
        (format t "Initial solution was optimum!~%")))

  )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-shake 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list rabs*     ;1
                            rarbs*   ;2
                            raracs*   ;3
                            refs*     ;4
                            reregs*   ;5
                            rehfs*    ;6
                            rerfs*    ;7
                            rehrfs*   ;8
                            rerehgs*  ;9
                            rehregs*  ;10
                            rehrehgs* ;11
                            )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy
                      ;; (random-improvement-smart 0.5 )
                      *random-improvement-with-candidates*
                      ;; +first-improvement+
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      ;; (time
      ;;  (setf results (vns-code
      ;;                 p1 s1
      ;;                 (list rab         ;1
      ;;                       rarb        ;2
      ;;                       rarac       ;3
      ;;                       ref     ;4
      ;;                       rereg   ;5
      ;;                       rerf    ;6
      ;;                       rehf    ;7
      ;;                       rehrf   ;8
      ;;                       )
      ;;                 ;; :inner-search-max-iter max-iterations
      ;;                 :action action
      ;;                 :max-iter max-iterations
      ;;                 :selection-strategy +best-improvement+
      ;;                 ;; :shake-search (jump-around-search-strategy no-of-jumps)
      ;;                 :search-strategy +exhaustive-search-strategy+)))

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs             ;0
                       rarbs            ;1
                       raracs           ;2
                       refs             ;3
                       rerfs            ;4
                       rehfs            ;5
                       rehrfs           ;6
                       reregs           ;7
                       rehrfs           ;8
                       rerehgs          ;9
                       rehregs          ;10
                       rehrehgs          ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy +best-improvement+
                      ;; :shake-search (jump-around-search-strategy no-of-jumps)
                      :search-strategy +exhaustive-search-strategy+)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*             ;3
                       rerfs*            ;4
                       rehfs*            ;5
                       rehrfs*           ;6
                       reregs*           ;7
                       rehrfs*           ;8
                       rerehgs*          ;9
                       rehregs*          ;10
                       rehrehgs*         ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      ;; :shake-search (jump-around-search-strategy no-of-jumps)
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*             ;3
                       rerfs*            ;4
                       rehfs*            ;5
                       rehrfs*           ;6
                       reregs*           ;7
                       rehrfs*           ;8
                       rerehgs*          ;9
                       rehregs*          ;10
                       rehrehgs*         ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter 1000
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000)))

      (bformat t "Testing VNS")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-no-output 
                     p1 s1 (list rab-best
                                 rarb-best
                                 rarac-best)
                     :max-iter 150
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-no-output
                     p1 s1
                     (list rab-random
                           rehrf-random
                           rarac-random
                           ref-random
                           rereg-random
                           rerf-random
                           rehf-random
                           rarb-random
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(search-strategy +exhaustive-search-strategy+)
       (search-strategy (random-neighborhood-search-strategy 100))

       (selection-strategy +first-improvement+)
       ;(selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (rarac-r-ex (make-neighborhood-criterion
                    rarac-description
                    search-strategy
                    selection-strategy))

       (rab-r-ex (make-neighborhood-criterion
                  rab-description
                  search-strategy
                  selection-strategy))

       (rereg-r-ex (make-neighborhood-criterion
                    rereg-description
                    search-strategy
                    selection-strategy))


       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns
                     p1 s1
                     (list
                      rarac-r-ex
                      rab-r-ex
                      rereg-r-ex
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-no-output 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-no-output
                      p1 s1
                      (list rabs*     ;1
                            rarbs*   ;2
                            raracs*   ;3
                            refs*     ;4
                            reregs*   ;5
                            rehfs*    ;6
                            rerfs*    ;7
                            rehrfs*   ;8
                            rerehgs*  ;9
                            rehregs*  ;10
                            rehrehgs* ;11
                            )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy
                      ;; (random-improvement-smart 0.5 )
                      *random-improvement*
                      ;; +first-improvement+
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-shake 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy *first-improvement*
                     ;; :shake-search (jump-around-search-strategy no-of-jumps)
                     :search-strategy *exhaustive-search-strategy*))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +first-improvement+
                     :search-strategy +exhaustive-search-strategy+))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (results nil)
       (max-iterations 1000)

       (*vrp-logging* 0))

  (bformat t "Testing VNS with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy *first-improvement*
                     ;; :shake-search (jump-around-search-strategy no-of-jumps)
                     :search-strategy *exhaustive-search-strategy*))

    (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

    (setf best-solution-exhaustive (first results))

    (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))


    ))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 ff-a-n33-k6-problem)
       (s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action)))



      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-scrambling
                      p1 s1
                      (list
                       rabs*            ;1
                       rarbs*           ;2
                       raracs*          ;3
                       refs*             ;4
                       rerfs*            ;5
                       rehfs*            ;6
                       rehrfs*           ;7
                       reregs*           ;8
                       rehrfs*           ;9
                       rerehgs*          ;10
                       rehregs*          ;11
                       rehrehgs*         ;12
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-scrambling
                      p1 s1
                      (list
                       rabs*            ;1
                       rarbs*           ;2
                       raracs*          ;3
                       refs*            ;4
                       rerfs*           ;5
                       rehfs*           ;6
                       rehrfs*          ;7
                       reregs*          ;8
                       rehrfs*          ;9
                       rerehgs*         ;10
                       rehregs*         ;11
                       rehrehgs*        ;12
                       )
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 200)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      ;; (setf results (vns-shake
      ;;                p1 s1
      ;;                (list
      ;;                 rab
      ;;                 rehrf
      ;;                 rarac
      ;;                 ref
      ;;                 rereg
      ;;                 rerf
      ;;                 rehf
      ;;                 rarb
      ;;                 )
      ;;                :max-iter max-iterations
      ;;                :action action
      ;;                ;; :shake-search (jump-around-search-strategy 10)
      ;;                :shake-search (jump-around-search-strategy 5)
      ;;                :shake-selection +jump-around-return-last-neighbor+
      ;;                :descent-selection-strategy +random-improvement+))

      (setf results (vns-code
                     p1 s1
                     (list
                      rab
                      rehrf
                      rarac
                      ref
                      rereg
                      rerf
                      rehf
                      rarb
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 100)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
       ;; (setf results (vns-shake
       ;;                p1 s1
       ;;                (list
       ;;                 ; rabs
       ;;                 rab
       ;;                 rehrf
       ;;                 ;raracs
       ;;                 rarac
       ;;                 ref
       ;;                 rereg
       ;;                 rerf
       ;;                 rehf
       ;;                 ;rarbs
       ;;                 rarb
       ;;                 )
       ;;                :max-iter max-iterations
       ;;                :action action
       ;;                ;; :shake-search (jump-around-search-strategy 10)
       ;;                :shake-search (jump-around-search-strategy 1)
       ;;                :shake-selection +jump-around-return-last-neighbor+
       ;;                :selection-strategy +first-improvement+))

         (setf results (vns-shake 
                     p1 s1
                     (list
                      rab
                      ;;rehrf
                      raracs
                      ref
                      reregs
                      ;;rerf
                      ;;rehf
                      rarb
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 20))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 100)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake 
                     p1 s1
                     (list
                      rabs*              ;1
                      rarbs*             ;2
                      raracs*            ;3
                      refs*              ;4
                      reregs*            ;5
                      rehfs*             ;6
                      rerfs*             ;7
                      rads*              ;8
                      rehrfs*            ;9
                      rerehgs*           ;10
                      rehregs*           ;11
                      rehrehgs*          ;12
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 20))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 20)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake 
                     p1 s1
                     (list
                      rabs*              ;1
                      rarbs*             ;2
                      raracs*            ;3
                      refs*              ;4
                      ;; reregs*            ;5
                      ;; rehfs*             ;6
                      ;; rerfs*             ;7
                      ;; rads*              ;8
                      ;; rehrfs*            ;9
                      ;; rerehgs*           ;10
                      ;; rehregs*           ;11
                      ;; rehrehgs*          ;12
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 10))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      (format t "distance-stack: ~a~%"
              (length (delta-distance-stack action)))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 200))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 60))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy
                     ;;(random-improvement-smart 0.7)
                      *first-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 50))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))
      ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-random-cvrp-solution p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n65-k9-problem")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      ;; rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy
                     ;; (random-improvement-smart 0.7)
                      *first-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 500))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))
      ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-basic-cvrp-solution-from-list
            1 `(( 2 )
                ( 23 )( 48 )( 35  46 )( 45  6 )( 63 )( 55 )
                ( 51  38 )( 40 )( 28  11 )( 25 )(43  22  4 )
                ( 29  42  50  60  19 )( 57 )( 49 )( 53 )( 41 )
                ( 44  37 )( 52 )( 16  15  5 )( 10  58 )
                ( 64  24  39 )( 61 )( 47  21 )( 9 )( 14  32 )
                ( 56 )( 33  12  34  27 )( 62 )( 20  26  1 )
                ( 17 )( 18 )( 13  59 )( 54  36 30  7  3  8 )
                ( 31 ))
            a-n65-k9-problem))
            (best-solution-exhaustive nil)
            (action (delta-cvrp-action*))
            (action-for-shake (delta-cvrp-action))
            (results nil)
            (max-iterations 200)
            (cvrp-action (basic-cvrp-action))
            )


           (bformat t "Testing DNS with a-n65-k9-problem")

           ;; first we compute the cost of the solution
           (simulate-solution s1 p1 cvrp-action)
           (setf (cost s1) (+ (total-distance cvrp-action)
                              (total-penalty cvrp-action)))

           (format t "Original solution (with cost ~a):~%"
                   (cost s1))
           (pp-solution s1 t)

           (time
            (progn
              (setf results (vns-shake-smart
                          p1 s1
                          (list
                           rabs*              ;0
                           rarbs*             ;1
                           raracs*            ;2
                           refs*              ;3
                           reregs*            ;4
                           rehfs*             ;5
                           rerfs*             ;6
                           rads*              ;7
                           rehrfs*            ;8
                           rerehgs*           ;9
                           rehregs*           ;10
                           rehrehgs*          ;11
                           )
                          :max-iter max-iterations
                          :action action
                          :action-for-shake action-for-shake
                          :selection-strategy
                          ;; (random-improvement-smart 0.7)
                           *first-improvement*
                          :search-strategy *exhaustive-search-strategy*
                          :inner-search-max-iter 50))

           (format t "finished vns-shake~%")


           (format t "Iterations: ~a. Optimum found ~a.~%"
                   (second results) (third results))

           (setf best-solution-exhaustive (first results))

           (if best-solution-exhaustive
               (then
                 (format t "Best value through: ~a~%"
                         (cost best-solution-exhaustive))
                 (format t "Best neighbor:~%")
                 (pp-solution best-solution-exhaustive t)

                 (simulate-solution best-solution-exhaustive p1 cvrp-action)

                 (setf best-solution-exhaustive (first results))
                 (format t "Best value through Yoel's: ~a~%"
                         (get-cost-from-action cvrp-action))
                 )

               (else
                 (format t "Initial solution was optimum!~%")))))
           ;; time

           ;; (format t "distance-stack: ~a~%"
           ;;         (length (delta-distance-stack action)))

           )

(let* ((p1 ff-a-n33-k6-problem)
       (s1 (make-initial-solution-for-finite-fleet-cvrp-random
            p1))
            (best-solution-exhaustive nil)
            (action (delta-cvrp-action*))
            (action-for-shake (delta-cvrp-action))
            (results nil)
            (max-iterations 200)
            (cvrp-action (basic-cvrp-action))
            )


           (bformat t "Testing DNS with a-n65-k9-problem")

           ;; first we compute the cost of the solution
           (simulate-solution s1 p1 cvrp-action)
           (setf (cost s1) (+ (total-distance cvrp-action)
                              (total-penalty cvrp-action)))

           (format t "Original solution (with cost ~a):~%"
                   (cost s1))
           (pp-solution s1 t)

           (time
            (progn
              (setf results (vns-shake-smart
                          p1 s1
                          (list
                           rabs*              ;0
                           rarbs*             ;1
                           raracs*            ;2
                           refs*              ;3
                           reregs*            ;4
                           ;; rehfs*             ;5
                           rerfs*             ;6
                           ;; rads*              ;7
                           ;; rehrfs*            ;8
                           ;; rerehgs*           ;9
                           ;; rehregs*           ;10
                           ;; rehrehgs*          ;11
                           )
                          :max-iter max-iterations
                          :action action
                          :action-for-shake action-for-shake
                          :selection-strategy
                          ;; (random-improvement-smart 0.7)
                           *first-improvement*
                          :search-strategy *exhaustive-search-strategy*
                          :inner-search-max-iter 50))

           (format t "finished vns-shake~%")


           (format t "Iterations: ~a. Optimum found ~a.~%"
                   (second results) (third results))

           (setf best-solution-exhaustive (first results))

           (if best-solution-exhaustive
               (then
                 (format t "Best value through: ~a~%"
                         (cost best-solution-exhaustive))
                 (format t "Best neighbor:~%")
                 (pp-solution best-solution-exhaustive t)

                 (simulate-solution best-solution-exhaustive p1 cvrp-action)

                 (setf best-solution-exhaustive (first results))
                 (format t "Best value through Yoel's: ~a~%"
                         (get-cost-from-action cvrp-action))
                 )

               (else
                 (format t "Initial solution was optimum!~%")))))
           ;; time

           ;; (format t "distance-stack: ~a~%"
           ;;         (length (delta-distance-stack action)))

           )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing VNS shake no output with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart-no-output
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 40))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))

      )
