(in-package :vrp)

(def-vrp-class has-id ()
  ((id))
  :documentation "A basic class with only one id.")

(def-vrp-class has-name ()
  ((name))
  :documentation "A basic class with only one name.")

(def-vrp-class has-coordinates ()
  ((x-coord) (y-coord))
  :documentation "A class that has geographic coordinates.")

(def-vrp-class has-cost ()
  ((cost))
  :documentation "A class with a cost.")

(def-vrp-class has-clients ()
  ((clients))
  :documentation "A set of clients.")

(def-vrp-class has-vehicles ()
  ((vehicles))
  :documentation "A class to represent things that have vehicles.")

(def-vrp-class has-infinite-fleet ()
  ()
  :documentation "Represents that we can have inifinitely many routes."
  ;; no constructor because it as an abstract class
  ;; no print-object because it as an abstract class
  ;; no obj= or clone because it as an abstract class
  )

(def-vrp-class has-one-depot ()
  ((depot))
  :documentation "Represents that routes have only one depot."
  ;; no constructor because it as an abstract class
  ;; no print-object because it as an abstract class
  ;; no obj= or clone because it as an abstract class
  )

(def-vrp-class has-multi-depots ()
  ((depots))
  :documentation "Represents that routes have only one depot."
  ;; no constructor because it as an abstract class
  ;; no print-object because it as an abstract class
  ;; no obj= or clone because it as an abstract class
  )

(def-vrp-class has-an-end-depot (has-one-depot)
  ((end-depot))
  :documentation "Represents that routes have an end depot."
  ;; no constructor because it as an abstract class
  ;; no print-object because it as an abstract class
  ;; no obj= or clone because it as an abstract class
  )

(defmethod end-depot ((obj has-one-depot))
  "If we ask a one-depot instance for it's end-depot, we just return 'the' ddepot."
  (depot obj))

(def-vrp-class basic-product (has-id has-name)
  ()
  :documentation "A basic product with one id and a name."
  :constructor (basic-product (id name))
  :print-object-string ("<p~a: ~a>" id name)
  :slots-for-obj= (id name)
  :slots-for-clone (id name))

(def-vrp-class basic-client (has-id)
  ()
  :documentation "A basic client with only one id."
  :constructor (basic-client (id))
  :print-object-string ("<c:~a>" id)
  :slots-for-obj= (id)
  :slots-for-clone (id))

(def-vrp-class demand-client (basic-client)
  ((demand))
  :documentation "A client with a demand that must be satisfied.")

(def-vrp-class limited-client (demand-client)
  ()
  :documentation "")

(def-vrp-class basic-cvrp-client (demand-client)
  ()
  :documentation "A basic client for the CVRP with id and demand."
  :constructor (basic-cvrp-client (id demand))
  :print-object-string ("<c~a: ~a>" id demand)
  :slots-for-obj= (id demand)
  :slots-for-clone (id demand))

(def-vrp-class size-limited-cvrp-client (limited-client)
  ()
  :documentation "A size-limited client for the CVRP with id and demand and route-limit."
  :constructor (size-limited-cvrp-client (id demand))
  :print-object-string ("<c~a: ~a>" id demand)
  :slots-for-obj= (id demand)
  :slots-for-clone (id demand))

(def-vrp-class multi-product-demand-client ()
  ((products
    :initform nil
    :documentation "A list with all the demanded products.")
   (products-demand :initform (make-hash-table)
    :documentation "A hash table with each product demand. In this implementation, the keys are the id of the products." ))
  :documentation "A client with a demand for several products.")

(def-vrp-class basic-cupet-client (basic-client
                                   multi-product-demand-client)
  ()
  :documentation "A basic client for the CUPET problem."
  :constructor (basic-cupet-client (id &optional (products nil)))
  :print-object-string ("<cupet c~a>" id )
  :slots-for-obj= (id products products-demand)
  :slots-for-clone (id products products-demand))

(def-vrp-class basic-vehicle (has-id)
  ()
  :documentation "A basic vehicle with only one id."
  :constructor (basic-vehicle (id))
  :print-object-string ("<v:~a>" id)
  :slots-for-obj= (id)
  :slots-for-clone (id))

(def-vrp-class cargo-vehicle ()
   ((cargo))
   :documentation "A vehicle with a current cargo slot.")

(def-vrp-class capacity-vehicle ()
   ((capacity))
   :documentation "A vehicle with a max capacity.")

(def-vrp-class multi-product-vehicle ()
   ((products
     :documentation "The products that the vehicle can carry."))
   :documentation "A vehicle that can carry several products.")

(def-vrp-class multi-compartment-vehicle ()
   ((number-of-compartments
     :documentation "The number of compartments in the vehicle.")
    (compartments-capacity
     :initform (make-hash-table)
     :documentation "A hash table with the capacity of each compartment."))
   :documentation "A vehicle with several compartments.")

(def-vrp-class clients-constrained-vehicle ()
   ((compatible-clients
     :documentation "A list with the compatible clients."))
   :documentation "A vehicle that can only visit some of the clients.")

(def-vrp-class unload-vehicle ()
   ()
   :documentation "A vehicle with an unload operation.")

(def-vrp-class unconditionally-unload-vehicle ()
   ()
   :documentation "A vehicle that always unloads all the client's demand.")

(def-vrp-class cvrp-vehicle (basic-vehicle
                             cargo-vehicle
                             unconditionally-unload-vehicle
                             unload-vehicle
                             capacity-vehicle)
   ()
   :documentation "A vehicle for the CVRP."
   :constructor (cvrp-vehicle (id capacity &optional (cargo 0)))
   :print-object-string ("<cv:~a. ~a/~a>" id cargo capacity)
   :slots-for-obj= (id capacity cargo)
   :slots-for-clone (id capacity cargo))

(def-vrp-class basic-cupet-vehicle (basic-vehicle
                                    multi-compartment-vehicle
                                    multi-product-vehicle
                                    clients-constrained-vehicle)
   ()
   :documentation "A basic vehicle for CUPET's problem."
   :constructor (basic-cupet-vehicle (id
                                      number-of-compartments
                                      &key
                                      (products nil)
                                      (compatible-clients nil)
                                      ))
   :print-object-string ("<cupet_v~a: ~a>"
                         id
                         number-of-compartments)
   :slots-for-obj= (id
                    number-of-compartments
                    compatible-clients
                    products
                    compartments-capacity)
   :slots-for-clone (id
                    number-of-compartments
                    compatible-clients
                    products
                    compartments-capacity))

(def-vrp-class basic-depot (has-id)
  ((id :initform 0))
  :documentation "A basic depot with only one id."
  :constructor (basic-depot (&optional (id 0)))
  :print-object-string ("<d:~a>" id)
  :slots-for-obj= (id)
  :slots-for-clone (id))

(def-vrp-class geographic-depot (basic-depot has-coordinates)
  ()
  :documentation "A basic depot with coordinates."
  :constructor (g-depot (id x-coord y-coord))
  :print-object-string ("<gd:~a>" id)
  :slots-for-obj= (id x-coord y-coord)
  :slots-for-clone (id x-coord y-coord))

(def-vrp-class basic-route (has-id
                            has-clients
                            has-one-depot)
  ((vehicle))
  :documentation "A basic route with clients, vehicle and one depot."
  :constructor (basic-route (&key id vehicle depot clients))
  :print-object-string ("<r~a: ~a (~a: ~a)>"
                        id vehicle depot clients)
  :slots-for-obj= (id vehicle depot clients)
  :slots-for-clone (id vehicle depot clients))

(def-vrp-class route-for-simulation (basic-route)
  ((previous-client))
  :documentation "A route with a previous-client slot.  Good for simulations."
  :constructor (route-for-simulation (&key id vehicle depot clients (previous-client nil)))
  :slots-for-obj= (id vehicle depot clients previous-client)
  :slots-for-clone (id vehicle depot clients previous-client))

(def-vrp-class route-for-simulation-with-end-depot
    (route-for-simulation
     has-an-end-depot)
  ()
  :documentation "A route for simulation with an end-depot."
  :constructor (route-for-simulation-with-end-depot
                (&key id vehicle depot end-depot clients))
  :print-object-string ("<r~a: ~a (~a: ~a) ~a>"
                        id vehicle depot clients end-depot)
  :slots-for-obj= (id vehicle depot end-depot clients previous-client)
  :slots-for-clone (id vehicle depot end-depot clients previous-client))

(def-vrp-class basic-solution (has-id
                               has-cost)
  ((routes))
  :documentation "A basic solution with a cost and a set of routes."
  :constructor (basic-solution (&key id routes (cost 0)))
  :print-object-string ("S~a: (~a)~%routes:~%~{  ~a~%~}" id cost routes)
  :slots-for-obj= (id routes cost)
  :slots-for-clone (id routes cost))

(def-vrp-class basic-cvrp-solution
    (basic-solution
     has-infinite-fleet)
  ()
  :documentation "The solution we should use in the CVRP."
  :constructor (basic-cvrp-solution (&key id routes (cost 0)))
  :print-object-string ("CVRP_S~a: (~a)~%~{  ~a~%~}" id cost routes)
  :slots-for-obj= (id routes cost)
  :slots-for-clone (id routes cost)
  )

(def-vrp-class basic-problem (has-id
                              has-clients
                              has-one-depot)
  ()
  :documentation "A basic problem with clients and a depot."
  :constructor (basic-problem (&key id clients depot))
  :print-object-string ("<P~a.~a, ~a>" id clients depot)
  :slots-for-obj= (id clients depot)
  :slots-for-clone (id clients depot))

(def-vrp-class distance-problem ()
  ((distance-matrix))
  :documentation "A problem with a distance matrix.")

(def-vrp-class capacity-problem ()
  ;; the slots
  ((capacity))
  :documentation "A problem with a capacity for all the vehicles.")

(def-vrp-class finite-fleet-problem ()
  ;; the slots
  ((fleet))
  :documentation "A problem with a finite fleet of vehicles.")

(def-vrp-class several-products-problem ()
  ;; the slots
  ((products))
  :documentation "A problem with several products that should be delivered to the clients.")

(def-vrp-class cupet-problem (basic-problem
                              distance-problem
                              several-products-problem
                              has-vehicles)
  ()
  :documentation "A distance-problem with several products to be delivered to the clients."
  :constructor (cupet-problem (&key id clients depot distance-matrix products vehicles))
  :slots-for-obj= (id clients depot distance-matrix products vehicles)
  :slots-for-clone (id clients depot distance-matrix products vehicles))

 (defmethod print-object ((obj cupet-problem) stream)
   (format stream "<CUPET_problem ~a: ~a clients, ~a products, ~a vehicles>"
           (id obj)
           (length (clients obj))
           (length (products obj))
           (length (vehicles obj))))

(def-vrp-class cvrp-problem (basic-problem
                             distance-problem
                             capacity-problem)
  ()
  :documentation "A distance-problem with a capacity for all the vehicles."
  :constructor (cvrp-problem (&key id clients depot distance-matrix capacity))
  :slots-for-obj= (id clients depot distance-matrix capacity)
  :slots-for-clone (id clients depot distance-matrix capacity))

 (defmethod print-object ((obj cvrp-problem) stream)
   (format stream "<CVRP ~a: ~a clients, capacity ~a>"
           (id obj)
           (length (clients obj))
           (capacity obj)))

(def-vrp-class finite-fleet-cvrp-problem
    ;; inherits from
    (basic-problem
     distance-problem
     finite-fleet-problem)
  ()
  :documentation "A distance-problem with a finite fleet of vehicles."
  :constructor (finite-fleet-cvrp-problem (&key id clients depot distance-matrix fleet))
  :slots-for-obj= (id clients depot distance-matrix fleet)
  :slots-for-clone (id clients depot distance-matrix fleet))

 (defmethod print-object ((obj finite-fleet-cvrp-problem) stream)
   (format stream "<FFCVRP ~a: ~a clients, fleet ~a>"
           (id obj)
           (length (clients obj))
           (fleet obj)))

(def-vrp-class finite-fleet-end-depot-cvrp-problem
    ;; inherits from
    (basic-problem
     distance-problem
     finite-fleet-problem
     has-an-end-depot)
  ()
  :documentation "A distance-problem with a finite fleet of vehicles and an end depot."
  :constructor (finite-fleet-end-depot-cvrp-problem
                (&key id clients depot end-depot distance-matrix fleet))
  :slots-for-obj= (id clients depot end-depot distance-matrix fleet)
  :slots-for-clone (id clients depot end-depot distance-matrix fleet))

(defmethod print-object ((obj finite-fleet-end-depot-cvrp-problem) stream)
   (format stream "<FFCVRP2D ~a: ~a clients, fleet ~a, depots ~a, ~a>"
           (id obj)
           (length (clients obj))
           (fleet obj)
           (depot obj)
           (end-depot obj)))

(def-vrp-class route-distance-action (has-id)
  ((current-distance)
   (id :documentation "The id of the route this action is being used on."))
  :documentation "An action to compute the distance travelled by a vehicle in a route."
  :constructor (route-distance-action (id &optional (current-distance 0)))
  :print-object-string ("<a_rd: ~a ~a>" id current-distance)
  :slots-for-obj= (id current-distance)
  :slots-for-clone (id current-distance))

(def-vrp-class basic-vehicle-capacity-action (has-id)
  ((capacity-violation)
   (id :documentation "The id of the route this action is being used on."))
  :documentation "An action to compute the capacity infeasibility in a route."
  :constructor (basic-vehicle-capacity-action (id &optional (capacity-violation 0)))
  :print-object-string ("<a_bvc: ~a ~a>" id capacity-violation)
  :slots-for-obj= (id capacity-violation)
  :slots-for-clone (id capacity-violation))

(def-vrp-class simulate-load-action ()
  ()
  :documentation "An action that will allow the simulation of the load operations."
  :constructor (simulate-load-action ())
  :print-object-string ("<a_sl>"))

(def-vrp-class basic-solution-distance-action (route-distance-action)
  ((total-distance))
  :documentation "An action that will be used to compute the distance travelled by all the vehicles in a solution."
  :constructor (basic-solution-distance-action
                (&optional (total-distance 0) (id 1)))
  :print-object-string ("<a_bsd: ~a>" total-distance)
  :slots-for-obj= (id current-distance total-distance)
  :slots-for-clone (id current-distance total-distance))

(def-vrp-class basic-penalty-action ()
  ((total-penalty
    :documentation "The total penalty due to this action.")
   (penalty-factor
    :documentation "The factor that we should multiply by the infeasibility measure."))
  :documentation "An base action to implement a basic penalty strategy.")

(def-vrp-class basic-capacity-penalty-action
    (basic-vehicle-capacity-action
     basic-penalty-action)
  ()
  :constructor (basic-capacity-penalty-action
                (&key (id 1)
                      (penalty-factor 1)
                      (total-penalty 0)
                      (capacity-violation 0)))
  :print-object-string ("<a_bcp~a: ~a>" id total-penalty)
  :slots-for-obj= (id capacity-violation penalty-factor total-penalty)
  :slots-for-clone (id capacity-violation penalty-factor total-penalty)
  :documentation "An action to implement a penalty strategy for the capacity violation.")

(def-vrp-class basic-cvrp-action
    (basic-solution-distance-action
     simulate-load-action
     basic-capacity-penalty-action)
  ()
  :constructor (basic-cvrp-action
                (&key (id 1)
                      (penalty-factor 1000)
                      (total-penalty 0)
                      (capacity-violation 0)
                      (current-distance 0)
                      (total-distance 0)))
  :print-object-string ("<a_cvrp~a. d: ~a, p: ~a>"
                        id total-distance total-penalty)
  :slots-for-obj= (id
                   penalty-factor
                   total-penalty
                   capacity-violation
                   current-distance
                   total-distance)
  :slots-for-clone (id
                   penalty-factor
                   total-penalty
                   capacity-violation
                   current-distance
                   total-distance)
  :documentation "An action to simulate a basic CVRP solution.")

(def-vrp-class delta-distance-action (has-id)
  ((delta-distance :initform 0))
  :documentation "This actions computes the difference in the distances of a solution after a neighborhood operation  has been made."
  :constructor (delta-distance-action
                (&key (delta-distance 0) (id 1)))
  :print-object-string ("<da_d ~a: ~a>"
                        id delta-distance)
  :slots-for-obj= (id delta-distance)
  :slots-for-clone (id delta-distance))

(def-vrp-class delta-basic-capacity-action (has-id)
  ((delta-routes-feasibility :initform nil
      :documentation "An array with the available basic-capacity in the vehicles on each route.  The available basic-capacity of a route is the capacity of the vehicle minus the sum of the demand of the clients.  In other words is how much cargo the vehicle could carry."))
  :documentation "A class to represent an action to compute the difference of vehicle-capacity in a solution after an operation (selection or insertion) has been made."
  ;; no constructor (I'll write it by hand)
  :print-object-string ("<da_bc: ~a: ~a>"
                        id delta-routes-feasibility)
  :slots-for-obj= (id delta-routes-feasibility)
  :slots-for-clone (id delta-routes-feasibility))

;; this is the code for the constructor
(defun delta-basic-capacity-action (number-of-routes
                                    &key (id 1)
                                      routes-feasibility)
  (make-instance
   'delta-basic-capacity-action
   :id id
   :delta-routes-feasibility (aif routes-feasibility it
                                  (make-array (1+ number-of-routes)
                                              :initial-element 0))))

(def-vrp-class delta-basic-capacity-penalty-action
    (basic-penalty-action
     delta-basic-capacity-action)
  ;; slots
  ((original-routes-feasibility
    :documentation "A slot to record the original routes-feasibility in the solution."))
  :documentation "A class to represent an action that penalizes the basic-capacity unfeasibility."
  :constructor (delta-basic-capacity-penalty-action
                (&key (penalty-factor 1000)
                      (total-penalty 0)
                      (original-routes-feasibility)
                      (delta-routes-feasibility)
                      (id 1)))
  :print-object-string ("<da_bcp ~a: total: ~a: ~a>"
                        id
                        total-penalty
                        delta-routes-feasibility)
  :slots-for-obj= (id
                   penalty-factor
                   total-penalty
                   delta-routes-feasibility
                   original-routes-feasibility)
  :slots-for-clone (id
                   penalty-factor
                   total-penalty
                   delta-routes-feasibility
                   original-routes-feasibility))

(def-vrp-class delta-basic-cvrp-action
    (delta-distance-action
     delta-basic-capacity-penalty-action)
  ()
  :documentation "A class to represent an action to compute the difference of vehicle-capacity in a solution after an operation (selection or insertion) has been made."
  :constructor (delta-cvrp-action
                (&key (delta-distance 0)
                      (delta-routes-feasibility)
                      (penalty-factor 1000)
                      (original-routes-feasibility nil)
                      (total-penalty 0)
                      (id 1)))
  :print-object-string ("<da_cvrp ~a: D: ~a. P: ~a>"
                        id
                        delta-distance
                        total-penalty)
  :slots-for-obj= (id
                   delta-distance
                   delta-routes-feasibility
                   original-routes-feasibility
                   penalty-factor
                   total-penalty)
  :slots-for-clone (id
                   delta-distance
                   delta-routes-feasibility
                   original-routes-feasibility
                   penalty-factor
                   total-penalty))

(def-vrp-class delta-distance-action* (delta-distance-action)
  ((delta-distance-stack :initform nil))
  :documentation "This action computes the difference in the distances of a solution after a neighborhood operation has been made, and adds the functionalities for an undo-delta-cost-computation. It does that by storing each previous result in the stack."
  :constructor (delta-distance-action*
                (&key (delta-distance 0)
                      (id 1)
                      (delta-distance-stack nil)))
  :print-object-string ("<da_d* ~a: ~a>"
                        id delta-distance)
  :slots-for-obj= (id delta-distance delta-distance-stack)
  :slots-for-clone (id delta-distance delta-distance-stack))

(def-vrp-class delta-basic-capacity-action*
    (delta-basic-capacity-action)
  ((delta-routes-feasibility-stack :initform nil
      :documentation "A stack to store the arrays with the available basic-capacity in the vehicles on each route.  The available basic-capacity of a route is the capacity of the vehicle minus the sum of the demand of the clients.  In other words is how much cargo the vehicle could carry."))
  :documentation "A class to represent an action* to compute the difference of vehicle-capacity in a solution after an operation (selection or insertion) has been made."
  ;; no constructor (I'll write it by hand)
  :print-object-string ("<da_bc*: ~a: ~a>"
                        id delta-routes-feasibility)
  :slots-for-obj= (id
                   delta-routes-feasibility
                   delta-routes-feasibility-stack)
  :slots-for-clone (id
                   delta-routes-feasibility
                   delta-routes-feasibility-stack))

;; this is the code for the constructor
(defun delta-basic-capacity-action* (number-of-routes
                                     &key (id 1)
                                       routes-feasibility
                                       delta-routes-feasibility-stack)
  (make-instance
   'delta-basic-capacity-action*
   :id id
   :delta-routes-feasibility (aif routes-feasibility it
                                  (make-array (1+ number-of-routes)
                                              :initial-element 0))
   :delta-routes-feasibility-stack delta-routes-feasibility-stack))

(def-vrp-class basic-penalty-action* (basic-penalty-action)
  ((total-penalty-stack
    :documentation "A stack with the previous total penalty due to this action."))
  :documentation "An base action to implement a basic penalty strategy with undo-delta-cost-computation.")

(def-vrp-class delta-basic-capacity-penalty-action*
    (basic-penalty-action*
     delta-basic-capacity-action*)
  ;; slots
  ((original-routes-feasibility
    :documentation "A slot to record the original routes-feasibility in the solution."))
  :documentation "A class to represent an action that penalizes the basic-capacity unfeasibility."
  :constructor (delta-basic-capacity-penalty-action*
                (&key (penalty-factor 1000)
                      (total-penalty 0)
                      (original-routes-feasibility)
                      (delta-routes-feasibility)
                      (delta-routes-feasibility-stack nil)
                      (total-penalty-stack nil)
                      (id 1)))
  :print-object-string ("<da_bcp* ~a: total: ~a: ~a>"
                        id
                        total-penalty
                        delta-routes-feasibility)
  :slots-for-obj= (id
                   penalty-factor
                   total-penalty
                   delta-routes-feasibility
                   original-routes-feasibility
                   delta-routes-feasibility-stack
                   total-penalty-stack)
  :slots-for-clone (id
                   penalty-factor
                   total-penalty
                   delta-routes-feasibility
                   original-routes-feasibility
                   delta-routes-feasibility-stack
                   total-penalty-stack))

(def-vrp-class delta-basic-cvrp-action*
    (delta-distance-action*
     delta-basic-capacity-penalty-action*)
  ()
  :documentation "A class to represent an action to compute the difference of vehicle-capacity in a solution after an operation (selection or insertion) has been made and can undo all those computations."
  :constructor (delta-cvrp-action*
                (&key (delta-distance 0)
                      (delta-routes-feasibility)
                      (penalty-factor 1000)
                      (original-routes-feasibility nil)
                      (total-penalty 0)
                      (delta-distance-stack nil)
                      (delta-routes-feasibility-stack nil)
                      (total-penalty-stack nil)
                      (id 1)))
  :print-object-string ("<da_cvrp* ~a: D: ~a. P: ~a>"
                        id
                        delta-distance
                        total-penalty)
  :slots-for-obj= (id
                   delta-distance
                   delta-routes-feasibility
                   original-routes-feasibility
                   penalty-factor
                   total-penalty
                   delta-distance-stack
                   delta-routes-feasibility-stack
                   total-penalty-stack)

  :slots-for-clone (id
                   delta-distance
                   delta-routes-feasibility
                   original-routes-feasibility
                   penalty-factor
                   total-penalty
                   delta-distance-stack
                   delta-routes-feasibility-stack
                   total-penalty-stack))

(def-vrp-class neighborhood-operation ()
  (;; the slots
   (route
    :documentation "The route where the operation will take place.")
   (pos
    :documentation "The position in the route where the operation will take place.")
   (operand
    :documentation "The result of the operation: a client or a set of clients that was selected or that should be inserted.  Actually I think that it is the index in the *selected-clients-array* where the selected client was stored or the position in the array of the client that should be inserted."))
  ;; the rest of the class elements
  :documentation "A base class for a neighborhood operation."
  ;; we don't want a constructor because this is an abstract class
  ;; we don't want either a print-object method
  ;; we leave slots for obj= and clone because maybe they are
  ;; used to polymorphically compute something
  :slots-for-obj= (route pos operand)
  :slots-for-clone (route pos operand))

(def-vrp-class operation-on-route ()
  (;; the slots
   (route
    :documentation "The route where the operation will take place.")
   (operand
    :documentation "It is the index in the *selected-clients-array* where the selected client was stored or the position in the array of the client that should be inserted."))
  ;; the rest of the class elements
  :documentation "A base class for all operations on a route."
  ;; we don't want a constructor because this is an abstract class
  ;; we don't want either a print-object method
  ;; we leave slots for obj= and clone because maybe they are
  ;; used to polymorphically compute something
  :slots-for-obj= (route operand)
  :slots-for-clone (route operand))

(def-vrp-class operation-with-operand ()
  (;; the slots
   (operand
    :documentation "It is the index to identify the operation."))
  ;; the rest of the class elements
  :documentation "A base class for all operations that have one operand."
  ;; we don't want a constructor because this is an abstract class
  ;; we don't want either a print-object method
  ;; we leave slots for obj= and clone because maybe they are
  ;; used to polymorphically compute something
  :slots-for-obj= (operand)
  :slots-for-clone (operand))

;; A class for the select-client operation
(def-vrp-class operation-select-client (neighborhood-operation)
  ;; the slots are inherited from neighborhood-operation
  ()
  ;; the rest of the class elements
  :documentation "A class to represent the select-client operation in a neighborhood criterion."
  :constructor (op-select-client (route pos &optional operand))
  :print-object-string ("<op:a ~a ~a ~a>" route pos operand)
  ;; the slots-for-clone and for obj are
  ;; inherited from the neighborhood-operation class
  )

;; A class for the insert-client operation
(def-vrp-class operation-insert-client (neighborhood-operation)
  ;; the slots are inherited from neighborhood-operation
  ()
  ;; the rest of the class elements
  :documentation "A class to represent the insert-client operation in a neighborhood criterion."
  :constructor (op-insert-client (route pos operand))
  :print-object-string ("<op:b ~a ~a ~a>" route pos operand)
  ;; the slots-for-clone and for obj are
  ;; inherited from the neighborhood-operation class
  )

;; A class for the insert-client operation
(def-vrp-class basic-working-copy ()
  ;; slots
  ((solution
    :documentation "A solution with the routes of clients and depot, the demands and the vehicles capacity.")
   (actual-position-functions-stack :initform nil
    :documentation  "A stack with all the previously used actual-positions functions.")
   (number-of-routes :initform 0
    :documentation "The number of routes this working-copy has.")
   (initial-routes-lengths :initform nil
    :documentation "An array with the initial route lengths in the working copy of the solution.")
   (routes-lengths :initform nil
    :documentation "An array with the current route lengths in the working copy.")
   (insertions-made :initform nil
    :documentation "A list with all the insertions made in the neighborhood process.  Each element is a list of the form (r,p,c) where r and p are the route and the position where the insertion was made and c is the client that was inserted.")
   (selections-count :initform -1
    :documentation "A variable to count how many select-client operations have been made.  This count is zero based.")
   (selected-clients
    :initform (make-array 20 :adjustable t :fill-pointer t :initial-element nil)
    :documentation "A data structure to store the clients that were selected during a neighborhood simulation.")
   (original-positions
    :initform (make-array 20 :adjustable t :fill-pointer t :initial-element nil)
    :documentation "A data structure to store the order of the clients in the original solution.")
   (selected-subroutes
    :initform nil
    :documentation "A data structure to store info about the selected suborutes in a neighborhood search.")
   (selected-clients-during-apply
    :initform nil
    :documentation "An array with the selected clients during the application of an operation to a solution.")
   )


  ;; the rest of the class elements
  :documentation "A class to represent a data structure for the simulation of operations in a neighborhood exploration."
  :constructor (basic-working-copy (solution))
  :slots-for-obj= (solution
                   initial-routes-lengths
                   number-of-routes
                   routes-lengths
                   insertions-made
                   selections-count
                   selected-clients
                   original-positions
                   selected-subroutes
                   selected-clients-during-apply)
  :slots-for-clone (solution
                    initial-routes-lengths
                    routes-lengths
                    number-of-routes
                    insertions-made
                    selections-count
                    selected-clients
                    original-positions
                    selected-subroutes
                    selected-clients-during-apply))

;;; A function to access the route of the working-copy
(defmethod routes ((working-copy basic-working-copy))
  "This function returns the routes of the solution in the given instance."
  (routes (solution working-copy)))

;;; A function to access the route of the working-copy
(defmethod cost ((working-copy basic-working-copy))
  "This function returns the cost of the solution in the given working-copy."
  (cost (solution working-copy)))

;;; A function to access the route of the working-copy
(defmethod (setf cost) (new-cost (working-copy basic-working-copy))
  "This function setfs the cost of the solution in the given working-copy."
  (setf (cost (solution working-copy)) new-cost))

;;; A function to access the route of the working-copy
(defmethod depot ((working-copy basic-working-copy))
  "This function returns a basic-depot with id 0."
  (basic-depot 0))

;;; A function to access the route of the working-copy
(defmethod end-depot ((working-copy basic-working-copy))
  "This function returns a basic-depot with id 0."
  (depot working-copy))

;; A class for the insert-client operation
(def-vrp-class working-copy-with-inifinite-fleet
    (basic-working-copy
     has-infinite-fleet)
  ;; no slots
  ()
  ;; the rest of the class elements
  :documentation "A working copy with potentially inifinitely many routes."
  :constructor (working-copy-with-infinite-fleet (solution))
  :slots-for-obj= (solution
                   initial-routes-lengths
                   number-of-routes
                   routes-lengths
                   insertions-made
                   selections-count
                   selected-clients
                   original-positions
                   selected-subroutes
                   selected-clients-during-apply)
  :slots-for-clone (solution
                    initial-routes-lengths
                    routes-lengths
                    number-of-routes
                    insertions-made
                    selections-count
                    selected-clients
                    original-positions
                    selected-subroutes
                    selected-clients-during-apply))

(defgeneric make-working-copy (solution)
  (:documentation "This function returns the appropriate working-copy according to the  solution passed as argument."))

(defmethod make-working-copy ((solution basic-solution))
  "Return an instance of basic-working-copy."
  (basic-working-copy solution))

(defmethod make-working-copy ((solution basic-cvrp-solution))
  "Return an instance of working-copy-with-infinite-fleet."
  (working-copy-with-infinite-fleet solution))

(defun get-client-with-id (id problem)
  "Returns the client with the given id in the given problem"
  (loop for c in (clients problem)
        when (= (id c) id) do (return c)))

(defun get-vehicle-with-id (id problem)
  "Returns the vehicle with the given id in the given problem"
  (loop for v in (fleet problem)
        when (= (id v) id) do (return v)))

(defgeneric get-distance-from-to (from to problem)
  (:documentation "Returns the distance from the first point to the second in the given problem."))

(defmethod get-distance-from-to ((from has-id)
                                 (to has-id)
                                 (problem distance-problem))
  "Returns the distance from the first point to the second in the given distance-problem."
  (aref (distance-matrix problem) (id from) (id to)))

(defgeneric product-demand (product client)
  (:documentation "Returns the demand of the given product in the client."))

(defmethod product-demand ((product basic-product)
                           (client multi-product-demand-client))
  "Returns the demand of the product in the given multi-product-demand client."
  (gethash (id product) (products-demand client)))

(defgeneric set-product-demand (product client demand)
  (:documentation "Sets the demand for a given product in a multi-product-demand-client."))

(defgeneric (setf product-demand)
    (new-value product client)
  (:documentation "Sets the demand for a given product in a multi-product-demand-client."))

(defmethod (setf product-demand)
    ((new-value number)
     (product basic-product)
     (client multi-product-demand-client))
  "Sets the demand for a given product in a multi-product-demand-client."
  (setf (gethash (id product) (products-demand client)) new-value))

(defgeneric compartment-capacity (product vehicle)
  (:documentation "Returns the capacity of the given compartment in the vehicle."))

(defmethod compartment-capacity ((compartment basic-product)
                                 (vehicle multi-compartment-vehicle))
  "Returns the demand of the product in the given multi-product-demand client."
  (gethash (id compartment) (compartments-capacity vehicle)))

(defmethod compartment-capacity ((compartment number)
                                 (vehicle multi-compartment-vehicle))
  "Returns the demand of the given compartment in the given multi-product-demand client."
  (gethash compartment (compartments-capacity vehicle)))

(defgeneric (setf compartment-capacity)
    (new-value compartment vehicle)
  (:documentation "Sets the demand of the given compartment in the given multi-product-demand client to the new-value."))

(defmethod (setf compartment-capacity)
    ((new-value number)
     (compartment number)
     (vehicle multi-compartment-vehicle))
  "Sets the demand of the given compartment in the given multi-product-demand client to the new-value."
  (setf (gethash compartment (compartments-capacity vehicle)) new-value))

(defmethod (setf compartment-capacity)
    ((new-value number)
     (compartment basic-product)
     (vehicle multi-compartment-vehicle))
  "Sets the demand of the given compartment in the given multi-product-demand client to the new-value."
  (setf (gethash (id compartment)
                 (compartments-capacity vehicle))
        new-value))

(defgeneric set-compartment-capacity (product vehicle value)
  (:documentation "Sets the capacity of the given compartment in the vehicle to the given value."))

(defmethod set-compartment-capacity
    ((compartment basic-product)
     (vehicle multi-compartment-vehicle)
     (value number))
  "Sets the capacity of the compartment (identified by product) in the given multiple compartment vehicle to the given value."
  (setf (gethash (id compartment) (compartments-capacity vehicle))
        value))

(defmethod compartment-capacity ((compartment number)
                                 (vehicle multi-compartment-vehicle))
  "Returns the demand of the given compartment in the given multi-product-demand client."
  (gethash compartment (compartments-capacity vehicle)))

(defgeneric client-is-compatible-with-vehicle (client vehicle)
  (:documentation "Returns non nil if the vehicle can visit that client."))

(defmethod client-is-compatible-with-vehicle
    ((client basic-client)
     (vehicle clients-constrained-vehicle))
  "Returns non nil if the vehicle can visit that client."
  (member client (compatible-clients vehicle) :test #'obj=))

(defgeneric get-simpler-operations-from (operation working-copy)
  (:documentation "Returns a list with all the (simpler) operations into which the given operation can be decomposed.  The elements in this list are the ones that should be passed to simulate-neighborhood-operation and to the delta-cost-computation methods."))

(defmethod get-simpler-operations-from
    (op (wc basic-working-copy))
  "By default, a neighborhood-operation does not decompose into simpler operations, so this method returns a list with the operation."
  (list op))

(defmacro with-basic-clients ((&rest ids) &body body)
  (let* ((symbols (mapcar (lambda (n) (symb "c" n))
                                  ids))
          (instances (mapcar (lambda (n) `(basic-client ,n))
                                  ids)))
     `(let* (,@(loop for s in symbols
                     for c in instances
                     collecting (list s c)))
        ,@body)))

;;; make-basic-route-from-list function
(defun make-basic-route-from-list (route-id clients-id vehicle-id
                                   &optional (depot-id 0))
  "This functions receives a list with the id of the clients in the route and a number with the vehicle-id.
  For example:
   (make-basic-route-from-list 3 (1 2 3) 2)
 returns the route:
   r3: c1, c2, c3, v2."
  ;; first we create the clients
  (let* ((clients (loop for c in clients-id
                        collecting (basic-client c)))
         ;; the vehicle
         (vehicle (basic-vehicle vehicle-id))
         (depot (basic-depot depot-id)))
    ;; return the route
    (basic-route :id route-id
                 :clients clients
                 :depot depot
                 :vehicle vehicle)))

;;; make-basic-solution-from-list function
(defun make-basic-solution-from-list (sol-id routes)
  "This functions receives a list that represent a basic solution.  The elements of the list are list with numbers.  The numbers represent the id of the clients in the route.  For example:
   ((1 2 3) (4 5) (6 7 8))
 represents the solution:
   S1:
     r1: c1, c2, c3
     r2: c4, c5
     r3: c6, c7, c8
For each route we create a basic vehicle, and for the solution we also create a depot."
  (let* ((depot-id 0)
         (actual-routes (loop for element in routes
                              for id from 1
                              collecting (make-basic-route-from-list
                                          id element id depot-id))))
    ;; return the solution
    (basic-solution :id sol-id :routes actual-routes)))

;;; with-basic-solution
(defmacro with-basic-solution ((solution-name
                                routes
                                &optional (id 1))
                               &body body)

  `(let* ((,solution-name (make-basic-solution-from-list
                           ,id ',routes)))
     ,@body))

;;; make-basic-route-from-list function
(defun make-basic-cvrp-route-from-list
    (route-id clients-id v-id problem)
  "This functions receives a list with the id of the clients in the route and a number with the vehicle-id.  It also receives a cvrp-problem.
  For example:
   (make-basic-route-from-list 3 (1 2 3) 2 p1)
 returns a simulation-route:
   r3: c1, c2, c3, v2.
 where c1, c2, and c3 are taken from the clients in the problem."
  ;; first we create the clients
  (let* ((clients (loop for c in clients-id
                        collect (get-client-with-id c problem)))
         ;; the vehicle
         (vehicle (cvrp-vehicle v-id (capacity problem)))
         (depot (depot problem)))
    ;; return the route
    (route-for-simulation :id route-id
                          :vehicle vehicle
                          :depot depot
                          :clients clients)))

;;; make-basic-cvrp-solution-from-list function
(defun make-basic-cvrp-solution-from-list (sol-id routes problem)
  "This functions receives a list that represent a basic solution.  The elements of the list are list with numbers.  The numbers represent the id of the clients in the route.  For example:
   (1 ((1 2 3) (4 5) (6 7 8)) p1)
 represents the solution:
   S1:
     r1: c1, c2, c3
     r2: c4, c5
     r3: c6, c7, c8
For each route we create a cvrp-vehicle, and for the solution we use the depot specified in the problem."
  (let* ((actual-routes
          (loop for element in routes
                for id from 1
                collect (make-basic-cvrp-route-from-list
                         id element id problem))))
    ;; return the solution
    (basic-cvrp-solution :id sol-id :routes actual-routes)))

;;; with-basic-cvrp-solution
(defmacro with-basic-cvrp-solution ((solution-name
                                routes
                                problem
                                &optional (id 1))
                               &body body)

  `(let* ((,solution-name (make-basic-cvrp-solution-from-list
                           ,id ',routes ,problem)))
     ,@body))

;;; make-basic-route-from-list function
(defun make-finite-fleet-cvrp-route-from-list
    (route-id clients-id v-id problem)
  "This functions receives a list with the id of the clients in the route and a number with the vehicle-id.  It also receives a cvrp-problem.
  For example:
   (make-finite-fleet-route-from-list 3 (1 2 3) 2 p1)
 returns a simulation-route:
   r3: c1, c2, c3, v2.
 where c1, c2, and c3 are taken from the clients in the problem, and v2 should be a vehicle in the problem's fleet with id 2."
  ;; first we create the clients
  (let* ((clients (loop for c in clients-id
                        collect (get-client-with-id c problem)))
         ;; the vehicles
         (vehicle (get-vehicle-with-id v-id problem))
         (depot (depot problem)))
    ;; return the route
    (route-for-simulation :id route-id
                          :vehicle vehicle
                          :depot depot
                          :clients clients)))

;;; make-basic-cvrp-solution-from-list function
(defun make-finite-fleet-cvrp-solution-from-list
    (sol-id routes problem)
  "This functions receives a list that represent a basic solution.  The elements of the list are list with numbers.  The numbers represent the id of the clients in the route, except the first number on each 'route', that represents the id of the vehicle in that route.  For example:
   (1 ((1 1 2 3) (3 4 5) (2 6 7 8)) p1)
 represents the solution:
   S1:
     r1: c1, c2, c3, v1
     r2: c4, c5,     v3
     r3: c6, c7, c8, v2
For the solution we use the depot specified in the problem."
  (let* ((actual-routes
          (loop for element in routes
                for id from 1
                collect (make-finite-fleet-cvrp-route-from-list
                         id (cdr element) (car element) problem))))
    ;; return the solution
    (basic-solution :id sol-id :routes actual-routes)))

;;; with-basic-cvrp-solution
(defmacro with-finite-fleet-cvrp-solution
    ((solution-name
      routes
      problem
      &optional (id 1))
     &body body)

  `(let* ((,solution-name (make-finite-fleet-cvrp-solution-from-list
                           ,id ',routes ,problem)))
     ,@body))

;;; make-basic-route-from-list function
(defun make-finite-fleet-with-end-depot-route-cvrp-from-list
    (route-id clients-id v-id problem)
  "This functions receives a list with the id of the clients in the route and a number with the vehicle-id.  It also receives a cvrp-problem.
  For example:
   (make-finite-fleet-route-from-list 3 (1 2 3) 2 p1)
 returns a simulation-route:
   r3: c1, c2, c3, v2, d0, d1.
 where c1, c2, and c3 are taken from the clients in the problem, v2 should be a vehicle in the problem's fleet with id 2, and d0 and d1 are the problem's depot and end-depot, respectively."
  ;; first we create the clients
  (let* ((clients (loop for c in clients-id
                        collect (get-client-with-id c problem)))
         ;; the vehicles
         (vehicle (get-vehicle-with-id v-id problem))
         (depot (depot problem))
         (end-depot (end-depot problem)))
    ;; return the route
    (route-for-simulation-with-end-depot
     :id route-id
     :vehicle vehicle
     :depot depot
     :end-depot end-depot
     :clients clients)))

;;; make-basic-cvrp-solution-from-list function
(defun make-finite-fleet-with-end-depot-cvrp-solution-from-list
    (sol-id routes problem)
  "This functions receives a list that represent a cvrp solution.  The elements of the list are list with numbers.  The numbers represent the id of the clients in the route, except the first number on each 'route', that represents the id of the vehicle in that route.  For example:
   (1 ((1 1 2 3) (3 4 5) (2 6 7 8)) p1)
 represents the solution:
   S1:
     r1: c1, c2, c3, v1, d0, d1
     r2: c4, c5,     v3, d0, d1
     r3: c6, c7, c8, v2, d0, d1
For each route we use the depot and end-depot specified in the problem."
  (let* ((actual-routes
          (loop for element in routes
                for id from 1
                collect
                (make-finite-fleet-with-end-depot-route-cvrp-from-list
                 id (cdr element) (car element) problem))))
    ;; return the solution
    (basic-solution :id sol-id :routes actual-routes)))

;;; with-basic-cvrp-solution
(defmacro with-finite-fleet-end-depot-cvrp-solution
    ((solution-name
      routes
      problem
      &optional (id 1))
     &body body)

  `(let* ((,solution-name
           (make-finite-fleet-with-end-depot-cvrp-solution-from-list
            ,id ',routes ,problem)))
     ,@body))

(defun make-demand-clients-from-demands (list-with-demands)
  "This function receives a list with demands and returns a list with instances of demand-client where the demand of the i-th client is the demand in the i-th position in the list."
  (loop for demand in list-with-demands
        for id from 1
        collecting (basic-cvrp-client id demand)))

(defun make-cvrp-from-lists (distances demands capacity
                             &optional (id 1))
  "This function receives a list with the distances, a list with the demands and a number with the capacity and returns an instance of a cvrp."
  (let* ((depot (basic-depot 0))
         (n (length demands))
         (clients (make-demand-clients-from-demands demands))
         ;; the dimension is 1+ because of the depot
         (matrix-dimensions (list (1+ n) (1+ n)))
         (d-matrix (make-array matrix-dimensions
                               :initial-contents distances)))
    ;; return the problem with the new data
    (cvrp-problem :id id :clients clients :depot depot
                  :distance-matrix d-matrix :capacity capacity)))

(defmacro with-cvrp-problem ((pname
                              &key distances demands capacity
                              (id 1)) &body body)
  `(let ((,pname (make-cvrp-from-lists ,distances
                                       ,demands
                                       ,capacity
                                       ,id)))
     ,@body))

(defun make-cvrp-vehicles-list-from-capacities (list-with-capacities)
  "This function receives a list with capacities and returns a list with instances of cvrp-vehicles where the capacity of the i-th vehicle is the number in the i-th position in the list."
  (loop for capacity in list-with-capacities
        for id from 1
        collecting (cvrp-vehicle id capacity)))

(defun make-finite-fleet-cvrp-from-lists (distances demands capacities
                             &optional (id 1))
  "This function receives a list with the distances, a list with the demands and a number with the capacity and returns an instance of a finite-fleet-cvrp."
  (let* ((depot (basic-depot 0))
         (n (length demands))
         (clients (make-demand-clients-from-demands demands))
         (vehicles (make-cvrp-vehicles-list-from-capacities capacities))
         ;; the dimension is 1+ because of the depot
         (matrix-dimensions (list (1+ n) (1+ n)))
         (d-matrix (make-array matrix-dimensions
                               :initial-contents distances)))
    ;; return the problem with the new data
    (finite-fleet-cvrp-problem :id id
                               :clients clients
                               :depot depot
                               :distance-matrix d-matrix
                               :fleet vehicles)))

(defmacro with-finite-fleet-cvrp-problem
    ((pname
      &key distances demands capacities
      (id 1)) &body body)
  `(let ((,pname (make-finite-fleet-cvrp-from-lists
                  ,distances
                  ,demands
                  ,capacities
                  ,id)))
     ,@body))

(defun make-finite-fleet-end-depot-cvrp-from-lists
    (distances demands capacities &optional (id 1))
  "This function receives a list with the distances, a list with the demands and a number with the capacity and returns an instance of a finite-fleet-cvrp."
  (let* ((n (length demands))
         (depot     (basic-depot 0))
         (end-depot (basic-depot (1+ n)))
         (clients (make-demand-clients-from-demands demands))
         (vehicles (make-cvrp-vehicles-list-from-capacities capacities))
         ;; the dimension is +2 because of the depot
         (matrix-dimensions (list (+ n 2) (+ n 2)))
         (d-matrix (make-array matrix-dimensions
                               :initial-contents distances)))
    ;; return the problem with the new data
    (finite-fleet-end-depot-cvrp-problem :id id
                                         :clients clients
                                         :depot depot
                                         :end-depot end-depot
                                         :distance-matrix d-matrix
                                         :fleet vehicles)))

(defmacro with-finite-fleet-end-depot-cvrp-problem
    ((pname
      &key distances demands capacities
      (id 1)) &body body)
  `(let ((,pname (make-finite-fleet-end-depot-cvrp-from-lists
                  ,distances
                  ,demands
                  ,capacities
                  ,id)))
     ,@body))
