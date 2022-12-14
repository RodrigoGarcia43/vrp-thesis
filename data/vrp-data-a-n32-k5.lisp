(in-package :vrp)

(defparameter a-n32-k5-coords
  '((82 76) (96 44) (50  5) (49  8) (13  7) (29 89) (58 30)
    (84 39) (14 24) ( 2 39) ( 3 82) ( 5 10) (98 52) (84 25)
    (61 59) ( 1 65) (88 51) (91  2) (19 32) (93  3) (50 93)
    (98 14) ( 5 42) (42  9) (61 62) ( 9 97) (80 55) (57 69)
    (23 15) (20 70) (85 60) (98  5)))

(defparameter a-n32-k5-distance-matrix
 (make-distance-matrix a-n32-k5-coords))

(defparameter a-n32-k5-demands
  '(19 21  6 19  7 12 16  6 16  8 14 21 16  3 22
     18 19  1 24  8 12  4  8 24 24  2 20 15  2 14  9))

(defparameter a-n32-k5-problem
  (make-basic-cvrp-problem a-n32-k5-distance-matrix
                           a-n32-k5-demands
                           100
                           1))
