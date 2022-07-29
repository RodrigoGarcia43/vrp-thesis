(in-package :vrp)

(defun mean-technique (neighborhood result-table region-id-list file-name)
  ;; the next line of code is added in order to avoid a warning
  ;; message since file-name parameter is not used in the function
  (declare (ignore file-name))
  ;; (format t "Mean technique~%")
  (mapcar #'(lambda (tuple) (cdr tuple))
	  (sort
	   (loop for id in region-id-list
	      collect
		(let* ((key (nth (1- id) (region-keys neighborhood)))
		       (lst (gethash key result-table)))
		  (cons (/ (apply #'+ lst) (length lst))			  
			id)))
	   #'< :key #'car)))

(defun regression-tree-technique (neighborhood result-table region-id-list file-name)
  ;; (format t "Regression tree technique~%")
  (let* ((path (make-pathname :name file-name
			      :type "csv"))
	 (str (open path :direction :output
		    :if-exists :overwrite
		    :if-does-not-exist :create)))
    ;; first write the head row in the csv file
    (write-csv-row (append
		    (loop for i from 1 to (length (first (region-keys neighborhood)))
		       collecting
			 (format nil "Factor~A" i))
		    (list "D.Cost"))
		   :stream str)

    ;; create the .csv file
    (loop for reg-id in region-id-list
       doing
	 (let* ((key (nth (1- reg-id) (region-keys neighborhood)))
		(d-cost-list (gethash key result-table)))
	   (loop for d-cost in d-cost-list
	      doing
	      ;; write delta cost for current solution in the csv file
		(write-csv-row (append key (list d-cost))
			       :stream str))))
    ;; here we close the data.csv file since it'll be used in the statistical technique
    (close str)

    (let* ((num-cols (1+ (length (first (region-keys neighborhood)))))
	   (file-name (concatenate 'string file-name ".csv"))
	   (data-set (clml.hjs.read-data:read-data-from-file file-name :type :csv
							     :csv-type-spec
							     (loop for i below num-cols collect 'integer)))
	   (regression-tree (clml.decision-tree.decision-tree:make-regression-tree data-set "D.Cost"))
	   (queries (loop for id in region-id-list
		       collect
			 (cons id (eval 
				   (append
				    '(vector)
				    (nth (1- id) (region-keys neighborhood))
				    (list "?"))))))
	   ;; predict D.Cost for all non-exhausted regions
	   (predictions (loop for (id . query) in queries
			   collect
			     (cons (clml.decision-tree.decision-tree:predict-regression-tree query
											     data-set
											     regression-tree)
				   id))))

      ;; delete the data.csv file
      (delete-file file-name)
      ;; sort regions by the predicted value and return regions id order
      (mapcar #'(lambda (tuple) (cdr tuple))
	      (sort predictions #'< :key #'car)))))

(defun regression-forest-technique (neighborhood result-table region-id-list file-name)
  (format t "Regression forest technique~%")
  (let* ((path (make-pathname :name file-name
			      :type "csv"))
	 (str (open path :direction :output
		    :if-exists :overwrite
		    :if-does-not-exist :create)))
    ;; first write the head row in the csv file
    (write-csv-row (append
		    (loop for i from 1 to (length (first (region-keys neighborhood)))
		       collecting
			 (format nil "Factor~A" i))
		    (list "D.Cost"))
		   :stream str)
    ;; create the .csv file
    (loop for reg-id in region-id-list
       doing
	 (let* ((key (nth (1- reg-id) (region-keys neighborhood)))
		(d-cost-list (gethash key result-table)))
	   (loop for d-cost in d-cost-list
	      doing
	      ;; write delta cost for current solution in the csv file
		(write-csv-row (append key (list d-cost))
			       :stream str))))
    ;; here we close the data.csv file since it'll be used in the statistical technique
    (close str)

    (let* ((num-cols (1+ (length (first (region-keys neighborhood)))))
	   (file-name (concatenate 'string file-name ".csv"))
	   (data-set (clml.hjs.read-data:read-data-from-file file-name :type :csv
							     :csv-type-spec
							     (loop for i below num-cols collect 'integer)))
	   (regression-tree (clml.decision-tree.random-forest:make-regression-forest data-set "D.Cost"))
	   (queries (loop for id in region-id-list
		       collect
			 (cons id (eval 
				   (append
				    '(vector)
				    (nth (1- id) (region-keys neighborhood))
				    (list "?"))))))
	   ;; predict D.Cost for all non-exhausted regions
	   (predictions (loop for (id . query) in queries
			   collect
			     (cons (clml.decision-tree.random-forest:predict-regression-forest query
											       data-set
											       regression-tree)
				   id))))

      ;; sort regions by the predicted value and return regions id order
      (mapcar #'(lambda (tuple) (cdr tuple))
	      (sort predictions #'< :key #'car)))))
