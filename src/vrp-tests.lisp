(let* ((st1 (exhaustive-neighborhood-search-strategy)))
  (bformat t "Testing exhaustive-neighborhood-search-strategy...")
  (format t "exhaustive-neighborhood-search-strategy: ~a~%" st1))

(let* ((st1 (random-neighborhood-search-strategy))
       (st2 (random-neighborhood-search-strategy 100))
       (st3 (random-neighborhood-search-strategy 200))
       (st4 (clone st1)))
  (bformat t "Testing random-neighborhood-search-strategy...")
  ;; printing the strategies
  (loop for s in (list st1 st2 st3)
        doing (format t "exhaustive-neighborhood-search-strategy: ~a~%" s))

  (check-t (obj= st1 st1))
  (check-t (obj= st1 st2))
  (check-t (obj= st1 st4))

  (check-nil (obj= st1 st3))
  (check-nil (obj= st2 st3))

  )

(let* ((st1 (jump-around-search-strategy))
       (st2 (jump-around-search-strategy 100))
       (st3 (jump-around-search-strategy 200))
       (st4 (clone st1)))
  (bformat t "Testing jump-around-strategy...")
  ;; printing the strategies
  (loop for s in (list st1 st2 st3)
        doing (format t "exhaustive-neighborhood-search-strategy: ~a~%" s))

  (check-t (obj= st1 st1))
  (check-t (obj= st1 st2))
  (check-t (obj= st1 st4))

  (check-nil (obj= st1 st3))
  (check-nil (obj= st2 st3))

  )

(let* ((*vrp-unit-testing-display-output* t)
       (*vrp-unit-testing-display-results* t)
       (bp1 (basic-strategy-blueprint))
       (bp2 (basic-strategy-blueprint
             :initializations-inside-the-let
             `((wc (basic-working-copy s1)))))
       (bp3 (basic-strategy-blueprint
             :initializations-outside-the-let
             `((prepare-solution-for-neighborhood-exploration wc))))
       (bp4 (basic-strategy-blueprint
             :code-inside-the-macros
             `((pp-solution wc t))))
       (bp5 (basic-strategy-blueprint
             :code-outside-the-macros
             `((format t "Hess"))))
       (bp6 (basic-strategy-blueprint
             :return-code
             `(wc)))
       (bp8 (basic-strategy-blueprint
             :macro-headings `((doselect-route (r1 wc)))))
       (bp7 (clone bp1)))


  (bformat t "Testing basic-blueprint...")

  (check-t  (obj= bp1 bp1))
  (check-t  (obj= bp1 bp7))

  (check-nil (obj= bp1 bp2))
  (check-nil (obj= bp1 bp3))
  (check-nil (obj= bp1 bp4))
  (check-nil (obj= bp1 bp5))
  (check-nil (obj= bp1 bp6))
  (check-nil (obj= bp1 bp8))

  )
