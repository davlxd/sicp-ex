(define (pascal-triangle n)
  (define (generate-new-line current-value prev-line new-line)
    (if (= 0 (length prev-line))
      (append new-line (list current-value))
      (generate-new-line 
	(first prev-line)
	(list-tail prev-line 1)
	(append new-line (list(+ current-value (first prev-line)))))))

  (define (iter current-line counter)
    (write-line current-line)
    (if (= counter n) 
      current-line
      (iter (generate-new-line 0 current-line (list)) (+ 1 counter))))
  (iter (list 1) 1))


(pascal-triangle 10)




