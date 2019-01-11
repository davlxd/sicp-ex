
(define (last-pair1 l)
  (let ((len (length l)))
    (let ((last-el (list-ref l (- len 1))))
      (list last-el))))

(last-pair1 (list 1 2))
(last-pair1 (list 1))


(define (last-pair2 l)
  (if (= 1 (length l)) 
    (list (car l))
    (last-pair2 (cdr l))))

(last-pair2 (list 1 2))
(last-pair2 (list 1))

(define (last-pair3 l)
  (let ((one-less (cdr l)))
    (if (null? one-less)
      l
      (last-pair3 one-less))))
(last-pair3 (list 1 3 4 5))
