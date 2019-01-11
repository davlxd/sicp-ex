(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))
; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). 

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    (list )
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (map-n op . seqs) ;; BiwaScheme doesn't have generic map
  (define (recur seqs)
    (if (null? (car seqs))
      (list )
      (cons (apply op (map car seqs))
	    (recur (map cdr seqs)))))
  (recur seqs))

(map-n + (list 1 2 3) (list 10 20 30) (list 100 200 300))





(define (dot-product v w)
  (accumulate + 0 (map-n * v w)))
(dot-product (list 1 2 3 4 5) (list 10 20 30 40 50))


(define (matrix-*-vector m v)
  (map (lambda (v-of-m) (dot-product v-of-m v)) m))
(matrix-*-vector 
  (list (list 1 2 3) 
	(list 4 5 6) 
	(list 7 8 9)
	(list 1 2 3)
	(list 3 2 1))
  (list 100 200 300))


(define (transpose mat)
  (accumulate-n cons (list ) mat))
(transpose
  (list (list 1 2 3) 
	(list 4 5 6) 
	(list 7 8 9)
	(list 1 2 3)
	(list 3 2 1)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (each-m) 
	   (map (lambda (each-cols) (dot-product each-cols each-m)) cols)) 
	 m)))

(matrix-*-matrix
  (list (list 1 2 3) 
	(list 4 5 6) 
	(list 7 8 9)
	(list 1 2 3)
	(list 3 2 1))

  (list (list 10 1) 
	(list 20 2) 
	(list 30 3))
  ; (list (list 10 20 30) 
  ;       (list 1 2 3 )) 
  )
