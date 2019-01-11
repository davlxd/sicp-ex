(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((reciprocal (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)) )))
    (write-line "div is mul of :")
    (write-line x)
    (write-line y)
    (write-line reciprocal)
    (mul-interval x reciprocal)))




(define (sub-interval x y)
  (let ((new-lower-bound (- (lower-bound x) (upper-bound y)))
	(new-upper-bound (- (upper-bound x) (lower-bound y))))
    (make-interval new-lower-bound new-upper-bound)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

; (par1 (make-interval 10.01 10.1) (make-interval 20.1 20.11))
; (par2 (make-interval 10.01 10.1) (make-interval 20.1 20.11))

; par1 and par2 has different result because par1 involves 1 div, par2 involves 3 div, 
; everytime div happens, we have to do a reciprocal, 
; and the value won't be accurate for (/ 1.0 bigInt)




; percent:



(define (make-center-percent c p) 
  (let ((width (* c p))) 
    (make-interval (- c width) (+ c width)))) 

(par1 (make-center-percent 10 0.01) (make-center-percent 20 0.1))

(par1 (make-center-percent 10 0.01) (make-center-percent 20 0.1))



