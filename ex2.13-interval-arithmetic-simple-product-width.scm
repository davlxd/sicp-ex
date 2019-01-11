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

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))


(define (sub-interval x y)
  (let ((new-lower-bound (- (lower-bound x) (upper-bound y)))
	(new-upper-bound (- (upper-bound x) (lower-bound y))))
    (make-interval new-lower-bound new-upper-bound)))

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c p)
  (let ((w (* c p)))
    (let ((lb (- c w))
	  (ub (+ c w)))
      (lambda (m) (m c p w lb ub)))))

(define (upper-bound i)
  (i (lambda (c p w lb ub) ub)))

(define (lower-bound i)
  (i (lambda (c p w lb ub) lb)))

(define (width i)
  (i (lambda (c p w lb ub) w)))

(define (center i)
  (i (lambda (c p w lb ub) c)))

(define (percent i)
  (i (lambda (c p w lb ub) p)))

; all positive
(define (mul-interval1 x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (make-interval (* lx ly) (* ux uy))
    (make-interval (* (- c1 (* c1 p1)) ) (* ux uy))

    ))
; [(c1 - c1 * p1) * (c2 - c2 * p2), (c1 + c1 * p1) * (c2 + c2 * p2)]
; [(c1c2 - c1c2p2 - c1c2p1 + c1c2p1p2), (c1c2 + c1c2p1 + c1c2p2 + c1c2p1p1)]
; [(c1c2+c1c2p1p2 - c1c2(p1 + p2)), (c1c2+c1c2p1p1 + c1c2(p1 + p2)]
; C = c1c2+c1c2p1p2;  W = c1c2(p1 + p2)
; P = W/C = [c1c2(p1+p2)] / (c1c2 + c1c2p1p2) = (p1 + p2) / (1 + p1p2)




