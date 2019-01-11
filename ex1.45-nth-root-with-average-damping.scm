(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (iter guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(fixed-point f next))))
  (iter first-guess))


(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))

(define (compose f g) (lambda (x) (f (g x))))

(define (identity x) x)

(define (repeated g n)
  (define (iter counter result)
    (if (= counter 0)
      result
      (iter (- counter 1) (compose g result))))
  (iter n identity))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define (nth x n) ; this can be log(n)ed
  (define (iter counter result)
    (if (= counter 0)
      result
      (iter (- counter 1) (* result x))))
  (iter n 1))

; lib ends

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (cubic-root x) (fixed-point-of-transform (lambda (y) (/ x (square y))) average-damp 1.0))

(define (cubic x) (* x x x))
(define (forth-root x) (fixed-point-of-transform (lambda (y) (/ x (cubic y))) (repeated average-damp 2) 1.0))

(define (nth-root x n k) (fixed-point-of-transform (lambda (y) (/ x (nth y (- n 1)))) (repeated average-damp k) 1.0))



(nth-root 256 8 2) ; hangs
(nth-root 65536 16 3) ; hangs

; Looks like k = floor(log(n)/log(2))


(define (nth-root x n) 
  (let ((damp-count (floor (/ (log n) (log 2)))))
    (fixed-point-of-transform (lambda (y) (/ x (nth y (- n 1)))) (repeated average-damp damp-count) 1.0)))



