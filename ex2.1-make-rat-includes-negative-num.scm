(define (remainder x y)
  (if (< (* x y) 0)
    (mod (* -1 x) (* -1 y))
    (mod x y)))
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n1 (/ n g))
	  (d1 (/ d g)))
      (display g)
      (if (< d1 0)
	(cons (- n1) (- d1))
	; (cons n1 d1)
	(cons n1 d1)))))

; gcd already helped

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline)
  )
(print-rat (make-rat 3 -4))

