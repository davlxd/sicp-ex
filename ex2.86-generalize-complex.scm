; Changes are commented as ;;<-



(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square x) (apply-generic 'mul x x)) ;;<-
(define (sqrt1 x) (apply-generic 'sqrt1 x)) ;;<- TODO
(define (atan1 x y) (apply-generic 'atan1 x y)) ;;<- TODO
(define (cos1 x) (apply-generic 'cos1 x)) ;;<- TODO
(define (sin1 x) (apply-generic 'sin1 x)) ;;<- TODO


;;->
(define (install-sqrt1)
  (put 'sqrt1 '(scheme-number) sqrt)
  (put 'sqrt1 '(rational) (lambda (value) (sqrt (/ (numer value) (denom value)))))
  (put 'sqrt1 '(real) sqrt)
  'done)

(define (install-atan1)
  (put 'atan1 '(scheme-number scheme-number) atan)
  (put 'atan1 '(rational rational) (lambda (y x)
				     (let ((real-y (/ (numer y) (denom y)))
					   (real-x (/ (numer x) (denom x))))
				       (atan real-y real-x))))
  (put 'atan1 '(real real) atan)
  'done)

(define (install-cos1)
  (put 'cos1 '(scheme-number) cos)
  (put 'cos1 '(rational) (lambda (value) (cos (/ (numer value) (denom value)))))
  (put 'cos1 '(real) cos)
  'done)

(define (install-sin1)
  (put 'sin1 '(scheme-number) sin)
  (put 'sin1 '(rational) (lambda (value) (sin (/ (numer value) (denom value)))))
  (put 'sin1 '(real) sin)
  'done)
;;<-



(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt1 (add (square (real-part z)) ;;<-
	       (square (imag-part z)))))
  (define (angle z)
    (atan1 (imag-part z) (real-part z))) ;;<-
  (define (make-from-mag-ang r a) 
    (cons (mul r (cos1 a)) (mul r (sin1 a)))) ;;<-
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)



(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos1 (angle z))))  ;;<-
  (define (imag-part z)
    (mul (magnitude z) (sin1 (angle z)))) ;;<-
  (define (make-from-real-imag x y) 
    (cons (sqrt1 (add (square x) (square y))) ;;<-
	  (atan1 y x))) ;;<-
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)



(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))  ;;<-
			 (add (imag-part z1) (imag-part z2))))  ;;<-
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2)) ;;<-
			 (sub (imag-part z1) (imag-part z2))))  ;;<-
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))  ;;<-
		       (add (angle z1) (angle z2))))  ;;<-
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))  ;;<-
		       (sub (angle z1) (angle z2))))  ;;<-
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)



(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))





