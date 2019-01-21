(define (deriv exp var)
	(display "deriv ... ")
	(write-line exp)
	(cond ((number? exp) 0)
				((variable? exp)
				 (if (same-variable? exp var) 1 0))
				((sum? exp)
				 (write-line "deconstruct sum")
				 (make-sum (deriv (addend exp) var)
									 (deriv (augend exp) var)))
				((product? exp)
				 (write-line "deconstruct product")
				 (make-sum
					 (make-product (multiplier exp)
												 (deriv (multiplicand exp) var))
					 (make-product (deriv (multiplier exp) var)
												 (multiplicand exp))))
				((exponentiation? exp)
				 (write-line "deconstruct exponentiation")
				 (make-product
					 (make-product (exponent exp)
												 (make-exponentiation (base exp) (- (exponent exp) 1)))
					 (deriv (base exp) var)))
				(else
					(error "unknown expression type -- DERIV" exp))))


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) 
	(if (= (length (cddr s)) 1)
		(caddr s)
		(cons '+ (cddr s))))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else (list '+ a1 a2))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) 
	(if (= (length (cddr p)) 1)
		(caddr p)
		(cons '* (cddr p))))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (list '* m1 m2))))

(define (make-exponentiation base exponent)
	(cond ((=number? exponent 0) 1)
				((=number? exponent 1) base)
				(else (list '** base exponent))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)

(deriv '(* x y (+ x 3)) 'x)

; -> x * y * (x + 3)

; -> x * deriv(y * (x + 3) + 1 * (y * x + 3)

; -> x * ( y * derive(x + 3) + 0) + (y * x + 3)

; -> x * y + (y * x + 3)



 

