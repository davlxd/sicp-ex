
(define (deriv exp var)
  (display "deriv : ")
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
	(else
	  (error "unknown expression type -- DERIV" exp))))



(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

; a:

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define addend car)
(define augend caddr)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define multiplier car)
(define multiplicand caddr)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

; (deriv '(x + (3 * (x + (y + 2))))  'x)


; b:

(define (split-with the-list spliter)
  (define (iter 1st-half remaining-list)
    (cond ((not (pair? remaining-list)) (cons 1st-half '()))
	  ((eq? spliter (car remaining-list)) (cons 1st-half (cdr remaining-list)))
	  (else (iter (append 1st-half (list (car remaining-list))) (cdr remaining-list)))))
  (iter '() the-list))

(define (sum? x) 
  (and (pair? x)
       (> (length (filter (lambda (el) (eq? el '+)) x)) 0)))
(define (addend x)
  (let ((raw (car (split-with x '+))))
    (if (= 1 (length raw)) (car raw) raw)))
(define (augend x)
  (let ((raw (cdr (split-with x '+))))
    (if (= 1 (length raw)) (car raw) raw)))

(define (product? x) 
  (and (pair? x)
       (= 0 (length (filter (lambda (el) (eq? el '+)) x)))))
(define multiplier car)
(define (multiplicand x)
  (let ((raw (cddr x)))
    (if (= 1 (length raw)) (car raw) raw)))

; (deriv '(x + 3 * (x + y + 2))  'x)
(deriv '(x * 3 + (x + y + 2))  'x)


