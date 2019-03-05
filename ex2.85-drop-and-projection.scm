
(define (make-real value) (attach-tag 'real value))


(define (project datum) (apply-generic 'project datum))

(define (install-project)
  (define (project-complex-down-to-real complex)
    (make-real (real-part complex)))
  (define (project-real-down-to-rational real)
    (make-rational (round real) 1)) ;; This is incorrect, how do you convert 1.5 to 3/2?
  ; just an idea:
    (make-rational (round (* real 1000000000)) 1000000000)
  (define (project-rational-down-to-integer rational)
    (make-scheme-number (numer rational)))

  (put 'project 'complex project-complex-down-to-real)
  (put 'project 'real project-real-down-to-rational)
  (put 'project 'rational project-rational-down-to-integer)
  'done)

; ex2.81
(define (raise datum) (apply-generic 'raise datum))

(put 'raise 'scheme-number (lambda (value) (make-rational value 1)))
(put 'raise 'rational (lambda (value) (make-real (/ (numer value) (denom value)))))
(put 'raise 'real (lambda (value) (make-complex-from-real-imag value 0)))


; ex2.79

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-equ?)
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) 
       (lambda (a b) (and (= (numer a) (number b)) (= (denom a) (denom b))))) ;denom and numer need export
  (put 'equ? '(complex complex)
       (lambda (a b) (and (= (real-part a) (real-part b)) (= (imag-part a) (imag-part b)))))
  'done)


(define (drop datum)
  (let ((type-tag (type-tag datum)))
    (let ((proc (get 'project type-tag)))
      (if proc
	(let ((dropped-datum (proc datum)))
	  (if (equ? datum (raise dropped-datum)) ;; let's assume raise doesn't give us 404
	    (drop dropped-datum)
	    datum))
	datum))))


; ex2.84
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(drop (apply proc (map contents args))) ;; drop at here
	(if (= (length args) 2)
	  (let ((type1 (car type-tags))
		(type2 (cadr type-tags))
		(a1 (car args))
		(a2 (cadr args)))
	    (if (not (eq? type1 type2))

	      ;;
	      (if (> (compare-types type1 type2) 0)
		(apply-generic op a1 (successive-raise a2 type1))
		(apply-generic op (successive-raise a1 type2) a2))
	      ;;

	      (error "The 2 params have same type with no method" type1)))
	  (error "No method for these types"
		 (list op type-tags)))))))



