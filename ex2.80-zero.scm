(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(error
	  "No method for these types -- APPLY-GENERIC"
	  (list op type-tags))))))

(define (=zero? x) (apply-generic '=zero? x))

(define (install-=zero?)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex) (lambda (x) (and (= 0 (real-part x)) (= 0 (imag-part x)))))
  'done)
