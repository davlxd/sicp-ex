
(define (make-real value)
  (attach-tag 'real value))

(define (raise datum)
  (let ((type (type-tag datum))
	(value (contents datum)))
    (cond ((eq? type 'scheme-number) (make-rational value 1))
	  ((eq? type 'rational) (make-real (/ (numer value) (denom value))))
	  ((eq? type 'real) (make-complex-from-real-imag value 0))
	  ((eq? type 'complex) datum))))

;
; Correction:
;

(define (raise datum) (apply-generic 'raise datum))

(put 'raise 'scheme-number (lambda (value) (make-rational value 1)))
(put 'raise 'rational (lambda (value) (make-real (/ (numer value) (denom value)))))
(put 'raise 'real (lambda (value) (make-complex-from-real-imag value 0)))
