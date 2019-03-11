(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (equ? x y) (apply-generic 'equ? x y))



(define (install-equ?)
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) 
       (lambda (a b) (and (= (numer a) (number b)) (= (denom a) (denom b))))) ;denom and numer need export
  (put 'equ? '(complex complex)
       (lambda (a b) (and (= (real-part a) (real-part b)) (= (imag-part a) (imag-part b)))))
  (put 'equ? '(scheme-number rational)
       (lambda (a b) (and (= (denom b) 1) (= a (numer b)))))
  (put 'equ? '(rational scheme-number)
       (lambda (a b) ((get 'equ? '(scheme-number rational)) b a)))
  (put 'equ? '(scheme-number complex)
       (lambda (a b) (and (= (imag-part b) 0) (= a (real-part b)))))
  (put 'equ? '(complex scheme-number)
       (lambda (a b) ((get 'equ? '(scheme-number complex)) b a)))
  (put 'equ? '(rational complex)
       (lambda (a b) (and (= (denom a) 1) (= (imag-part b) 0) (= (numer a) (real-part b)))))
  (put 'equ? '(complex rational)
       (lambda (a b) ((get 'equ? '(rational complex)) b a)))
  'done)
