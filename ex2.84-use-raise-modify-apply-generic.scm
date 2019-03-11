
(define (make-real value) (attach-tag 'real value))

(define (raise datum)
  (let ((type (type-tag datum))
        (value (contents datum)))
    (cond ((eq? type 'scheme-number) (make-rational value 1))
          ((eq? type 'rational) (make-real (/ (numer value) (denom value))))
          ((eq? type 'real) (make-complex-from-real-imag value 0))
          ((eq? type 'complex) datum))))

(define (compare-types type1 type2)
  (let ((type-dict '((scheme-number . 0)
                     (rational . 1)
                     (real . 2)
                     (complex . 3))))
    (let ((type-indices
            (map (lambda (type)
                   (cdr (find (lambda (type-in-dict) (eq? (car type-in-dict) type)) type-dict)))
                 (list type1 type2))))
      (- (car type-indices) (cadr type-indices)))))


(define (successive-raise datum target-type)
  (if (eq? (type-tag datum target-type))
    datum
    (successive-raise (raise datum) target-type)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
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



