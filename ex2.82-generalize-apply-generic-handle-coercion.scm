; BTW the exercises have become a bit freestyle recently, the 2nd author took over maybe?
;
; Suppose we have (put 'some-op '(scheme-number complex complex) lambda... )
; With (apply-generic 'some-op a-scheme-number a-rational a-complex)
; we would only try ['some-op '(scheme-number scheme-number scheme-number)]
;                   ['some-op '(rational rational rational)]
;                   ['some-op '(complex, complex, complex)]
; 
;
;
;


(define (list-contains-same-element? l)
  (cond ((or (null? l) (= 1 (length l))) #t)
        ((eq? (car l) (cadr l)) (list-contains-same-element? (cdr l)))
        (else #f)))



(define (apply-generic op . args)
  (define (coerce target-type-tags type-tags args)
    (if (not (null? target-type-tags))
      (let ((target-type (car target-type-tags)))

        ; unfinished

        )

      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
            (apply proc (map contents args))

            (if (list-contains-same-element? type-tags)
              (error "Same type but still no method" type-tags)
              (coerce type-tags args))






            (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else
                          (error "No method for these types"
                                 (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))



            ))))


