; a. If we call exp with 2 complex numbers, you'd got a 404 for ['exp '(complex complex)]
;    Then with the existing implementation of apply-generic, it'd try to convert the 1st 
;    complex to complex, then invoke apply-generic with the exact same parameters,
;    so we have a infinite loop
;
;
;
; b. For the case of 404 of some op with 2 same types, without Louis's contribution, apply-generic
;    would try to find coercion with same type, which result in another 404, so the process halts,
;    it's not ideal but works, barely correct.
;
;
; c. 
;
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
            (if (not (eq? type1 type2)) ; <-- changes
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags)))))
              (error "The 2 params have same type with no method" type1))) ; <--- changes
          (error "No method for these types"
                 (list op type-tags)))))))



