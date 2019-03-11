(define (deriv0 exp var)
  (display "deriv0 ... ")
  (write-line exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (write-line "deconstruct sum")
         (make-sum (deriv0 (addend exp) var)
                   (deriv0 (augend exp) var)))
        ((product? exp)
         (write-line "deconstruct product")
         (make-sum
           (make-product (multiplier exp)
                         (deriv0 (multiplicand exp) var))
           (make-product (deriv0 (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (write-line "deconstruct exponentiation")
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv0 (base exp) var)))
        (else
          (error "unknown expression type -- DERIV0" exp))))


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (reduce make-sum 0 (cddr s)))

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

(deriv0 '(* x y (+ x 3)) 'x)



; Above from ex2.57

; a. Because numbers nad variables cannot be generalized to operator + operands and use operator as index of table.
;    For instance, if we use numbers themselves as index, that causes infinite rows.
;    However, IMHO it still possible to move number? and variable? conditions from deriv to operator & operands
;    so numbers and variables can be data-directed dispatched as well.

; b.

; lib
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
; lib end


; implmenet get and put
(define table '())
(define (get . args)
  (let ((target-list (find (lambda (entry) (equal? args (sublist entry 0 (length args)))) table)))
    (if target-list
      (list-ref target-list (length args))
      #f)))
(define (put . args) (set! table (cons args table)))
; (put 'hello 'world 3 3)
; (put 'hello 'word 1 2)
; (write-line table)
; (get 'hello)
; (get 'hello 'world)
; (get 'hello 'word)
; (get 'hello 'word 3)
; (get 'hello 'word 1)


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-deriv)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (handle-sum operands var)
    (define addend car)
    (define (augend s) (reduce make-sum 0 (cdr s)))
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (handle-product operands var)
    (define (multiplier p) (car p))
    (define (multiplicand p) 
      (if (= (length (cdr p)) 1)
        (cadr p)
        (cons '* (cdr p))))
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))

  (put 'deriv '+ handle-sum)
  (put 'deriv '* handle-product)
  'done)

(install-deriv)
(deriv0 '(* x y (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)

; c.


; lib
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
; lib end


; implmenet get and put
(define table '())
(define (get . args)
  (let ((target-list (find (lambda (entry) (equal? args (sublist entry 0 (length args)))) table)))
    (if target-list
      (list-ref target-list (length args))
      #f)))
(define (put . args) (set! table (cons args table)))
;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-makers)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (put 'maker 'sum make-sum)
  (put 'maker 'product make-product)
  'done)
(install-makers)


(define (install-deriv-sum)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (handle-sum operands var)
    (define addend car)
    (define (augend s) (reduce make-sum 0 (cdr s)))
    ((get 'maker 'sum) (deriv (addend operands) var)
                       (deriv (augend operands) var)))

  (put 'deriv '+ handle-sum)
  'done)
(install-deriv-sum)


(define (install-deriv-product)
  (define (handle-product operands var)
    (define (multiplier p) (car p))
    (define (multiplicand p) 
      (if (= (length (cdr p)) 1)
        (cadr p)
        (cons '* (cdr p))))
    ((get 'maker 'sum)
     ((get 'maker 'product) (multiplier operands)
                            (deriv (multiplicand operands) var))
     ((get 'maker 'product) (deriv (multiplier operands) var)
                            (multiplicand operands))))

  (put 'deriv '* handle-product)
  'done)
(install-deriv-product)


(define (install-maker-exponentiation)
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

  (put 'maker 'expoentiation make-exponentiation)
  'done)
(install-maker-exponentiation)


(define (install-deriv-exponentiation)
  (define (handle-exponentiation operands var)
    (define base car)
    (define exponent cadr)

    ((get 'maker 'product)
     ((get 'maker 'product) (exponent operands)
                            ((get 'maker 'expoentiation) (base operands) (- (exponent operands) 1)))
     (deriv (base operands) var)))

  (put 'deriv '** handle-exponentiation)
  'done)
(install-deriv-exponentiation)

(deriv '(* x y (+ x 3)) 'x)

; d.

; In that case the parameter order when put need to be changed as well
;


