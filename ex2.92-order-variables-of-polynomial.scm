;; TAG LIB
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))
;;

;; PUT/GET
(define table '())
(define (get . args)
  (let ((target-list (find (lambda (entry) (equal? args (sublist entry 0 (length args)))) table)))
    (if target-list
      (list-ref target-list (length args))
      #f)))
(define (put . args) (set! table (cons args table)))
;;


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))


;; INSTALL-SCHEME-NUMBER
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'negate '(scheme-number) (lambda (x) (tag (- x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(install-scheme-number-package)
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
;;

;; INSTALL RATIONAL
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'negate '(rational) (lambda (x) (tag (sub-rat (make-rat 0 1) x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)
(define (make-rational n d) ((get 'make 'rational) n d))
;;


;; INSTALL COMPLEX

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'negate '(complex) (lambda (x) (tag (sub-complex (make-from-real-imag 0 0) x))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-complex-package)
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
;;


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
(install-=zero?)


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-term (term-list p))))

  (define (negate-term L)
    (if (not (empty-termlist? L))
      (adjoin-term  (make-term (order (first-term L)) (negate (coeff (first-term L)))) (negate-term (rest-terms L)))
      L))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))


  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let ((rest-of-result
                    (div-terms (sub-terms L1 
                                          (mul-terms L2 
                                                     (adjoin-term (make-term new-o new-c)
                                                                  (the-empty-termlist)))) 
                               L2))) 
              (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                    (cadr rest-of-result))
              ))))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))


  (define (poly-=zero? terms)
    (cond ((null? terms) #t)
          ((=zero? (coeff (first-term terms))) (poly-=zero? (rest-terms terms)))
          (else #f)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (poly-=zero? (term-list p))))
  (put 'term-list '(polynomial) term-list) ;;<-------
  (put 'variable '(polynomial) variable) ;;<-------
  'done)
(install-polynomial-package)
(define (make-poly var terms) ((get 'make 'polynomial) var terms))
(define (term-list-of-poly poly) (apply-generic 'term-list poly)) ;;<-----
(define (variable-of-poly poly) (apply-generic 'variable poly)) ;;<-----




;; export from polynomial package for constructing testing data
(define (coeff term) (cadr term))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

(define (the-empty-termlist) '())
(define (make-term order coeff) (list order coeff))


; 3x^2 + 2x + 1
(define poly1
  (make-poly 'x
             (adjoin-term (make-term 2 (make-scheme-number 3))
                          (adjoin-term (make-term 1 (make-scheme-number 2))
                                       (adjoin-term (make-term 0 (make-scheme-number 1))
                                                    (the-empty-termlist))))))
; 6x - 4
(define poly2
  (make-poly 'x
             (adjoin-term (make-term 1 (make-scheme-number 6))
                          (adjoin-term (make-term 0 (make-scheme-number -4))
                                       (the-empty-termlist)))))

poly1

; (3x^2 + 2x + 1)y + (6x - 4)y
; => (3x^2 + 8x -3)y
; => 3y.x^2 + 8y.x - 3y

(add
  (make-poly 'y
             (adjoin-term (make-term 1 poly1) (the-empty-termlist)))
  (make-poly 'y
             (adjoin-term (make-term 1 poly2) (the-empty-termlist))))



; (3x^2 + 2x + 1)y + (6x - 4)y
; =>  (3x^2y + 2xy + y)  +  (6xy - 4y)
; =>  [ 3y.x^2 + 2y.x + y ] + [(6x - 4)y]




; My idea is firstly convert all nested poly into a generic form (``expanding'' like text says)
; Then recover into nested poly form but make nesting vars in consistent order
;
; In the mean time this marshall/unmarshall algo should have the capability of converting
; '(poly z (2 1))
; =>
; '(poly x (0 (poly y (0 (poly z (2 1))))))

;
; export from install-polynomial-package
(define (order term) (car term))
(define (coeff term) (cadr term))
;

; data structure for expanded poly
(define (powered-var var power) (list '^ var power))
(define (multiply-list . args) (cons '* args))
(define (adjoin-multiply-list a l) (cons '* (cons a (cdr l))))
;


(define (expand poly)
  (append-map (lambda (term)
                (let ((current-powered-var (powered-var (variable-of-poly poly) (order term)))
                      (coeff-of-term (coeff term)))
                  (if (eq? 'polynomial (type-tag coeff-of-term))
                    (map (lambda (inner-multiply-list) (adjoin-multiply-list current-powered-var inner-multiply-list))
                         (expand coeff-of-term))
                    (list (multiply-list coeff-of-term current-powered-var)))))
              (term-list-of-poly poly)) )

poly1 ; 3x^2 + 2x + 1
(expand poly1)

(define poly3 (add
                (make-poly 'y
                           (adjoin-term (make-term 2 poly1) (the-empty-termlist)))
                (make-poly 'y
                           (adjoin-term (make-term 2 poly2) (the-empty-termlist)))))

poly3 ; (3x^2 + 8x -3)y^2
(term-list-of-poly poly3)
(expand poly3)



(define poly4
  (make-poly 'y
             (adjoin-term (make-term 1
                                     (make-poly 'x 
                                                (adjoin-term (make-term 1 (make-scheme-number 4))
                                                             (adjoin-term (make-term 0 (make-scheme-number -2))
                                                                          (the-empty-termlist)))))
                          (the-empty-termlist))))
poly4 ; (4x - 2)y


(define poly5
  (make-poly 'z
             (adjoin-term (make-term 2 (add poly3 poly4))
                          (adjoin-term (make-term 1 poly4)
                                       (the-empty-termlist)))))
poly5 ; [(3x^2 + 8x - 3)y^2 + (4x - 2)y]z^2 + [(4x - 2)y]z



(expand poly5)
;((* (^ z 2) (^ y 2) (scheme-number . 3) (^ x 2))
; (* (^ z 2) (^ y 2) (scheme-number . 8) (^ x 1))
; (* (^ z 2) (^ y 2) (scheme-number . -3) (^ x 0))
; (* (^ z 2) (^ y 1) (scheme-number . 4) (^ x 1))
; (* (^ z 2) (^ y 1) (scheme-number . -2) (^ x 0))
; (* (^ z 1) (^ y 1) (scheme-number . 4) (^ x 1))
; (* (^ z 1) (^ y 1) (scheme-number . -2) (^ x 0))
; )

(define poly6
  (make-poly 'z
             (adjoin-term (make-term 2 (add poly3 (make-poly 'y
                                                             (adjoin-term (make-term 1 (make-scheme-number 3)) (the-empty-termlist)))))
                          (adjoin-term (make-term 1 (make-poly 'x
                                                               (adjoin-term (make-term 1 (make-scheme-number 4)) (the-empty-termlist))))
                                       (the-empty-termlist)))))
(expand poly6)

poly6 ; [(3x^2 + 8x - 3)y^2 + 3y]z^2 + 4xz

; ((* (^ z 2) (^ y 2) (scheme-number . 3) (^ x 2))
;  (* (^ z 2) (^ y 2) (scheme-number . 8) (^ x 1))
;  (* (^ z 2) (^ y 2) (scheme-number . -3) (^ x 0))
;  (* (^ z 2) (scheme-number . 3) (^ y 1))
;  (* (^ z 1) (scheme-number . 4) (^ x 1)))


;;TODO expand a 3 level poly
;;TODO (diminish ...)








