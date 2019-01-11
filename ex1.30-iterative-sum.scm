(define (identity x) x)
(define (incre x) (+ x 1))
(define (cube x) (* x x x))

; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;          (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a value-so-far)
    (if (> a b)
      value-so-far
      (iter (next a) (+ value-so-far (term a)))))
  (iter a 0))

(define (sum-integers a b)
  (sum identity a incre b))

(sum-integers 1 4)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; (sum-integers 1 4)
(integral cube 0 1 0.00001)


