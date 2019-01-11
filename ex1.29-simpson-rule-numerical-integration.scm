(define (identity x) x)
(define (incre x) (+ x 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-integers a b)
  (sum identity a incre b))
(sum-integers 1 4)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(define (sum2 f n) 
  ; 0 included
  (if (< n 0)
    0
    (+ (f n)
       (sum2 f (- n 1)))))

(define (simpsons-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y m) 
    (f (+ a (* m (h)))))
  (print (h))
  (define (ny m) 
    (cond ((= 0 m) (y m))
	  ((= n m) (y m))
	  ((even? m) (* 2 (y m)))
	  (else (* 4 (y m)))))
  (* (sum2 ny n)
     (/ (h) 3)))


; (integral cube 0 1 0.00001)
(simpsons-integral cube 0 1 10000)

; (integral cube 0 1 0.001)


