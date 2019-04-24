; The following explores the Y operator/combinitor with ideas borrow from Wikipedia, The Why of Y, and https://mvanier.livejournal.com/2897.html
;
; Y combinator is a combinator (lambda expression without free variables) that generalize the idea of exercise 4.21
;


; Taken factorial for instance
;
(define factorial
  (lambda (n)
    (if (= n 1)
      1
      (* n (factorial (- n 1))))))
(factorial 4)
;
; To eliminate the factorial reference in lambda body, we parameterize it
;
(define incomplete-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 1)
        1
        (* n (f (- n 1)))))))

;
; The KEY POINT here now is, if we observe incomplete-factorial, its parameter f is factorial itself
; because assuming we already have factorial exists that is capable of calculation factorial numbers, then
; (incomplete-factorial factorial) itself, expands into
;
(lambda (n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))
;
; would have the same capability, hence it's factorial itself as well
; So we can define factorial resursively like this:
;
; (define factorial (incomplete-factorial factorial))
;
; and of course the evaluator would eval (incomplete-factorial factorial) part firstly which leads to unbound error
; And because (incomplete-factorial factorial) itself is factorial which is a single-param lambda,
; so we can leverage lambda for delayed evaluation:
;
(define factorial (lambda (x) ((incomplete-factorial factorial) x)))
(factorial 4)

;
; On the other hand, factorial can be seen as the fixed-point for incomplete-factorial
; and considering the purpose of Y is seeking fixed-point, we can rewrite the above resursion as:
;
; factorial = (incomplete-factorial factorial)
; => (Y incomplete-factorial) = (incomplete-factorial (Y incomplete-factorial))
;
; Similarly we can define Y resursively as:
;
(define Y (lambda (f) (f (Y f))))
;
; Again similarly it won't work because if we apply incomplete-factorial to Y we'd have
; (incomplete-factorial (Y incomplete-factorial)), to eval this exp we'd have to eval parameter into:
; (incomplete-factorial (incomplete-factorial (Y incomplete-factorial))), so we have an infinite resursion
; With delayed evaluation:
;
(define Y (lambda (f) (f (lambda (x) ((Y f) x)))))
((Y incomplete-factorial) 4)

;
; With delayed evaluation inplace (Y incomplete-factorial) turns into
; (incomplete-factorial (lambda (x) ((Y incomplete-factorial) x)))
; => (lambda (n) (if (= n 1) 1 (* n (('procedure (x) ((Y incomplete-factorial) x)) (- n 1)))))
; Apply 4:
; (* 4 (('procedure (x) ((Y incomplete-factorial) x)) 3))
; (* 4 ((Y incomplete-factorial) 3))
; ...

;
; So we have our first Y!
;
(define Y (lambda (f) (f (lambda (x) ((Y f) x)))))


;
; Nice and easy except in its body it refers defined Y itself, let's try to get rid of it
;
; Some ideas we can borrow from exercise are
;   - Parameterize the logic
;   - Take logic along with function calls
;   - Hence we'd have an extra parameter for procedure calls
;
; Take a look at the structure of transformed:
;
(lambda (n)
  ((lambda (fact)
     (fact fact n))  ;; 1.
   (lambda (ft k)
     (if (= k 1)
       1
       (* k (ft ft (- k 1))))))) ;; 2.

; Line 1 kicks off the resursive call, Line 2 makes sure it's keeping going


;
; So for
;
(define Y (lambda (f) (f (lambda (x) ((Y f) x)))))
;
; Parameterize:
;
(define Y
  (lambda (self)
    (lambda (f)
      (f (lambda (x) (((self self) f) x))))))
;
; Does it work?
;
(((Y Y) incomplete-factorial) 4)
;
; Yes!, now inline the initial kickoff by using exercise solution as reference
;
(define Y
  ((lambda (x) (x x))
   (lambda (self)
     (lambda (f)
       (f (lambda (x) (((self self) f) x)))))))

((Y incomplete-factorial) 4) ;; works

((Y
   (lambda (f)
     (lambda (n)
       (cond ((= n 0) 0)
             ((= n 1) 1)
             (else (+ (f (- n 2)) (f (- n 1))))))))
 9)  ;; works


; So we now have our first formal definition of Y combinator!
;
(define Y
  ((lambda (x) (x x))
   (lambda (self)
     (lambda (f)
       (f (lambda (x) (((self self) f) x)))))))








; However the material I found on the Internet incline to use another approach:
;
; Given
;
(define incomplete-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 1)
        1
        (* n (f (- n 1)))))))
;
; Firstly we eliminate explict defined recursion by applying the same strategy above
; =>
;
(define incomplete-factorial2
  (lambda (self)
    (lambda (n)
      (if (= n 1)
        1
        (* n ((self self) (- n 1)))))))
((incomplete-factorial2 incomplete-factorial2) 4)
;
; Inline => 
;
(define factorial
  (lambda (n)
    ((lambda (x) ((x x) n))
     (lambda (self)
       (lambda (k)
         (if (= k 1)
           1
           (* k ((self self) (- k 1)))))))))
(factorial 4)
;
; Which basically is the exercise solution except this is curried version

;
; Now we try to extract incomplete-factorial as a parameter, then ideally the remaining would be Y
;
; Firstly with (lambda (x) (f x)) equals f we simplify
;
(define factorial
  ((lambda (x) (x x))
   (lambda (self)
     (lambda (n)
       (if (= n 1)
         1
         (* n ((self self) (- n 1))))))))
(factorial 4)
;
; Next abstract incomplete-factorial
;
(define factorial
  ((lambda (x) (x x))

   (
    (lambda (f)
      (lambda (self)
        (f (self self))))

    (lambda (f)
      (lambda (n)
        (if (= n 1)
          1
          (* n (f (- n 1))))))
    )
   ; (lambda (self) ;; this is result of above evaluation
   ;   (lambda (n)
   ;     (if (= n 1)
   ;       1
   ;       (* n ((self self) (- n 1))))))

   ))

;
; And again this triggers infinite resursion, because the above 2-way apply triggers
; eval exp (f (self self)) as lambda body, which in turn triggers
; eval exp (self self)
; => (f (self self)) => (f (f (self self))) => ...
;
; Again with the help of delayed evaluation:
;
(define factorial
  ((lambda (x) (x x))
   (
    (lambda (f)
      (lambda (self)
        (f (lambda (x) ((self self) x)))))

    (lambda (f)
      (lambda (n)
        (if (= n 1)
          1
          (* n (f (- n 1))))))
    )
   ))
(factorial 4)
;
; Take incomplete-factorial to outmost
;

(define factorial
  ((lambda (f)
     ((lambda (x) (x x))
      (lambda (self)
        (f (lambda (x) ((self self) x))))))

   (lambda (f)
     (lambda (n)
       (if (= n 1)
         1
         (* n (f (- n 1)))))))
  )
(factorial 4)

;
; So we have Y!
;
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (self)
       (f (lambda (x) ((self self) x)))))))

((Y incomplete-factorial) 4)





; Comparing the 2 Ys:
;
(define Y
  ((lambda (x) (x x))
   (lambda (self)
     (lambda (f)
       (f (lambda (x) (((self self) f) x)))))))

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (self)
       (f (lambda (x) ((self self) x)))))))

; It looks equivalent but I cannot lambda calculusly prove it
; I've spent too much time on this so I'll stop now



