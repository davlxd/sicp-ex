; a.
((lambda (n) ; -> lambda0
   ((lambda (fact)  ; <- lambda1
      (fact fact n))
    (lambda (ft k)  ; <- lambda2
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 3)

; Manual emulation:
; => (lambda0 3)
; => (lambda1 lambda2)
; => (lambda2 lambda2 3)
; => (* 3 (lambda2 lambda2 2))
; => (* 3 (* 2 (lambda2 lambda2 1))
; => (* 3 (* 2 1)
;
;
;

; For Fib:
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fib k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (fib fib (- k 2)) (fib fib (- k 1))))))))
 9)

; b.
;
(define (f x)
  (define (even1? n)
    (if (= n 0)
      true
      (odd1? (- n 1))))
  (define (odd1? n)
    (if (= n 0)
      false
      (even1? (- n 1))))
  (even1? x))

(f 4)
(f 3)

(define (f x)
  ((lambda (even1? odd1?) ; <- lambda0
     (even1? even1? odd1? x))
   (lambda (ev? od? n) ; <- lambda1
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n) ; <- lambda2
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 4)
(f 3)

; Manual emulation of (f 4)
; => (lambda0 lambda1 lambda2)
; => (lambda1 lambda1 lambda2 4)
; => (lambda2 lambda1 lambda2 3)
; => (lambda1 lambda1 lambda2 2)
; => (lambda2 lambda1 lambda2 1)
; => (lambda1 lambda1 lambda2 0)
; => #t

; The following explores the Y operator/combinitor
;
; Which is a combinator (lambda expression without free variables) that generalize the above idea
;
; And the core idea of above transformation is
;   - Parameterize the logic, or
;   - Take logic along with function calls
;
; Take a look at the structure of transformed:
;
; (lambda (n)
;   ((lambda (fact)
;      (fact fact n))  ;; 1.
;    (lambda (ft k)
;      (if (= k 1)
;        1
;        (* k (ft ft (- k 1))))))) ;; 2.
;
; Line 1 kicks off the resursive call, Line 2 makes sure it's keeping going
; All the others are logics taken along
;
; And if we abstract the logic as
;
; (define fact
;   (lambda (f)
;     (lambda (k)
;       (if (= k 1)
;         1
;         (* k (f (- k 1)))))))
;
;
; And rewrite(curry) the 2nd lambda as
;
; (lambda (ft)
;   (lambda (k)
;     (if (= k 1)
;       1
;       (* k (ft ft (- k 1))))))
;
;
; So it can be generalized as:
;
; (lambda (f)  ;; f is formal param for fact
;   (lambda (ft)
;     (f (ft ft))))
;
;
; Putting them together and simplify params:
;
; (lambda (n)  ;; n is formal param for f
;   (lambda (f)  ;; f is fact
;     ((lambda (x) (x x n))
;      (lambda (x) (f (x x))))))
;
; So our dear Y would be
;
; (lambda (f)
;   ((lambda (x) (x x))
;    (lambda (x) (f (x x)))))
;
; It's parameter is fact above, its return value is a lambda taking the same parameter as fact itself
;
; Let's try

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (x x))))))

(define fact
  (lambda (f)
    (lambda (k)
      (if (= k 1)
        1
        (* k (f (- k 1)))))))

((Y fact) 3)

; Unfactortunately it's not working, because to apply factact to Y, it will eval ((lambda (x) (x x)) (lambda (x) (fact (x x))))
; which basiclly is apply (lambda (x) (fact (x x))) to itselfact, which becomes into
; (fact ((lambda (x) (fact (x x))) (lambda (x) (fact (x x))))), the evaluation continues as
; (fact (fact ((lambda (x) (fact (x x))) (lambda (x) (fact (x x)))) ... infactinitely
;
; To delay the eval of (x x), consider itself is a one-param lambda
; Redefine Y
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

; In this case

(Y fact)

; evals into
; (fact (lambda (y) (( (lambda (x) (fact (lambda (y) ((x x) y))))  (lambda (x) (fact (lambda (y) ((x x) y)))) ) y)))
;                      |           <--       content is protected by lambda not eval immediately      -->         |

((Y fact) 3) ;=> 6

;
; To take a look at the result lambda after applying fact into Y, we have
;
((lambda (x) (x x))
 (lambda (x)
   (lambda (k)
     (if (= k 1)
       1
       (* k ( (lambda (y) ((x x) y)) (- k 1)))))))
;
; Which is equivelant to  (with n/k curried)
;
(lambda (n)
  ((lambda (fact)
     (fact fact n))
   (lambda (ft k)
     (if (= k 1)
       1
       (* k (ft ft (- k 1))))))
  )

;
; try:
;
(
 ((lambda (x) (x x))
  (lambda (x)
    (lambda (k)
      (if (= k 1)
        1
        (* k ( (lambda (y) ((x x) y)) (- k 1)))))))
 3) ;=> 6

; And the fact we abstract above

(define fact
  (lambda (f)
    (lambda (k)
      (if (= k 1)
        1
        (* k (f (- k 1)))))))
;
; Actually is a lambda, whose fixed-point is the lambda we want, put it another way:
; (fact <the-lambda-we-want>) = <the-lambda-we-want>, or let's expand
;
; (lambda (k)
;   (if (= k 1)
;     1
;     (* k (<the-lambda-we-want> (- k 1)))))
;
; equals
; <the-lambda-we-want>
;
; Or put it another way:
; if <the-lambda-we-want> is capable of calculating factorial of any number, then
;
; (lambda (k)
;   (if (= k 1)
;     1
;     (* k (<the-lambda-we-want> (- k 1)))))
;
; can do that as well
;
; So Y would be a lambda which is able to find fact's fixed-point
; Then we can define
;
; (define Y (lambda (f) (f (Y f)))) ; explains as (Y f) equals (f (Y f))
;
; Again we need to lazilize (Y f) to avoid infinite loop
;
(define Y (lambda (f) (f (lambda (x) ((Y f) x)))))

((Y fact) 3) ; => 6

; This Y looks much easier and I like it very much, except it's not a combinator because itself uses named define
; So game on!
;
; To deal with
;
(define fact
  (lambda (k)
    (if (= k 1)
      1
      (* k (fact (- k 1))))))

; we can eliminate the reference of defined fact by parameterizing it (directly curried)

(define fact
  (lambda (self)
    (lambda (k)
      (if (= k 1)
        1
        (* k ((self self) (- k 1)))))))

((fact fact) 3)

; Inline the bootstrap

(define fact
  (lambda (n)
    ((lambda (x) ((x x) n))
     (lambda (self)
       (lambda (k)
         (if (= k 1)
           1
           (* k ((self self) (- k 1)))))))))
(fact 3)

; Which basically is the exercise solution, we eliminated defined based recursion!

; Extract and parameterize the logic

(define fact
  (lambda (n)
    ((lambda (x) ((x x) n))

     (
      (lambda (f)
        (lambda (self)
          (f (lambda (x) ((self self) x)))))

      (lambda (f)
        (lambda (k)
          (if (= k 1)
            1
            (* k (f (- k 1))))))
      )

     )))

(fact 3)

; Take the logic to outmost

(define fact
  (
   (lambda (f)
     (lambda (n)
       ((lambda (x) ((x x) n))
        (lambda (self) (f (lambda (x) ((self self) x)))))))

   (lambda (f)
     (lambda (k)
       (if (= k 1)
         1
         (* k (f (- k 1))))))
   )
  )
(fact 3)

; Generlize:

(define fact
  (lambda (f)
    (lambda (k)
      (if (= k 1)
        1
        (* k (f (- k 1)))))))

(define Y
  (lambda (f)
    (lambda (n)
      ((lambda (x) ((x x) n))
       (lambda (self) (f (lambda (x) ((self self) x))))))))

((Y fact) 3)

; (lambda (x) ((y x))) equals y itself

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (self) (f (lambda (x) ((self self) x)))))))

((Y fact) 3)

; This basically is what I did in the beginning
















; For the mutually resursive above

; Firstly curry x and n

(define f
  (lambda (x)
    ((lambda (even1? odd1?)
       ((even1? even1? odd1?) x))
     (lambda (ev? od?)
       (lambda (n)
         (if (= n 0) true ((od? ev? od?) (- n 1)))))
     (lambda (ev? od?)
       (lambda (n)
         (if (= n 0) false ((ev? ev? od?) (- n 1)))))))
  )

(f 4)

; Then parameterize logics

(define f
  (lambda (x)
    ((lambda (even1? odd1?)
       ((even1? even1? odd1?) x))

     (
      (lambda (f)
        (lambda (ev? od?)
          (f (lambda (x) ((od? ev? od?) x)))))
      (lambda (f)
        (lambda (n)
          (if (= n 0) true (f (- n 1)))))
      )

     (
      (lambda (f)
        (lambda (ev? od?)
          (f (lambda (x) ((ev? ev? od?) x)))))
      (lambda (f)
        (lambda (n)
          (if (= n 0) false (f (- n 1)))))
      )

     )))

(f 4)

; Take logics to outmost

(define f
  (
   (lambda (f1 f2)
     (lambda (x)
       ((lambda (even1? odd1?)
          ((even1? even1? odd1?) x))

        (lambda (ev? od?)
          (f1 (lambda (x) ((od? ev? od?) x))))

        (lambda (ev? od?)
          (f2 (lambda (x) ((ev? ev? od?) x))))
        )))

   (lambda (f)
     (lambda (n)
       (if (= n 0) true (f (- n 1)))))

   (lambda (f)
     (lambda (n)
       (if (= n 0) false (f (- n 1)))))

   )
  )

(f 4)

; Generalize

(define YY
  (lambda (f1 f2)
    (lambda (x)
      ((lambda (even1? odd1?)
         ((even1? even1? odd1?) x))

       (lambda (ev? od?)
         (f1 (lambda (x) ((od? ev? od?) x))))

       (lambda (ev? od?)
         (f2 (lambda (x) ((ev? ev? od?) x))))
       ))))

((YY
   (lambda (f)
     (lambda (n)
       (if (= n 0) true (f (- n 1)))))

   (lambda (f)
     (lambda (n)
       (if (= n 0) false (f (- n 1)))))

   )
 4)

; =>

(define YY
  (lambda (f1 f2)
    ((lambda (a b) (a a b))

     (lambda (a b)
       (f1 (lambda (x) ((b a b) x))))

     (lambda (a b)
       (f2 (lambda (x) ((a a b) x))))
     )))

((YY
   (lambda (f)
     (lambda (n)
       (if (= n 0) true (f (- n 1)))))

   (lambda (f)
     (lambda (n)
       (if (= n 0) false (f (- n 1)))))

   )
 4)

; Parameterize logics option 2

(define f
  (lambda (x)
    ((lambda (even1? odd1?)
       ((even1? even1? odd1?) x))

     (
      (lambda (f)
        (lambda (ev? od?)
          (f ev? od?)))
      (lambda (a b)
        (lambda (n)
          (if (= n 0) true ((b a b) (- n 1)))))
      )

     (
      (lambda (f)
        (lambda (ev? od?)
          (f ev? od?)))
      (lambda (a b)
        (lambda (n)
          (if (= n 0) false ((a a b) (- n 1)))))
      )

     ))
  )

(f 4)

; =>

(define f
  (
   (lambda (f1 f2)
     (lambda (x)
       ((lambda (even1? odd1?)
          ((even1? even1? odd1?) x))

        (lambda (ev? od?)
          (f1 ev? od?))

        (lambda (ev? od?)
          (f2 ev? od?))

        )))
   (lambda (a b)
     (lambda (n)
       (if (= n 0) true ((b a b) (- n 1)))))

   (lambda (a b)
     (lambda (n)
       (if (= n 0) false ((a a b) (- n 1)))))
   )

  )

(f 4)

; =>

(define YY
  (lambda (f1 f2)
    ((lambda (a b) (a a b))

     (lambda (a b) (f1 a b))

     (lambda (a b) (f2 a b))
     )))

((YY
   (lambda (a b)
     (lambda (n)
       (if (= n 0) true ((b a b) (- n 1)))))

   (lambda (a b)
     (lambda (n)
       (if (= n 0) false ((a a b) (- n 1)))))
   ) 3)

; Ideas borrow from Wikipedia and The Why of Y, and https://mvanier.livejournal.com/2897.html
;
