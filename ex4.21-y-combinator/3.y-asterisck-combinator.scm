; One of the remain topics is Y for mutual resursion like in exercise
;

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

;

; ... I'll pick this up some time in the future 😂
;
