(define painter (list 0))
(define (below a b)(append (list b) (list a)))
(define (beside a b) (append a b))


(define (right-split1 painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split1 painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split1 painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split1 painter (- n 1))))
      (below painter (beside smaller smaller)))))


(define (split o1 o2)
  (lambda (painter n)
    (define (iter n)
      (if (= n 0)
	painter
	(let ((smaller (iter (- n 1))))
	  (o1 painter (o2 smaller smaller)))))
    (iter n)))
;; this is stupid

(define (split1 o1 o2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split1 o1 o2) painter (- n 1))))
	(o1 painter (o2 smaller smaller))))
    ))


(define (split2 o1 o2)
  (define (anon painter n)
    (if (= n 0)
      painter
      (let ((smaller (anon painter (- n 1))))
	(o1 painter (o2 smaller smaller)))))
  anon
  )

(define right-split (split2 beside below))

(right-split painter 2)

; (define up-split (split below beside))

