(define (make-vect x y) (lambda (op) (op x y)))
(define (xcor-vect vect) (vect (lambda (x y) x)))
(define (ycor-vect vect) (vect (lambda (x y) y)))
(define (serilize-vect vect) (cons (xcor-vect vect) (ycor-vect vect)))




(define (make-segment start end)
  (lambda (op) (op start end)))

(define (start-segment segment)
  (segment (lambda (start end) start)))

(define (end-segment segment)
  (segment (lambda (start end) end)))

(define (serilize-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (cons (serilize-vect start) (serilize-vect end))))

; (serilize-vect (make-vect 10.1 33))
(serilize-segment (make-segment (make-vect 10.1 33) (make-vect 20 1)))


