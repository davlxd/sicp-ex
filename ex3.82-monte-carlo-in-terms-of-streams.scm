(define (rand) (random (expt 2 64)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((bound-area (* (abs (- x2 x1)) (abs (- y2 y1))))
        (ratio (monte-carlo trials P)))
    (* bound-area ratio)))


;(exact->inexact
;  (estimate-integral (lambda ()
;                       (<= (+ (square (- (random-in-range -1.0 1.0) 0))
;                              (square (- (random-in-range -1.0 1.0) 0)))
;                           (square 1)))
;                     -1 
;                     1 
;                     -1 
;                     1
;                     1000000))

;
; Above is ex3.5
; Below is current exercise
;

(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (x) (newline) (display x)) s n))

(define (random-in-range-stream low high)
  (cons-stream (+ low (random (- high low))) (random-in-range-stream low high)))

(define (if-random-dot-in-stream low-x low-y high-x high-y center-x center-y radius)
  (stream-map (lambda (x y) (<= (+ (square (- x center-x)) (square (- y center-y))) (square radius)))
              (random-in-range-stream low-x high-x)
              (random-in-range-stream low-y high-y)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))


(define pi 
  (let ((low-x -1.0)
        (low-y -1.0)
        (high-x 1.0)
        (high-y 1.0)
        (center-x 0)
        (center-y 0)
        (radius 1))
    (stream-map (lambda (p) (* p (* (- high-x low-x) (- high-y low-y))))
                (monte-carlo (if-random-dot-in-stream low-x low-y high-x high-y center-x center-y radius) 0 0))))

(display-stream pi 20)
(stream-ref pi 100000)





