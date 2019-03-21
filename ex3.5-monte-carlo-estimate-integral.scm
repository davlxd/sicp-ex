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


(exact->inexact
  (estimate-integral (lambda ()
                       (<= (+ (square (- (random-in-range -1.0 1.0) 0))
                              (square (- (random-in-range -1.0 1.0) 0)))
                           (square 1)))
                     -1 
                     1 
                     -1 
                     1
                     1000000))











