; (define (make-vect x y) (lambda (op) (op x y)))
; (define (xcor-vect vect) (vect (lambda (x y) x)))
; (define (ycor-vect vect) (vect (lambda (x y) y)))
; (define (serilize-vect vect) (cons (xcor-vect vect) (ycor-vect vect)))

;; this is neater
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vect1 vect2)
  (make-vect
    (+ (xcor-vect vect1)
       (xcor-vect vect2))
    (+ (ycor-vect vect1)
       (ycor-vect vect2))))
(define (scale-vect s vect)
  (make-vect
    (* s (xcor-vect vect))
    (* s (ycor-vect vect))))
(define (sub-vect vect1 vect2)
  (add-vect vect1 (scale-vect -1 vect2)))

; (define (make-frame origin edge1 edge2) (list origin edge1 edge2))
; (define (origin-frame frame) (car frame))
; (define (edge1-frame frame) (cadr frame))
; (define (edge2-frame frame) (caddr frame))

;; this is neater
(define make-frame list)
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
			    (edge1-frame frame))
		(scale-vect (ycor-vect v)
			    (edge2-frame frame))))))


; (define (make-segment start end) (lambda (op) (op start end)))
; (define (start-segment segment) (segment (lambda (start end) start)))
; (define (end-segment segment) (segment (lambda (start end) end)))

;; this is neater
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)


(define (draw-line start end)
  (let ((startx (xcor-vect start))
	(starty (ycor-vect start))
	(endx (xcor-vect end))
	(endy (ycor-vect end)))
    (print (list startx starty endx endy))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
	(draw-line
	  ((frame-coord-map frame) (start-segment segment))
	  ((frame-coord-map frame) (end-segment segment))))
      segment-list)))



;  Use segments->painter to define the following primitive painters:

; a.  The painter that draws the outline of the designated frame.

(define painter-outline-of-frame
  (let ((seg1 (make-segment (make-vect 0 0) (make-vect 1 0)))
	(seg2 (make-segment (make-vect 0 0) (make-vect 0 1)))
	(seg3 (make-segment (make-vect 1 1) (make-vect 0 1)))
	(seg4 (make-segment (make-vect 1 1) (make-vect 1 0))))
    (segments->painter (list seg1 seg2 seg3 seg4))))

; (painter-outline-of-frame 
;   (make-frame (make-vect 2 2) (make-vect 2 2) (make-vect 2 0)))


; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(define painter-x
  (let ((seg1 (make-segment (make-vect 0 0) (make-vect 1 1)))
	(seg2 (make-segment (make-vect 0 1) (make-vect 1 0))))
    (segments->painter (list seg1 seg2))))

; (painter-x
; (make-frame (make-vect 2 2) (make-vect 2 2) (make-vect 2 0)))


; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define painter-diamond
  (let ((seg1 (make-segment (make-vect .5 1) (make-vect 1 .5)))
	(seg2 (make-segment (make-vect 1 .5) (make-vect .5 0)))
	(seg3 (make-segment (make-vect .5 0) (make-vect 0 .5)))
	(seg4 (make-segment (make-vect 0 .5) (make-vect .5 1))))
    (segments->painter (list seg1 seg2 seg3 seg4))))

(painter-diamond
  (make-frame (make-vect 1 1) (make-vect 1 0) (make-vect 0.5 1) ))



; d.  The wave painter.
;; same logic applies
