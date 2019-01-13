; lib
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect vect1 vect2)
  (make-vect
    (+ (xcor-vect vect1) (xcor-vect vect2))
    (+ (ycor-vect vect1) (ycor-vect vect2))))
(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect)) (* s (ycor-vect vect))))
(define (sub-vect vect1 vect2) (add-vect vect1 (scale-vect -1 vect2)))

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
(define (compose f g)
  (lambda (x) (f (g x))))
; lib ends

; Some ideas on transform-painter:
; Maybe operation on painter can be abstracted as 
; apply painter on 2 frames consecutively,
; since painter takes only one frame in the first place,
; so we can create a procedure called composite-frames
; to composite 2 frames together

(define (composite-frame frame1 frame2)
  (let ((frame1-origin (origin-frame frame1))
        (frame1-edge1 (edge1-frame frame1))
        (frame1-edge2 (edge2-frame frame1)))
    (let ((frame1-origin-mapped ((frame-coord-map frame2) frame1-origin))
          (frame1-edge1-mapped ((frame-coord-map frame2) frame1-edge1))
          (frame1-edge2-mapped ((frame-coord-map frame2) frame1-edge2)))
      (make-frame frame1-origin-mapped frame1-edge1-mapped frame1-edge2-mapped))))
(composite-frame
  (make-frame (make-vect 1 1) (make-vect 2 0) (make-vect 1 2))
  (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 -1))
 )
; (composite-frame
;   (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 -1))
;   (make-frame (make-vect 1 1) (make-vect 2 0) (make-vect 1 2))
;  )

; ... this won't work because definition of apply frame-coord-map to edge vector 
; is unclear, frame -> vector -> points -> vector -> frame 

; Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter 
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate-cw-180 painter)
  (transform-painter painter 
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate-cw-270 painter)
  (transform-painter painter 
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))



(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-top frame)))))


(define (rotate-cw-90 painter)
  (lambda (frame)
    ((flip-horiz (rotate-cw-270 painter)) frame)))
;           |
;           v
(define (rotate-cw-90 painter)
    (flip-horiz (rotate-cw-270 painter)))
;           |
;           v
(define rotate-cw-90
  (compose flip-horiz rotate-cw-270))

(define (below2 painter1 painter2)
  (rotate-cw-270 (beside (rotate-cw-90 painter1) (rotate-cw-90 painter2))))


( (below2 (lambda (frame) (print 0))  (lambda (frame) (print 0)) ) 
  (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1) ))
