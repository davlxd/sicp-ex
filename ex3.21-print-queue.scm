(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue))) 


(define q1 (make-queue))
(insert-queue! q1 'a)
; ((a) a)
;
; This because since rear-ptr is a list (null ending), (cons front-prt rear-ptr) has the same effect as
; prepending front-ptr as first element of rear-ptr

(insert-queue! q1 'b)
; ((a b) b)
;
; b wasn't inserted twice, it's because two pointer reference same data, it's shared, and just like Eva Lu Ator said,
; Lisp interpreter print shared resources as is

(delete-queue! q1)
; ((b) b)


(delete-queue! q1)
; (() b)
;
; b remains because delete-queue! doesn't update rear-ptr, which is okay since insert-queue! will update eventually,
; just not perfect



; print-queue can be just an alias of front-ptr
;

(define print-queue front-ptr)

(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)



