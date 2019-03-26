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

(insert-queue! q1 'b)

(delete-queue! q1)

(delete-queue! q1)




; exercise begin:
;
(define make-deque make-queue)
(define empty-deque empty-queue?)
(define front-deque front-queue)

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" queue)
    (car (rear-ptr-ptr deque))))

(define (front-insert-deque! deque item)
  <...>
  )

(define rear-insert-deque! insert-queue!)


(define front-delete-deque! delete-queue!)


(define (rear-delete-deque! deque) ; So far it's good until this operation, and it seems deque requires double linked list
  <...>
  )





; dlist element
(define (make-element item) (cons '() item))
(define (prev-element element) (car element))
(define (element-item element) (cdr element))
(define (set-element-prev! element prev) (set-car! element prev))
;


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque)
    (element-item (car (front-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" deque)
    (element-item (car (rear-ptr deque)))))


(define (front-insert-deque! deque item)
  (let ((new-element (make-element item)))
    (let ((new-pair (cons new-element '())))
      (cond ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque)
            (else
              (set-cdr! new-pair (front-ptr deque))
              (set-element-prev! (car (front-ptr deque)) new-pair)
              (set-front-ptr! deque new-pair))))))


(define (rear-insert-deque! deque item)
  (let ((new-element (make-element item)))
    (let ((new-pair (cons new-element '())))
      (cond ((empty-deque? deque)
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque)
            (else
              (set-cdr! (rear-ptr deque) new-pair)
              (set-element-prev! new-element (rear-ptr deque))
              (set-rear-ptr! deque new-pair))))))


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else
          (set-front-ptr! deque (cdr (front-ptr deque)))
          (if (null? (front-ptr deque)) (set-rear-ptr! deque '()))
          (if (not (empty-deque? deque))
            (set-element-prev! (car (front-ptr deque)) '()))))) 


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else
          (set-rear-ptr! deque (prev-element (car (rear-ptr deque))))
          (if (null? (rear-ptr deque)) (set-front-ptr! deque '()))
          (if (not (empty-deque? deque))
            (set-cdr! (rear-ptr deque) '()))))) 


(define (print-deque deque) (map (lambda (element) (element-item element)) (front-ptr deque)))



(define q1 (make-deque))
(define q2 (make-deque))

(front-insert-deque! q1 'a)
(rear-insert-deque! q2 'b)

(rear-delete-deque! q1)
(front-delete-deque! q2)


(front-insert-deque! q1 'b)
(rear-insert-deque! q1 'c)
(print-deque q1)


(front-insert-deque! q1 'a)
(print-deque q1)

(rear-insert-deque! q1 'd)
(print-deque q1)


(front-delete-deque! q1)
(print-deque q1)


(rear-delete-deque! q1)
(print-deque q1)





