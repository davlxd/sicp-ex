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



(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" queue)
        (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr)))) 

    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty queue" queue))
            (else (set-front-ptr! (cdr front-ptr))
                  front-ptr
                  ))) 

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) (lambda (item) (insert-queue! item))) ;; This is actually plain insert-queue! ¯\_(o o)_/¯
            ((eq? m 'delete-item!) (delete-queue!))
            (else (error "Unkown request -- MAKE-QUEUE" m))))
    dispatch))


(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) (queue 'delete-item!))


(define q1 (make-queue))
q1

(insert-queue! q1 'a)

(insert-queue! q1 'b)

(delete-queue! q1)

(delete-queue! q1)



