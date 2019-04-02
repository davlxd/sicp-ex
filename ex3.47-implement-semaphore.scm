(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))


(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))



(define p ((make-serializer) (lambda () (write-line "Okay"))))
(p)



(define (make-semaphore n)
  (define count 0)
  (define mutex (make-mutex))
  (lambda (m)
    (cond ((eq? m 'acquire)
           (set! count (+ count 1)) ;; This has to be atomic
           (if (>= count n) (mutex 'acquire))) ;; This has to be atomic
          ((eq? m 'release)
           (set! count (- count 1)) ;; This has to be atomic
           (if (<= count n) (mutex 'release)))))) ;; This has to be atomic


; If it's allowed to modify test-and-set! a little
(define (test-and-set! predicate cell value)
  (if (not (predicate))
    false
    (begin (set-car! cell value)
           (car cell)))


  (define (make-semaphore n)
    (define cell (list 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if ((not (test-and-set! (lambda () (>= (car cell) n)) cell (+ (car cell) 1))))
               (the-semaphore 'acquire)))
            ((eq? m 'release)
             (test-and-set! (lambda () true) cell (- (car cell) 1))))))
  the-semaphore)

;
;








