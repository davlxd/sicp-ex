(define (make-monitored f)
  (define count 0)
  (lambda (arg)
    (if (eq? arg 'how-many-calls?)
      count
      (begin
        (set! count (+ count 1))
        (f arg)))))

;(define (make-monitored f)
;  (let ((count 0))
;    (lambda (arg)
;      (if (eq? arg 'how-many-calls?)
;        count
;        (begin
;          (set! count (+ count 1))
;          (f arg))))))
;

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

(s 10000)
(s 'how-many-calls?)

(s 81)
(s 'how-many-calls?)
