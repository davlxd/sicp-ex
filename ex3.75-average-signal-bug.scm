(defino (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define identity (lambda (x) x))
(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (x) (newline) (display x)) s n))

(define sample-signals
  (cons-stream
    1
    (cons-stream
      2
      (cons-stream
        1.5
        (cons-stream
          1
          (cons-stream
            0.5
            (cons-stream
              -0.1
              (cons-stream
                -2
                (cons-stream
                  -3
                  (cons-stream
                    -2
                    (cons-stream
                      -0.5
                      (cons-stream
                        0.2
                        (cons-stream
                          3
                          (cons-stream
                            4
                            ()))))))))))))))

(define sense-data sample-signals)

(define (sign-change-detector a b)
  (cond 
    ((and (< b 0) (< a 0)) 0)
    ((and (< b 0) (>= a 0)) 1)
    ((and (>= b 0) (< a 0)) -1)
    ((and (>= b 0) (>= a 0)) 0)
    ))


(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))


(display-stream zero-crossings 12)



; The problem is Louis' version calc avpt with current signal and previous avpt not previous signal value

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (newline)
    (write-line (list "avpt:" avpt))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0))


(display-stream zero-crossings 12)


; Fix

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (newline)
    (write-line (list "avpt:" avpt "last-value" last-value "last-avpt" last-avpt))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt
                                      ))))

(define zero-crossings (make-zero-crossings sense-data 0 (/ (+ (stream-car sense-data) 0) 2)))
(display-stream zero-crossings 12)




