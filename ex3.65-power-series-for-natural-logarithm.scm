
(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s) 
  (add-streams s (cons-stream 0 (partial-sums s)))) 

(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))

(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (newline)
  (stream-for-each (lambda (x) (display x) (display " ")) s n))


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; (define (pi-summands n)
;   (cons-stream (/ 1.0 n)
;                (stream-map - (pi-summands (+ n 2)))))
; (define pi-stream
;   (scale-stream (partial-sums (pi-summands 1)) 4))

(define (ln-summands-1 n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands-1 (+ n 1)))))
(define ln-stream-1 (partial-sums (ln-summands-1 1)))


(define ln-summands-2
  (stream-map (lambda (x) (* (if (even? x) -1 1) (/ 1 x))) integers))
(define ln-stream-2 (stream-map exact->inexact (partial-sums ln-summands-2)))


(define ln-summands
  (cons-stream 1
               (stream-map (lambda (x) 
                             (* (if (> x 0) -1 1) (/ 1 (+ (denominator x) 1))))
                           ln-summands)))

(define ln-stream (stream-map exact->inexact (partial-sums ln-summands)))

(display-stream ln-stream 8)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define tableau-of-ln (make-tableau euler-transform ln-stream))

(define (display-tableau t n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (s) (display-stream s n)) t n))

(display-tableau tableau-of-ln 8)
; 1. .5 .8333333333333334 .5833333333333334 .7833333333333333 .6166666666666667 .7595238095238095 .6345238095238095
; .7 .6904761904761905 .6944444444444444 .6924242424242424 .6935897435897436 .6928571428571428 .6933473389355742 .6930033416875522
; .6932773109243697 .6931057563587684 .6931633407243163 .6931399010346379 .6931508286133836 .693145196222819 .6931483322631647 .6931464760471532
; .6931488693329254 .6931466819721099 .6931473540289107 .6931471119161925 .69314721065439 .6931471662236255 .6931471878731178 .6931471766096701
; .6931471960735491 .6931471760384008 .6931471820517628 .6931471800121786 .6931471807802199 .6931471804642407 .6931471806043729 .6931471805381099
; .6931471806635636 .6931471805287478 .6931471805701179 .6931471805563449 .6931471805613197 .6931471805593835 .6931471805601892 .6931471805598335
; .6931471805604038 .693147180559785 .6931471805599997 .693147180559926 .6931471805599525 .6931471805599424 .6931471805599465 .6931471805599448
; .6931471805599444 .6931471805599448 .6931471805599455 .6931471805599452 .6931471805599453 .6931471805599453 .6931471805599454 .6931471805599453


; pi was:

; 4. 2.666666666666667 3.466666666666667 2.8952380952380956 3.3396825396825403 2.9760461760461765 3.2837384837384844 3.017071817071818
; 3.166666666666667 3.1333333333333337 3.1452380952380956 3.13968253968254 3.1427128427128435 3.1408813408813416 3.142071817071818 3.1412548236077655
; 3.142105263157895 3.1414502164502167 3.1416433239962656 3.1415712902014277 3.1416028416028423 3.141587320947787 3.141595655236941 3.141590862710498
; 3.141599357319005 3.141590860395881 3.1415932312437636 3.141592438436833 3.14159274345584 3.1415926124349105 3.1415926739096363 3.1415926429103624
; 3.1415927140337785 3.141592637113005 3.1415926587096235 3.141592651803974 3.141592654277287 3.141592653301986 3.1415926537192127 3.1415926535279137
; 3.1415926539752927 3.1415926534771037 3.1415926536250534 3.1415926535778107 3.141592653594204 3.1415926535880514 3.14159265359053 3.1415926535894667
; 3.1415926535911765 3.141592653589245 3.141592653589981 3.1415926535897305 3.141592653589818 3.141592653589786 3.1415926535897984 3.141592653589793
; 3.141592653589778 3.141592653589794 3.1415926535897953 3.1415926535897944 3.141592653589795 3.141592653589795 3.141592653589795 3.141592653589795




