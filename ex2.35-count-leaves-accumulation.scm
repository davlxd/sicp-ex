(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

; (define (count-leaves x)
;   (cond ((null? x) 0)  
;         ((not (pair? x)) 1)
;         (else (+ (count-leaves (car x))
;                  (count-leaves (cdr x))))))

; (count-leaves (list 1 (list 2 (list 3 4))))

(define (count-leaves2 t)
  (accumulate <??> 
	      <??> 
	      (map (lambda (item) 
		     (if (pair? item)
		       (length item) 
		       item)) 
		   t)))

(define (count-leaves2 t)
  (accumulate (lambda (x y)
		(if (pair? x)
		  (+ (count-leaves2 x) y)
		  (+ 1 y)))
	      0 
	      t))

(count-leaves2 (list (list 10 20 30) (list 40 (list 50 60))))



(define (count-leaves3 t)
  (accumulate +
	      0 
	      (map (lambda (item) 
		     (if (pair? item)
		       (count-leaves3 item) 
		       1)) 
		   t)))
(count-leaves3 (list (list 10 20 30) (list 40 (list 50 60))))


