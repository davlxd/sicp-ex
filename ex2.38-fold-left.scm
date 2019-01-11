(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
	    (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(fold-right op initial (cdr sequence)))))


(fold-right / 1 (list 1 2 3))  ; 1.5
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list (list ) (list 1 2 3)) ; 
; (1 (2 (3 ())))
(fold-left list (list ) (list 1 2 3)) 
; (((() 1) 2) 3)

;Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.
; commutative law and associative law

; prove:

; fold-left with initial and sequance of a b c:
; (op (op (op initial a) b) c)
; ((initial op a) op b) op c

; fold-right with initial and sequance of a b c:
; (op a (op b (op c initial)))
; (a op (b op (c op initial)))

; with commutative law and associative law:
; ((initial op a) op b) op c === (a op (b op (c op initial)))



