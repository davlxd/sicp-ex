; For 
;
((lambda (x)
   (define u 2)
   (define v 3)
   (+ x u v)) 1)
; being sequentially interpretered
;

; _____________________________________________________________________________
;
; global env:
; _____________________________________________________________________________
;                       ^
;                       |
;                 ______|________ 
;             
;                  E1: x = 1
;                      u = 2
;                      v = 3
;
;                      eval (+ x u v)
;                 _______________
;
;

; For 
;
((lambda (x)
   (let ((u '*unassigned*)
         (v '*unassigned*))
     (set! u 2)
     (set! v 3)
     (+ x u v))) 1)

; _____________________________________________________________________________
;
; global env:
; _____________________________________________________________________________
;                       ^
;                       |
;                 ______|________ 
;             
;                  E1: x = 1
;                 _______________
;                       ^
;                       |
;             __________|___________
;         
;              E2: u = '*unassigned*
;                  v = '*unassigned*
;
;                  then u being set! to 2,
;                       v being set! to 3,
;                       eval (+ x u v)
;             ______________________
;
;

; Extra frame E2 exists because (let ((u '*unassigned*) (v '*unassigned*)) <...>)
; being converted to ((lambda (u v) <...>) '*unassigned* '*unassigned*)
; And eval of compound procedure extends a new env
;
; It doesn't make difference because evaluation happens in E2 can find x in E1


; To implement "simultaneous" scope without constructing the extra frame
; I think we can add defines at the beginning, like this:


(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

; would be transformed into

(lambda <vars>
  (define u '*unassigned*)
  (define v '*unassigned*)
  (set! u <e1>)
  (set! v <e2>)
  <e3>)


