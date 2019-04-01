;
; Ben's concern makes sense to a certain extent, some one could read the balance right after P1 execution complete or P2, so in this case this one is reading intermediate value
;
; But this intermediate value is legitimate value
;
;    With Peter and Paul 
;         -10       -25
;

;        Peter                Paul
;
;                            balance       : 100
;                            new val & set : 75
;     balance        : 75
;     new val & set! : 65
;
;                   => 65


;        Peter                Paul
;
;     balance        : 100
;     new val & set! : 90
;                            balance       : 90
;                            new val & set : 65
;
;                   => 65


