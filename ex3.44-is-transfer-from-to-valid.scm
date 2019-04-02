; Exchange without serialization can happen like this:
;
;
; Initially a1(10), a2(20), a3(30)
; Peter exchange a1 and a2       Paul exchange a2 and a3
;
;    get a1 = 10
;    get a2 = 20
;    calc diff = -10
;    set a1 = 10 - -10 = 20
;                                   get a2 = 20
;                                   get a3 = 30
;                                   calc diff = -10
;    set a2 = 20 + -10 = 10
;                                   set a2 = 10 - -10 = 20
;                                   set a3 = 30 + -10 = 20
;
;          => a1(20), a2(20), a3(20)
;
;


; Same interleave applies to from and to:
;
; Initially a1(10), a2(20), a3(30)
; Peter transfer 10 from a1 to a2      Paul transfer 20 from a2 to a3
;
;    set a1 = 10 - 10 = 0
;                                   set a2 = 20 - 20 = 0
;    set a2 = 0 + 10 = 10
;                                   set a3 = 30 + 20 = 50
;
;          => a1(0), a2(10), a3(50)
;
;

; It's okay because both transfer-from and transfer-to are atomic, 
; even it can be interleaved between these two, but it doesn't matter because
; the amount of money will be applied eventually
