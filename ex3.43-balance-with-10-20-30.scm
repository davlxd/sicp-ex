
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))



; One of the condition violated scenarios
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




; Sum will be preserved because once diff is calculated, serialized withdraw and deposit
; guanrantee that the amount of diff will be put into target account without interference


; One of the condition violated scenarios
;
; Initially a1(10), a2(20), a3(30)
; Peter exchange a1 and a2       Paul exchange a2 and a3
;
;    get a1 = 10
;    get a2 = 20
;    calc diff = -10
;
;    get old a1 = 10
;    calc new a1 = 10 - -10 = 20
;    set! a1 = 20
;                                   get a2 = 20
;                                   get a3 = 30
;                                   calc diff = -10
;    get old a2 = 20
;    calc new a2 = 20 + -10 = 10
;                                   get old a2 = 20
;    set! a2 = 10
;                                   calc new a2 = 20 + -10 = 10
;                                   set! a2 = 10
;
;                                   get old a3 = 30
;                                   calc new a3 = 30 + -10 = 20
;                                   set! a3 = 20
;
;
;          => a1(20), a2(10), a3(20)


