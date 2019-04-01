;
; a. There are 6 possible orders:
;
; Peter -> Paul  -> Mary  : 100 -> 110 -> 90 -> 45
; Peter -> Mary  -> Paul  : 100 -> 110 -> 55 -> 35
; Paul  -> Peter -> Mary  : 100 -> 80  -> 90 -> 45
; Paul  -> Mary  -> Peter : 100 -> 80  -> 40 -> 50
; Mary  -> Paul  -> Peter : 100 -> 50  -> 30 -> 40
; Mary  -> Peter -> Paul  : 100 -> 50  -> 60 -> 40
;
;

;
; b. With Peter and Paul only:
;         +10       -20
;

;        Peter                Paul
;
;                            balance       : 100
;                            new val & set : 80
;     balance        : 80
;     new val & set! : 90
;
;                   => 90




;
;        Peter                Paul
;
;                            balance       : 100
;     balance        : 100
;                           (balance       : 100)
;                            new val & set : 80
;     new val & set! : 110
;
;                   => 110





;
;        Peter                Paul
;
;                            balance       : 100
;     balance        : 100
;                           (balance       : 100)
;     new val & set! : 110
;                            new val & set : 80
;
;                   => 80



;
;        Peter                Paul
;
;     balance        : 100
;     new val & set! : 110
;                            balance       : 110
;                            new val & set : 90
;
;                   => 90


;
; Now include Mary (paul, peter)
;             1/2   -20    +10

;        Peter                    Paul                    Mary
;
;                                                        balance       : 100
;                                                        new val & set : 50
;                            balance       : 50
;                            new val & set : 30
;     balance        : 30
;     new val & set! : 40
;
;                   => 40


;        Peter                    Paul                    Mary
;
;                                                        balance       : 100
;
;                            balance       : 100
;                                                       (balance       : 100)
;                                                        new val & set : 50
;                            new val & set : 80
;     balance        : 80
;     new val & set! : 90
;
;                   => 90


;        Peter                    Paul                    Mary
;
;
;                            balance       : 100
;                            new val & set : 80
;                                                        balance       : 80
;                                                        new val & set : 40
;     balance        : 40
;     new val & set! : 50
;
;                   => 50


;        Peter                    Paul                    Mary
;
;                                                        balance       : 100
;
;                            balance       : 100
;                                                       (balance       : 100)
;                            new val & set : 80
;                                                        new val & set : 50
;     balance        : 50
;     new val & set! : 60
;
;                   => 60



; ....




