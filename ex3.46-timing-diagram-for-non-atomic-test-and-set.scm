;
;
;
;    With Peter and Paul, correct behaviour:
;         +10       -20
;

;        Peter                Paul
;
;                             mutex acquired
;     mutex acquire retry     balance: 100
;     mutex acquire retry     new val = 100 - 20
;     mutex acquire retry     set! 80
;                             mutex released
;     mutex acquired
;     balance: 80
;     new val = 80 + 10
;     set! 90
;     mutex released
;
;                   => 90



; expand to test-and-set! =>

;        Peter                Paul
;
;                             mutex cell return false
;                             set mutex cell true
;     mutex cell return true  get balance: 100
;     mutex cell return true  new val = 100 - 20
;     mutex cell return true  set! 80
;                             set mutex cell false
;     mutex cell return false
;     set mutex cell true
;     get balance: 80
;     new val = 80 + 10
;     set! 90
;     set mutex cell false
;
;                   => 90



; with non-atomic test-and-set!


;        Peter                Paul
;
;                             mutex cell return false
;     mutex cell return false
;                             set mutex cell true
;     set mutex cell true     get balance 100
;     get balance 100         new val = 100 - 20
;     new val = 100 + 10      set! 80
;     set! 110                set mutex cell false
;     set mutex cell false
;
;                   => 110


