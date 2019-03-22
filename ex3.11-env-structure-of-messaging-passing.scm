(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; _____________________________
;
; global env: make-account
; _____________________________
;                         |  ^
;                         |  |
;                         o  o
;                         |
;                        params: balance
;                        body: (define withdraw ...
;                              (define deposit ...
;                              (define dispatch ...
;                              dispatch



(define acc (make-account 50))

; for (make-account 50), create a new env E1 with initial binding balance=50
; Then inside E1, eval body of make-account, which creates bindings
; withdraw
; deposit
; dispatch
;
; _______________________________________________________________
;
; global env: make-account --+
; ___________________________|___________________________________
;                            |  ^                        ^
;                            |  |                        |
;                            o  o                        |
;                            |                           |
;                           params: balance              |
;                           body: (define withdraw ...   |
;                                 (define deposit ...    |
;                                 (define dispatch ...   |
;                                 dispatch               |
;                                                        |
;                                    ____________________|___
;                                                                <------------+------+------+
;                                     E1: balance: 50                         |      |      |
;                                         withdraw: (lambda ...  ----------o  o      |      |
;                                         deposit: (lambda ...   -----------------o  o      |
;                                         dispatch: (lambda ...  ------------------------o  o
;                                         (dispatch as ret)
;                                    ________________________
;
;
;
; (define acc (make-account 50))
; and dispatch as return value, which in turn bound to acc to global env
;
; _______________________________________________________________
;
; global env: make-account --+
;             acc --+        |
; __________________|________|___________________________________
;                   |        |  ^                        ^
;                   |        |  |                        |
;                   |        o  o                        |
;                   |        |                           |
;                   |       params: balance              |
;                   |       body: (define withdraw ...   |
;                   |             (define deposit ...    |
;                   |             (define dispatch ...   |
;                   |             dispatch               |
;                   |                                    |
;                   |                ____________________|___
;                   |
;                   |                                            <------------+------+------+
;                   |                 E1: balance: 50                         |      |      |
;                   |                     withdraw: (lambda ...  ----------o  o      |      |
;                   |                     deposit: (lambda ...   -----------------o  o      |
;                   +-------------------> dispatch: (lambda ...  ------------------------o  o
;                                    ________________________
;
;



((acc 'deposit) 40)
;90
;
; For (acc 'deposit) part, first find content of acc from global env
; a new env E2 will be created, with initial binding m='deposit, and enclosing env as E1
;
; _______________________________________________________________
;
; global env: make-account --+
;             acc --+        |
; __________________|________|___________________________________
;                   |        |  ^                        ^
;                   |        |  |                        |
;                   |        o  o                        |
;                   |        |                           |
;                   |       params: balance              |
;                   |       body: (define withdraw ...   |
;                   |             (define deposit ...    |
;                   |             (define dispatch ...   |
;                   |             dispatch               |
;                   |                                    |
;                   |                ____________________|___
;                   |                                            <------------+------+------+
;                   |                 E1: balance: 50                         |      |      |
;                   |                     withdraw: (lambda ...  ----------o  o      |      |
;                   |                     deposit: (lambda ...   -----------------o  o      |
;                   +-------------------> dispatch: (lambda ...  ------------------------o  o
;                                    ________________________
;                                              ^
;                                              |
;                                              |
;                                              |
;                                    ________________________
;
;                                     E2: m: 'deposit
;                                    ________________________
;
;
;
;
; Inside E2, the body of disaptch will be eval with 'deposit, and returns deposit itself as lambda
;
;                                    ________________________
;
;                                     E2: m: 'deposit
;                                         (deposit as ret)
;                                    ________________________
;
;
; Then we go to the eval of (deposit 40), which again will create another new env with initial binding and enclosing to E1
; E2 will be destryoed
;
; _______________________________________________________________
;
; global env: make-account --+
;             acc --+        |
; __________________|________|___________________________________
;                   |        |  ^                        ^
;                   |        |  |                        |
;                   |        o  o                        |
;                   |        |                           |
;                   |       params: balance              |
;                   |       body: (define withdraw ...   |
;                   |             (define deposit ...    |
;                   |             (define dispatch ...   |
;                   |             dispatch               |
;                   |                                    |
;                   |                ____________________|___
;                   |                                            <------------+------+------+
;                   |                 E1: balance: 50                         |      |      |
;                   |                     withdraw: (lambda ...  ----------o  o      |      |
;                   |                     deposit: (lambda ...   -----------------o  o      |
;                   +-------------------> dispatch: (lambda ...  ------------------------o  o
;                                    ________________________
;                                              ^
;                                              |
;                                              |
;                                              |
;                                    ________________________
;
;                                     E3: amount: 40
;                                    ________________________
;
; 
; And inside E3, we eval the body of deposit, find balance var, change the value and return its value
;
; _______________________________________________________________
;
; global env: make-account --+
;             acc --+        |
; __________________|________|___________________________________
;                   |        |  ^                        ^
;                   |        |  |                        |
;                   |        o  o                        |
;                   |        |                           |
;                   |       params: balance              |
;                   |       body: (define withdraw ...   |
;                   |             (define deposit ...    |
;                   |             (define dispatch ...   |
;                   |             dispatch               |
;                   |                                    |
;                   |                ____________________|___
;                   |                                            <------------+------+------+
;                   |                 E1: balance: 90                         |      |      |
;                   |                     withdraw: (lambda ...  ----------o  o      |      |
;                   |                     deposit: (lambda ...   -----------------o  o      |
;                   +-------------------> dispatch: (lambda ...  ------------------------o  o
;                                    ________________________
;



((acc 'withdraw) 60) ;30
; This is similar to above, result structure would be:
;
; _______________________________________________________________
;
; global env: make-account --+
;             acc --+        |
; __________________|________|___________________________________
;                   |        |  ^                        ^
;                   |        |  |                        |
;                   |        o  o                        |
;                   |        |                           |
;                   |       params: balance              |
;                   |       body: (define withdraw ...   |
;                   |             (define deposit ...    |
;                   |             (define dispatch ...   |
;                   |             dispatch               |
;                   |                                    |
;                   |                ____________________|___
;                   |                                            <------------+------+------+
;                   |                 E1: balance: 30                         |      |      |
;                   |                     withdraw: (lambda ...  ----------o  o      |      |
;                   |                     deposit: (lambda ...   -----------------o  o      |
;                   +-------------------> dispatch: (lambda ...  ------------------------o  o
;                                    ________________________
;



(define acc2 (make-account 100))
;
; ____________________________________________________________________________________________________
;
; global env: make-account --+
;             acc --+        |
;             acc2  |
; _____________|____|________|________________________________________________________________________
;              |    |        |  ^                        ^                                         ^    
;              |    |        |  |                        |                                         |
;              |    |        o  o                        |                                         |
;              |    |        |                           |                                         |
;              |    |       params: balance              |                                         |
;              |    |       body: (define withdraw ...   |                                         |
;              |    |             (define deposit ...    |                                         |
;              |    |             (define dispatch ...   |                                         |
;              |    |             dispatch               |                                         |
;              |    |                                    |                                         |
;              |    |                ____________________|___                                      |
;              |    |                                            <------------+------+------+      |
;              |    |                 E1: balance: 30                         |      |      |      |
;              |    |                     withdraw: (lambda ...  ----------o  o      |      |      |
;              |    |                     deposit: (lambda ...   -----------------o  o      |      |
;              |    +-------------------> dispatch: (lambda ...  ------------------------o  o      |
;              |                     ________________________                                      |
;              |                                                                                   |
;              |                                                                                   |
;              |                                                               ____________________|___
;              |                                                                                           <------------+------+------+
;              |                                                                E4: balance: 100                         |      |      |
;              |                                                                    withdraw: (lambda ...  ----------o  o      |      |
;              |                                                                    deposit: (lambda ...   -----------------o  o      |
;              +------------------------------------------------------------------> dispatch: (lambda ...  ------------------------o  o
;                                                                              ________________________
;                                                               
;
;
;
;
;
;
;
;









