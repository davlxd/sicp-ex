(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))



(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))


; -> 

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds")))
   initial-amount))
;
; THEN:
;
; ___________________________
;
; global env: make-withdraw 
; ___________________________
;                         |  ^
;                         |  |
;                         o  o
;                         |
;                        params: initial-amount
;                        body: ((lambda ...


(define W1 (make-withdraw 100))
;
; THEN
;
; ___________________________________________________
;
; global env: make-withdraw ---------------------+
; ___________________________________________________
;                      ^                         |  ^
;                      |                         |  |
;             ______________________             |  |
;                                                |  |
;             E1: initial-amount: 100            o  o
;             ______________________             |
;                                                |
;                                               params: initial-amount
;                                               body: ((lambda (balance) ...
;
; In E1, eval ((lambda (balance)  .... ) initial-amount)
; into        ((lambda (balance)  .... ) 100)
;
;
; THEN, create another new env/frame, E2
;
; ___________________________________________________
;
; global env: make-withdraw ---------------------+
; _______________________________________________|___
;                      ^                         |  ^
;                      |                         |  |
;             ______________________             |  |
;                                                |  |
;             E1: initial-amount: 100             o  o
;             ______________________             |
;                      ^                         |
;                      |                        params: initial-amount
;                      |                        body: ((lambda (balance) ...
;             ______________________
;
;             E2: balance: 100
;             ______________________
;
;
; THEN in E2, after eval, we have a lambda text with balance being replaced into 100
; And bind that to W1 in global env
;
; _________________________________________________________________
;                           
; global env:  make-withdraw ----------------------------------+
;              W1--+                                           |
; _________________|___________________________________________|____
;                  |                 ^                         |  ^
;                  |                 |                         |  |
;                  |        ______________________             |  |
;                  |                                           |  |
;                  |        E1: initial-amount: 100             o  o
;                  |        ______________________             |
;                  |                 ^                         |
;                  |                 |                        params: initial-amount
;                  |                 |                        body: ((lambda (balance) ...
;                  |        ______________________
;                  |
;                  |        E2: balance: 100
;                  |        ______________________
;                  |                 ^
;                  |                 |
;                  |                 |
;                  o  o--------------+
;                  |
;                  |
;              params: amount
;              body: ...
;
;
(W1 50)
;
; First acquare the pair of W1 from global env, which is a procedure, to apply
; we will construct a frame with amount bound to 50
;
; _________________________________________________________________
;                           
; global env:  make-withdraw ----------------------------------+
;              W1--+                                           |
; _________________|___________________________________________|____
;                  |                 ^                         |  ^
;                  |                 |                         |  |
;                  |        ______________________             |  |
;                  |                                           |  |
;                  |        E1: initial-amount: 100             o  o
;                  |        ______________________             |
;                  |                 ^                         |
;                  |                 |                        params: initial-amount
;                  |                 |                        body: ((lambda (balance) ...
;                  |        ______________________
;                  |
;                  |        E2: balance: 100
;                  |        ______________________
;                  |                 ^   ^
;                  |                 |   |
;                  |                 |   |
;                  o  o--------------+   +---------------------+
;                  |                                           |
;                  |                                           |
;                  |                                      _____________
;                  |
;                  |                                      E3: amount: 50
;              params: amount                             _____________
;              body: ...
;
;
; And in E3, we eval the body of W1 with amount bound to 50
; 
; when done, E3 is killed, balance in E2 changed to 50:
;
; _________________________________________________________________
;                           
; global env:  make-withdraw ----------------------------------+
;              W1--+                                           |
; _________________|___________________________________________|____
;                  |                 ^                         |  ^
;                  |                 |                         |  |
;                  |        _______________________            |  |
;                  |                                           |  |
;                  |        E1: initial-amount: 100            o  o
;                  |        _______________________            |
;                  |                 ^                         |
;                  |                 |                        params: initial-amount
;                  |                 |                        body: ((lambda (balance) ...
;                  |        ______________________
;                  |
;                  |        E2: balance: 50
;                  |        ______________________
;                  |                 ^
;                  |                 |
;                  |                 |
;                  o  o--------------+
;                  |
;                  |
;              params: amount
;              body: ...
;
;
;

(define W2 (make-withdraw 100))

;
;
; ____________________________________________________________________________________________________
;                           
; global env:  make-withdraw ---------------------------------------------------------------------+
;              W1 ------------------------------------+                                           |
;              W2 --+                                 |                                           |
; __________________|_________________________________|___________________________________________|____
;                   |                                 |                 ^                         |  ^
;                   |                                 |                 |                         |  |
;                   |      ______________________     |        ______________________             |  |
;                   |                                 |                                           |  |
;                   |      E3: initial-amount: 100    |        E1: initial-amount: 100            o  o
;                   |      ______________________     |        ______________________             |
;                   |               ^                 |                 ^                         |
;                   |               |                 |                 |                        params: initial-amount
;                   |               |                 |                 |                        body: ((lambda (balance) ...
;                   |      ______________________     |        ______________________
;                   |                                 |                                  
;                   |      E4: balance: 100           |        E2: balance: 100
;                   |      ______________________     |        ______________________
;                   |               ^                 |                 ^
;                   |               |                 |                 |
;                   |               |                 |                 |
;                   o  o------------+                 o  o--------------+
;                   |                                 |
;                   |                                 |
;               params: amount                    params: amount
;               body: ...                         body: ...
;
;
