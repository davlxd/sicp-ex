(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints 
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector) (connector 'has-value?))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))

(define inform-about-value (lambda () 'done))


; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
; ________________________________|______|_________|__________|__________|_____
;                                 |  ^   |  ^      |  ^       |   ^      |   ^
;                                 |  |   |  |      |  |       |   |      |   |
;                                 v  |   v  |      v  |       |   |      v   |
;                                                             |   |    O   O-+
;                                      ........               |   |  body: let ...
;                                                             |   |
;                                                             v   |
;                                                           O   O-+
;                                                         params:exception
;                                                                procedure
;                                                                list
;                                                         body: ... loop...
;                                                       


; For (make-connector), no initial bindings

; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
; ________________________________|______|_________|__________|__________|_____
;                        ^        |  ^   |  ^      |  ^       |   ^      |   ^
;                        |        |  |   |  |      |  |       |   |      |   |
;                        |        v  |   v  |      v  |       |   |      v   |
;                        |                                    |   |    O   O-+
;                                      ........               |   |  body: let ...
;                        |                                    |   |
;                   _____|_________                           v   |
;                                                           O   O-+
;                     E1:                                     params:exception
;                   _______________                              procedure
;                                                                list
;                                                         body: ... loop...
;                                                       
; 
;
;
; let ... inside make-connector creates E2
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
; ________________________________|______|_________|__________|__________|_____
;                        ^        |  ^   |  ^      |  ^       |   ^      |   ^
;                        |        |  |   |  |      |  |       |   |      |   |
;                        |        v  |   v  |      v  |       |   |      v   |
;                        |                                    |   |    O   O-+
;                        |             ........               |   |  body: let ...
;                        |                                    |   |
;                   _____|_________                           v   |
;                                                           O   O-+
;                     E1:                                     params:exception
;                   _______________                              procedure
;                          ^                                     list
;                          |                              body: ... loop...
;                          |                            
;                   _______________
;
;                     E2: value=false
;                         informant=false
;                         constraints=()
;
;                         set-my-value=lambda...    
;                         forget-my-value=lambda...
;                         connect=lambda..
;                         me=lambda ...
;                         
;                         return me
;                   _______________
;

; For (define a (make-connector)):
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;                |       ^        |  ^   |  ^      |  ^       |   ^      |   ^
;                |       |        |  |   |  |      |  |       |   |      |   |
;                |       |        v  |   v  |      v  |       |   |      v   |
;                |       |                                    |   |    O   O-+
;                |       |             ........               |   |  body: let ...
;                |       |                                    |   |
;                |  _____|_________                           v   |
;                |                                          O   O-+
;                |    E1:                                     params:exception
;                |  _______________                              procedure
;                |         ^                                     list
;                |         |                              body: ... loop...
;                |         |                            
;                |  _______________
;                |
;                |    E2: value=false
;                |        informant=false  <-------------+
;                |        constraints=()                 |
;                |                                       |
;                |        set-my-value=lambda...    O    O
;                |        forget-my-value=lambda... O    O
;                |        connect=lambda..          O    O
;                +----->  me=lambda ...             O    O
;                   _______________
;



; (set-value! a 10 'user) creates a new env with initial bindings a 10 'user
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;                |       ^        |  ^   |  ^      |  ^    ^  |   ^      |   ^
;                |       |        |  |   |  |      |  |    |  |   |      |   |
;                |       |        v  |   v  |      v  |    |  |   |      v   |
;                |       |                                 |  |   |    O   O-+
;                |       |             ........            |  |   |  body: let ...
;                |       |                                 |  |   |
;                |  _____|_________                        |  v   |
;                |                                         |O   O-+
;                |    E1:                                  |  params:exception
;                |  _______________                        |     procedure
;                |         ^                               |     list
;                |         |                              body: ... loop...
;                |         |                               |
;                |  _______________                        |
;                |                                         |
;                |    E2: value=false                      |
;                |        informant=false  <-------------+ |
;                |        constraints=()                 | |
;                |                                       | |
;                |        set-my-value=lambda...    O    O |
;                |        forget-my-value=lambda... O    O |
;                |        connect=lambda..          O    O |
;                +----->  me=lambda ...             O    O |
;                   _______________                        |
;                                                          |
;                                                          |
;                                                          |
;                                                          |
;                                                __________|___________
;                                                       
;                                                 E3: connector = a
;                                                     new-value = 10
;                                                     informant = 'user
;
;                                                     eval into ((a 'set-value!) 10 'user))
;                                                ______________________
;
;
;
; To eval (a 'set-value!):
;
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;                |       ^        |  ^   |  ^      |  ^    ^  |   ^      |   ^
;                |       |        |  |   |  |      |  |    |  |   |      |   |
;                |       |        v  |   v  |      v  |    |  |   |      v   |
;                |       |                                 |  |   |    O   O-+
;                |       |             ........            |  |   |  body: let ...
;                |       |                                 |  |   |
;                |  _____|_________                        |  v   |
;                |                                         |O   O-+
;                |    E1:                                  |  params:exception
;                |  _______________                        |     procedure
;                |         ^                               |     list
;                |         |                              body: ... loop...
;                |         |                               |
;                |  _______________                        |
;                |                                         |
;                |    E2: value=false                      |
;                |        informant=false  <-------------+ |
;                |        constraints=()                 | |
;                |                                       | |
;                |        set-my-value=lambda...    O    O |
;                |        forget-my-value=lambda... O    O |
;                |        connect=lambda..          O    O |
;                +----->  me=lambda ...             O    O |
;                   _______________                        |
;                           ^                              |
;                           |                              |
;                           |                              |
;                           |                              |
;                           |                    __________|___________
;                  _________|____________               
;                                                 E3: connector = a
;                    E4: request='set-value!          new-value = 10
;                        returns lambda set-my-value  informant = 'user
;                  ______________________
;                                                     eval into ((a 'set-value!) 10 'user))
;                                                ______________________
;
;
; To eval (set-my-value 10 'user):
;
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;                |       ^        |  ^   |  ^      |  ^    ^  |   ^      |   ^
;                |       |        |  |   |  |      |  |    |  |   |      |   |
;                |       |        v  |   v  |      v  |    |  |   |      v   |
;                |       |                                 |  |   |    O   O-+
;                |       |             ........            |  |   |  body: let ...
;                |       |                                 |  |   |
;                |  _____|_________                        |  v   |
;                |                                         |O   O-+
;                |    E1:                                  |  params:exception
;                |  _______________                        |     procedure
;                |         ^                               |     list
;                |         |                              body: ... loop...
;                |         |                               |
;                |  _______________                        |
;                |                                         |
;                |    E2: value=false                      |
;                |        informant=false  <-------------+ |
;                |        constraints=()                 | |
;                |                                       | |
;                |        set-my-value=lambda...    O    O |
;                |        forget-my-value=lambda... O    O |
;                |        connect=lambda..          O    O |
;                +----->  me=lambda ...             O    O |
;                   _______________                        |
;                           ^                              |
;                           |                              |
;                           |                              |
;                           |                              |
;                           |                    __________|___________
;                  _________|____________               
;                                                 E3: connector = a
;                    E5: newval=10                    new-value = 10
;                        setter='user                 informant = 'user
;                                        
;                        eval set-my-value            eval into ((a 'set-value!) 10 'user))
;                  _______________________       ______________________
;
;
;
;
; Value in E2 will set to 10, then creates E6 to eval (for-each-except), with initial bindings
;   exception = setter
;   procedure = inform-about-value
;   list = constraints
;
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;       ^        |       ^        |  ^   |  ^      |  ^    ^  |   ^      |   ^
;       |        |       |        |  |   |  |      |  |    |  |   |      |   |
;       |        |       |        v  |   v  |      v  |    |  |   |      v   |
;       |        |       |                                 |  |   |    O   O-+
;       |        |       |             ........            |  |   |  body: let ...
;       |        |       |                                 |  |   |
;       |        |  _____|_________                        |  v   |
;       |        |                                         |O   O-+
;       |        |    E1:                                  |  params:exception
;       |        |  _______________                        |     procedure
;       |        |         ^                               |     list
;       |        |         |                              body: ... loop...
;       |        |         |                               |
;       |        |  _______________                        |
;       |        |                                         |
;       |        |    E2: value=false                      |
;       |        |        informant=false  <-------------+ |
;       |        |        constraints=()                 | |
;       |        |                                       | |
;       |        |        set-my-value=lambda...    O    O |
;       |        |        forget-my-value=lambda... O    O |
;       |        |        connect=lambda..          O    O |
;       |        +----->  me=lambda ...             O    O |
;       |           _______________                        |
;       |                   ^                              |
;       |                   |                              |
;       |                   |                              |
;       |                   |                              |
;       |                   |                    __________|___________
;       |          _________|____________               
;       |                                         E3: connector = a
;       |            E5: newval=10                    new-value = 10
;       |                setter='user                 informant = 'user
;       |                                
;       |                eval set-my-value            eval into ((a 'set-value!) 10 'user))
;       |          _______________________       ______________________
;       |
;       |
; ______|__________
;
;  E6: exception = setter            <-----+
;      procedure = inform-about-value      |
;      list = constraints                  |
;                                          |
;      loop = lambda ...              O    O
;
;      To eval (loop list)
; __________________
;
;
;
;
; Then creates E7 to eval (loop list)
;
; _____________________________________________________________________________
;
; global env: make-connector --------------------------------------------+
;             for-each-except  -------------------------------+          |
;             has-value? --------------------------+          |          |
;             set-value! ----------------+         |          |          |
;             inform-about-value -+      |         |          |          |
;             a -+                |      |         |          |          |
; _______________|________________|______|_________|__________|__________|_____
;       ^        |       ^        |  ^   |  ^      |  ^    ^  |   ^      |   ^
;       |        |       |        |  |   |  |      |  |    |  |   |      |   |
;       |        |       |        v  |   v  |      v  |    |  |   |      v   |
;       |        |       |                                 |  |   |    O   O-+
;       |        |       |             ........            |  |   |  body: let ...
;       |        |       |                                 |  |   |
;       |        |  _____|_________                        |  v   |
;       |        |                                         |O   O-+
;       |        |    E1:                                  |  params:exception
;       |        |  _______________                        |     procedure
;       |        |         ^                               |     list
;       |        |         |                              body: ... loop...
;       |        |         |                               |
;       |        |  _______________                        |
;       |        |                                         |
;       |        |    E2: value=false                      |
;       |        |        informant=false  <-------------+ |
;       |        |        constraints=()                 | |
;       |        |                                       | |
;       |        |        set-my-value=lambda...    O    O |
;       |        |        forget-my-value=lambda... O    O |
;       |        |        connect=lambda..          O    O |
;       |        +----->  me=lambda ...             O    O |
;       |           _______________                        |
;       |                   ^                              |
;       |                   |                              |
;       |                   |                              |
;       |                   |                              |
;       |                   |                    __________|___________
;       |          _________|____________               
;       |                                         E3: connector = a
;       |            E5: newval=10                    new-value = 10
;       |                setter='user                 informant = 'user
;       |                                
;       |                eval set-my-value            eval into ((a 'set-value!) 10 'user))
;       |          _______________________       ______________________
;       |
;       |
; ______|__________
;
;  E6: exception = setter            <-----+
;      procedure = inform-about-value      |
;      list = constraints                  |
;                                          |
;      loop = lambda ...              O    O
;
;      To eval (loop list)
; __________________
;         ^
;         |
; ________|_________
;
;  E7; item = list
;
;      eval (cond ...
; __________________ 
;
;

