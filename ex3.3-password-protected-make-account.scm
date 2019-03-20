(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-password m)
    (cond ((not (eq? input-password password)) (error "Incorrect password")) ;; should be hashed
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'secret-other-password 'withdraw) 40)

