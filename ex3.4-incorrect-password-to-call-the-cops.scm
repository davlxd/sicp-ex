(define (make-account balance password)
  (define incorrect-password-count 0)
  (define (call-the-cops)
    (newline)
    (write-line "Calling the cops"))

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch input-password m)
    (if (eq? input-password password)
      (begin
        (if (not (= 0 incorrect-password-count)) (set! incorrect-password-count 0))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      (if (>= incorrect-password-count 7)
        (call-the-cops)
        (begin
          (set! incorrect-password-count (+ incorrect-password-count 1))
          (error "Incorrect password")))))

  dispatch)


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)
((acc 'secret-other-password 'withdraw) 40)

