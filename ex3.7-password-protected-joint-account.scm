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
    (cond ((not (eq? input-password password)) (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)


(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
((peter-acc 'open-sesame 'deposit) 30)
((peter-acc 'open-sesame 'withdraw) 10)




(define (make-joint acc old-password new-password)
  (lambda (input-password m)
    (if (not (eq? input-password new-password))
      (error "Incorrect password ~")
      (acc old-password m))))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'open-sesame 'withdraw) 40)
((paul-acc 'rosebud 'withdraw) 10)
((paul-acc 'rosebud 'deposit) 100)
((paul-acc 'rosebud 'withdraw) 90)


(define dav-acc
  (make-joint peter-acc 'open-sesame-wrong 'rosebud))

((dav-acc 'rosebud 'withdraw) 10)
((dav-acc 'rosebud 'deposit) 100)



