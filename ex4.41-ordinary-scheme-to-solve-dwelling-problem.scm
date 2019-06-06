
(define (baker-space) '((1) (2) (3) (4)))


(define (cooper-baker-space)
  (filter
    (lambda (cooper-baker)
      (let ((cooper (car cooper-baker))
            (baker (cadr cooper-baker)))
        (not (= cooper baker))))
    (append-map
      (lambda (cooper)
        (map
          (lambda (baker)
            (cons cooper baker))
          (baker-space)))
      '(2 3 4 5))))
(cooper-baker-space)


(define (fletcher-cooper-baker-space)
  (filter
    (lambda (fletcher-cooper-baker)
      (let ((fletcher (car fletcher-cooper-baker))
            (rest-of-fletcher (cdr fletcher-cooper-baker))
            (cooper (cadr fletcher-cooper-baker)))
        (and (not (memq fletcher rest-of-fletcher))
             (not (= (abs (- fletcher cooper)) 1)))))
    (append-map
      (lambda (fletcher)
        (map
          (lambda (cooper-baker)
            (cons fletcher cooper-baker))
          (cooper-baker-space)))
      '(2 3 4))))
(fletcher-cooper-baker-space)


(define (miller-fletcher-cooper-baker-space)
  (filter
    (lambda (miller-fletcher-cooper-baker)
      (let ((miller (car miller-fletcher-cooper-baker))
            (rest-of-miller (cdr miller-fletcher-cooper-baker))
            (cooper (caddr miller-fletcher-cooper-baker)))
        (and (not (memq miller rest-of-miller))
             (> miller cooper))))
    (append-map
      (lambda (miller)
        (map
          (lambda (fletcher-cooper-baker)
            (cons miller fletcher-cooper-baker))
          (fletcher-cooper-baker-space)))
      '(1 2 3 4 5))))
(miller-fletcher-cooper-baker-space)



(define (smith-miller-fletcher-cooper-baker-space)
  (filter
    (lambda (smith-miller-fletcher-cooper-baker)
      (let ((smith (car smith-miller-fletcher-cooper-baker))
            (rest-of-smitch (cdr smith-miller-fletcher-cooper-baker))
            (fletcher (caddr smith-miller-fletcher-cooper-baker)))
        (and (not (memq smith rest-of-smitch))
             (not (= (abs (- smith fletcher)) 1)))))
    (append-map
      (lambda (smith)
        (map
          (lambda (miller-fletcher-cooper-baker)
            (cons smith miller-fletcher-cooper-baker))
          (miller-fletcher-cooper-baker-space)))
      '(1 2 3 4 5))))
(smith-miller-fletcher-cooper-baker-space)


(map
  (lambda (each-posibility)
    (list (list 'smith (car each-posibility))
          (list 'miller (cadr each-posibility))
          (list 'fletcher (caddr each-posibility))
          (list 'cooper (cadddr each-posibility))
          (list 'baker (car (cddddr each-posibility)))))
  (smith-miller-fletcher-cooper-baker-space))

