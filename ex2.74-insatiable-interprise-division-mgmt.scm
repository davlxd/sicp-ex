;
;
;
; a. There must be a tag attached to devision's file, let's call it division-name:
;   

(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum)
            (car datum)
	          (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum)
            (cdr datum)
	          (error "Bad tagged datum -- CONTENTS" datum)))

(define division-name-tag type-tag)
(define file-contents contents)


(define (install-dev-department)
  (define (get-record-dev name file-content)
    'some-record)

  (put 'get-record 'dev get-record-dev)
  'done)

(define (get-record name file)
  ((get 'get-record (division-name-tag file)) name (file-contents file)))


; b. In this case division name (tag) needs to be included in record as well

(define division-name-tag type-tag)
(define record-contents contents)


(define (install-dev-department)
  (define (get-record-dev name file-content)
    (attach-tag 'dev 'some-record))

  (define (get-salary-dev record-contents)
    'salary)


  (put 'get-record 'dev get-record-dev)
  (put 'get-salary 'dev get-salary-dev)
  'done)

(define (get-salary record)
  ((get 'get-salary (division-name-tag record)) (record-contents record)))



