(define (make-table)
  (let ((local-table (list '*table*)))
    (define data local-table)

    (define (lookup subtable key-list)
      (cond ((not subtable) #f)
            ((null? key-list) (if (list? subtable) subtable (cdr subtable)))
            (else (lookup (assoc (car key-list) (cdr subtable)) (cdr key-list)))))

    (define (del! subtable key-list)
      (cond ((not subtable) #f)
            ; ((= 1 (length key-list)) (del-assoc! (car key-list) (cdr subtable))) ;; inplace seems not remove 1st element?
            ((= 1 (length key-list)) (set-cdr! subtable (del-assoc (car key-list) (cdr subtable))))
            (else (del! (assoc (car key-list) (cdr subtable)) (cdr key-list)))))

    (define (insert! subtable key-list value)
      (if (null? key-list)
        (set-cdr! subtable value)
        (let ((cur-key (car key-list))
              (rest-keys (cdr key-list))
              (subtable-rest (if (list? subtable) (cdr subtable) '())))
          (let ((record (assoc cur-key subtable-rest)))
            (if (not record)
              (begin
                (set! record (list cur-key))
                (set-cdr! subtable (cons record subtable-rest))))
            (insert! record rest-keys value)))))


    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key-list) (lookup local-table key-list)))
            ((eq? m 'insert-proc!) (lambda (key-list value) (insert! local-table key-list value)))
            ((eq? m 'delete-proc!) (lambda (key-list) (del! local-table key-list)))
            ((eq? m 'data) data)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define del (operation-table 'delete-proc!))

(put '(alphanum alpha a) 97)
(operation-table 'data)
(get '(alphanum alpha a))

(operation-table 'data)
(put '(hello) 'world)
(operation-table 'data)
(get '(hello))
(get '(alphanum alpha a))


(put '(alphanum num) 4)
(operation-table 'data)
(get '(hello))
(get '(alphanum alpha a))
(get '(alphanum num))

(put '(alphanum num) 9999)
(operation-table 'data)
(get '(hello))
(get '(alphanum alpha a))
(get '(alphanum num))

(put '(alphanum alpha b) 98)
(operation-table 'data)
(get '(alphanum alpha b))


; overwrite value
(put '(alphanum alpha b) 98888)
(operation-table 'data)
(get '(alphanum alpha b))



; overwriting structure

(get '(alphanum))
(put '(alphanum) 'alphanum)
(operation-table 'data)
(get '(alphanum))


(put '(alphanum number) 1024)
(operation-table 'data)
(get '(alphanum))
(get '(alphanum number))



; del

(operation-table 'data)
(del '(alphanum))
(operation-table 'data)
(del '(hello))
(operation-table 'data)

