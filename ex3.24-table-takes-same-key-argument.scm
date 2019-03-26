(define (make-table same-key?)
  (let ((local-table (list '*table*))
        (assoc1 (association-procedure same-key? car)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc1 key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table 
                          (lambda (list-element-key enquiry-key)
                            (cond ((and (string? list-element-key) (string? enquiry-key)) (string-ci=? list-element-key enquiry-key))
                                  ((and (number? list-element-key) (number? enquiry-key)) (<= (abs (- list-element-key enquiry-key)) 1))
                                  (else (equal? list-element-key enquiry-key))))))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'strings "hello" "world")
(put 'strings "HellO" "WorlD")
(put 'strings "HELLO" "WORLD")
(put 'numbers 0 0)
(put 'numbers 1 1)
(put 'numbers 2 2)
(put 'numbers 3 3)
(put 'numbers 4 4)
(put 'letters 'a 97)
(put 'letters 'b 98)

(put 'math '+ 43)
(put 'math '- 45)

; GET

(get 'strings "hello")

(get 'numbers 0)
(get 'numbers 1)
(get 'numbers 2)
(get 'numbers 3)

(get 'letters 'a)
(get 'letters 'b)

(get 'math '-)
