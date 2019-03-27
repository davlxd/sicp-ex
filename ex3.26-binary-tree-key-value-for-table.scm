; The idea is to see if (get ...) with binary treed table is faster than unordered listed table
;
;
;
; so for the old table: (from ex3.25)

; Assoc inside MIT-scheme is optimized I belive, so use this naive version for lookup and MIT-scheme's assoc for insert!
(define (assoc-simple key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc-simple key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define data local-table)

    (define (lookup subtable key-list)
      (cond ((not subtable) #f)
            ((null? key-list) (if (list? subtable) subtable (cdr subtable)))
            (else (lookup (assoc-simple (car key-list) (cdr subtable)) (cdr key-list)))))

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
            ((eq? m 'data) data)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define TESTING_SIZE 100000)

(define (put-many n)
  (if (<= n 0)
    'done
    (let ((rand-number (random TESTING_SIZE)))
      (put (list rand-number) rand-number)
      (put-many (- n 1)))))


(define (execution-time proc)
  (let ((start-ts (real-time-clock)))
    (proc)
    (- (real-time-clock) start-ts)))

; With testing_size 100000, on my machine this gives me ~ 370000
(execution-time (lambda () (put-many TESTING_SIZE)))

; With testing_size 100000, on my machine this gives me ~ 44
(execution-time (lambda () 
                  (newline)
                  (write-line (get (list 99999999999)))
                  ))




; binary tree operations adapted to table data structure

(define (entry tree) (car tree))
(define (entry-key tree) (caar tree))
(define (entry-value tree) (cdar tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? key set)
  (cond ((null? set) false)
        ((= key (entry-key set)) (entry set))
        ((< key (entry-key set))
         (element-of-set? key (left-branch set)))
        ((> key (entry-key set))
         (element-of-set? key (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (car x) (entry-key set)) set)
        ((< (car x) (entry-key set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (car x) (entry-key set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;(define testing-tree (make-tree (cons 4 4) '() '()))
;(adjoin-set (cons 3 3) testing-tree)
;(define testing-tree2 (adjoin-set (cons 5 5) (adjoin-set (cons 3 3) testing-tree)))
;testing-tree2
;(element-of-set? 4 testing-tree2)
;(element-of-set? 3 testing-tree2)


(define (assoc-simple key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc-simple key (cdr records)))))

(define (assoc-binary-tree key tree) (element-of-set? key tree))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define data local-table)

    (define (lookup subtable key-list)
      (cond ((not subtable) #f)
            ((null? key-list) (if (list? subtable) subtable (cdr subtable)))
            (else (lookup (assoc-binary-tree (car key-list) (cdr subtable)) (cdr key-list)))))

    (define (insert! subtable key-list value)
      (if (null? key-list)
        (set-cdr! subtable value)
        (let ((cur-key (car key-list))
              (rest-keys (cdr key-list))
              (subtable-rest (if (list? subtable) (cdr subtable) '())))
          (let ((record (assoc-binary-tree cur-key subtable-rest)))
            (if (not record)
              (begin
                (set! record (list cur-key))
                (set-cdr! subtable (adjoin-set record subtable-rest))))
            (insert! record rest-keys value)))))


    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key-list) (lookup local-table key-list)))
            ((eq? m 'insert-proc!) (lambda (key-list value) (insert! local-table key-list value)))
            ((eq? m 'data) data)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))



(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; To note this table now works for numeric keys only


(define TESTING_SIZE 100000)

(define (put-many n)
  (if (<= n 0)
    'done
    (let ((rand-number (random TESTING_SIZE)))
      (put (list rand-number) rand-number)
      (put-many (- n 1)))))


(define (execution-time proc)
  (let ((start-ts (real-time-clock)))
    (proc)
    (- (real-time-clock) start-ts)))


; With testing_size 100000, on my machine this gives me ~ 5000
(execution-time (lambda () (put-many TESTING_SIZE)))

; With testing_size 100000, on my machine this gives me ~ 2
(execution-time (lambda () 
                  (newline)
                  (write-line (get (list 99999999999)))
                  ))

; With testing_size 100000, on my machine this gives me ~ 18
(execution-time (lambda () 
                  (newline)
                  (write-line (get (list 0)))
                  ))

; With testing_size 100000, on my machine this gives me ~ 1
(execution-time (lambda () 
                  (newline)
                  (write-line (get (list 1000)))
                  ))



