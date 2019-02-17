(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (contains? the-list element)
    (cond ((null? the-list) #f)
          ((eq? (car the-list) element) #t)
          (else (contains? (cdr the-list) element))))
  (cond ((not (contains? (symbols tree) symbol))
         (error "symbol not found in this tree"))
        ((leaf? tree) '())
        ((contains? (symbols (left-branch tree)) symbol)
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? (symbols (right-branch tree)) symbol)
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)    ; symbol
                             (cadr pair))  ; frequency
                  (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge ordered-node-list)
  (cond ((null? ordered-node-list) (error "OMG"))
        ((= 1 (length ordered-node-list)) (car ordered-node-list))
        (else (let ((smallest (car ordered-node-list))
                    (smaller (cadr ordered-node-list)))
                (successive-merge (adjoin-set (make-code-tree smallest smaller)
                                              (cddr ordered-node-list)))))))
(generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)
;
; (
;   (leaf na 16)
;   (
;     (leaf yip 9)
;     (
;       (
;         (leaf a 2)
;         (
;           (leaf wah 1)
;           (leaf boom 1)
;           (wah boom)
;           2
;         )
;         (a wah boom)
;         4
;       )
;       (
;         (leaf sha 3)
;         (
;           (leaf job 2)
;           (leaf get 2)
;           (job get)
;           4
;         )
;         (sha job get)
;         7
;       )
;       (a wah boom sha job get)
;       11
;     )
;     (yip a wah boom sha job get)
;     20
;   )
;   (na yip a wah boom sha job get)
;   36
; )

(encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
