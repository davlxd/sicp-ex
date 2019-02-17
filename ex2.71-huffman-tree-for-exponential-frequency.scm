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
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))

(encode-symbol 'A (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
(encode-symbol 'E (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))


; Huffman tree for such frequencies is a centipede like binary tree
; for most it's always 1, for least it's log(max-freq) also n itself +-1
;


