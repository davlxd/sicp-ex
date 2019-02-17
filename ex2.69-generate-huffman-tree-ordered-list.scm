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



(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
(make-code-tree (make-leaf 'A 4)
                (make-code-tree
                  (make-leaf 'B 2)
                  (make-code-tree (make-leaf 'D 1)
                                  (make-leaf 'C 1))))

