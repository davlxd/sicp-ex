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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; sample-tree:
; (
;   (leaf a 4) 
;   (
;     (leaf b 2)
;     (
;       (leaf d 1) (leaf c 1) (d c) 2
;     )
;     (b d c)
;     4
;   )
;   (a b d c)
;   8
; )


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

(encode-symbol 'a sample-tree)


; Generally it would be T(n) = O(n) + T(n/2), so approximately O(n*lg(n)), n as dict size
; For special case, for most it's O(1); for least it's O(n*lg(n)), lg(n) steps, for each steps O(n) search as always worst case, n as dict size





