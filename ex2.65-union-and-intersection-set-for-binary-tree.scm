(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set-ordered-list (cdr set1)
                                                  (cdr set2))))
            ((< x1 x2)
             (intersection-set-ordered-list (cdr set1) set2))
            ((< x2 x1)
             (intersection-set-ordered-list set1 (cdr set2)))))))


(define (union-set-ordered-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set-ordered-list (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set-ordered-list (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set-ordered-list set1 (cdr set2)))))))))


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
  (car (partial-tree elements (length elements))))


(define (intersection-set set1 set2)
  (let ((set1-as-ordered-list (tree->list set1))
        (set2-as-ordered-list (tree->list set2)))
    (list->tree (intersection-set-ordered-list set1-as-ordered-list set2-as-ordered-list))))

(define (union-set set1 set2)
  (let ((set1-as-ordered-list (tree->list set1))
        (set2-as-ordered-list (tree->list set2)))
    (list->tree (union-set-ordered-list set1-as-ordered-list set2-as-ordered-list))))



(intersection-set (list->tree '(1 3 5 7 9 11))
                  (list->tree '(2 3 6 7 9 13)))

(union-set (list->tree '(1 3 5 7 9 11))
                  (list->tree '(2 3 6 7 9 13)))

