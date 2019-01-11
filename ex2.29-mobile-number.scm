; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

; (left-branch (make-mobile (make-branch 10 10) (make-branch 12 12)))
; (right-branch (make-mobile (make-branch 10 10) (make-branch 12 12)))
; (branch-structure (make-branch 10 10))

; b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (let ((lb (left-branch mobile))
	  (rb (right-branch mobile)))
      (+ (total-weight (branch-structure lb)) (total-weight (branch-structure rb))))))


(total-weight 
  (make-mobile 
    (make-branch 10 10) 
    (make-branch 12 (make-mobile 
		      (make-branch 10 100) 
		      (make-branch 12 102)))))


; c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
(define (balanced? mobile)
  (let ((leftBranch (left-branch mobile))
	(rightBranch (right-branch mobile)))
    (let ((leftLength (branch-length leftBranch))
	  (leftStructure (branch-structure leftBranch))
	  (rightLength (branch-length rightBranch))
	  (rightStructure (branch-structure rightBranch)))
      (= (* leftLength (total-weight leftStructure))
	 (* rightLength (total-weight rightStructure)))))) ;; needs to recur in here 

(balanced? 
  (make-mobile 
    (make-branch 10 3.6) 
    (make-branch 12 (make-mobile 
		      (make-branch 10 1) 
		      (make-branch 12 2)))))
; d.  Suppose we change the representation of mobiles so that the constructors are

; (define (make-mobile left right)
;   (cons left right))
; (define (make-branch length structure)
;   (cons length structure))

; How much do you need to change your programs to convert to the new representation?

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile)) ;; change this
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch)) ;; change this

(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (let ((lb (left-branch mobile))
	  (rb (right-branch mobile)))
      (+ (total-weight (branch-structure lb)) (total-weight (branch-structure rb))))))


(total-weight 
  (make-mobile 
    (make-branch 10 10) 
    (make-branch 12 (make-mobile 
		      (make-branch 10 100) 
		      (make-branch 12 102)))))
(define (balanced? mobile)
  (let ((leftBranch (left-branch mobile))
	(rightBranch (right-branch mobile)))
    (let ((leftLength (branch-length leftBranch))
	  (leftStructure (branch-structure leftBranch))
	  (rightLength (branch-length rightBranch))
	  (rightStructure (branch-structure rightBranch)))
      (= (* leftLength (total-weight leftStructure))
	 (* rightLength (total-weight rightStructure))))))

(balanced? 
  (make-mobile 
    (make-branch 10 3.6) 
    (make-branch 12 (make-mobile 
		      (make-branch 10 1) 
		      (make-branch 12 2)))))
