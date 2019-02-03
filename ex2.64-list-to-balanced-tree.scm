; a. Firstly pick up (quotient (- n 1) 2) elements as left branch, 
;    resursively invoking this function to build left branch tree,
;    since partial-tree can give us remaining element, so from the remaining element we can
;      get entry
;      get elements needed to build right branch tree
;      remaining elements
;    once right branch tree was built, we make the tree, return along with remaining elements.
;
; (len '(1 3 5 7 9 11)) = 6
; (quotient 5 2) = 2
; (quotient 2 2) = 1
; (quotient 1 2) = 0
; so:
;               5
;            /     \
;           1      9
;            \    / \
;             3  7  11
;
;
; b. O(n)
;


