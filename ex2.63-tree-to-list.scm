; a. Yes they produce the same result, in mid-order, result is 1 3 5 7 9 11
; b. if not count append which is a O(n) operation, both would be O(n), however
;    if counting append, 
;      for tree->list-1: function call n times, cons n times, append cost = cons n times
;      for tree->list-2: function call n times, cons n times.
;    so apparently tree->list-2 grows slower
;
;
;
;
; Correction:
; For tree->list-1: 
; T(n) = 2*T(n/2) + O(n/2) (as the procedure append takes linear time)
; Solving above equation, we get T(n) = O(n * log n)
;                         4(O(2))                   sum: O(2)
;           2(O(1))                2(O(1))          sum: O(2)
;        1(O(0))    1(O(0))   1(O(0))    1(O(0))    sum: O(2)
;                                                   total: O(n/2 * lgn)
; 
; For tree->list-2:
; T(n) = 2*T(n/2) + O(1)
; Solving the above equation, we get T(n) = O(n)

