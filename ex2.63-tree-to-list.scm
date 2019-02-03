; a. Yes they produce the same result, in mid-order, result is 1 3 5 7 9 11
; b. if not count append which is a O(n) operation, both would be O(n), however
;    if counting append, 
;      for tree->list-1: function call n times, cons n times, append cost = cons n times
;      for tree->list-2: function call n times, cons n times.
;    so apparently tree->list-2 grows slower
;
;


