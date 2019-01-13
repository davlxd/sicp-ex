; Exercise 2.52.  Make changes to the square limit of wave shown in figure 2.9 by working at each of the levels described above. In particular:

; a.  Add some segments to the primitive wave painter of exercise  2.49 (to add a smile, for example).

; this would be continuation of ex2.49

; b.  Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter left)
                  (below right corner)))))

; c.  Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)
