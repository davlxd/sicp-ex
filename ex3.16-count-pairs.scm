(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(count-pairs '())
(count-pairs '(a))
(count-pairs '(a b))


(count-pairs '(a b c) )
;  C3 ---> O  O ---> O  O ----> O  X
;          |         |          |
;          a         b          c
;



; Experiment:

(define ab (cons 'a 'b))
ab
(count-pairs ab) ; 1

(define abab (cons ab ab))
abab
(count-pairs abab) ; 3

(define abababab (cons abab abab))
abababab
(count-pairs abababab) ; 7

(define cabab (cons 'c abab))
cabab
(count-pairs cabab) ; 4
; ----
(define c-null (cons 'c '())) ; aka '(c)
c-null
(count-pairs c-null)
(define c-null-c-null (cons c-null c-null))
c-null-c-null
(count-pairs c-null-c-null)
(define c-null-c-nullx2 (cons c-null-c-null c-null-c-null))
c-null-c-nullx2
(count-pairs c-null-c-nullx2)




; Count as 4 one

(define c (cons 'c '()))
(define C4 (cons 'a (cons c c)))
C4

(count-pairs '(a (c) c) )
;Value: 4

(count-pairs C4)
;Value: 4

;  C4 ---> O  O ---> O  O ----> O  X
;          |         |       ^  |
;          a         |       |  c
;                    +-------+


; Count as 4 two

(define ab (cons 'a 'b))
(define C4 (cons 'c (cons ab ab)))
C4

(count-pairs C4)
;Value: 4

;  C4 ---> O  O ---> O  O ----> O  O
;          |         |       ^  |  |
;          |         |       |  |  |
;          c         |       |  a  b
;                    +-------+




; Count as 7 one

(define c-null (cons 'c '())) ; aka '(c)
(define c-null-c-null (cons c-null c-null)) ; '((c) c)
(define C7 (cons c-null-c-null c-null-c-null)) ; '(((c) c) (c) c)
C7
(count-pairs C7) ;7
(count-pairs  '(((c) c) (c) c)) ;7
;
;  C4 ---> O  O ---> O  O ----> O  X
;          |      ^  |       ^  |
;          |      |  |       |  c
;          |      |  |       |
;          +------+  +-------+




; Count as 7 two

(define ab (cons 'a 'b))

(define abab (cons ab ab))
abab
(count-pairs abab) ; 3

(define C7 (cons abab abab))
C7
(count-pairs C7) ; 7
;
;  C4 ---> O  O ---> O  O ----> O  O
;          |      ^  |       ^  |  |
;          |      |  |       |  |  |
;          |      |  |       |  a  b
;          +------+  +-------+







(define cyclic-abc '(a b c))
(set-cdr! (cddr cyclic-abc) cyclic-abc)
; cyclic-abc  ; this never ends
(count-pairs cyclic-abc) ; this never ends as well


;
; C ----> O O ------> O O -------> O --------+
;     ^   |           |            |         |
;     |   v           v            v         |
;     |  'a          'b           'c         |
;     |                                      |
;     +--------------------------------------+


