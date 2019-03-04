; The thing is magnitude operation is associated with lower level
; the level whose types are polar and rectangle
;
; So when (magnitude z) gets parsed, (apply-generic) would try to:
; (get 'magnitude '(complex)) to locate procedure, which doesn't exist
; That's why we would have such error message
;
; What Mr Hacker did, put INDEX('magnitude, '(complex)') into the table, 
; whose value is magnitude, which in turn defined as (apply-generic 'magnitude z)
; The key is after this the complex type of z got striped, which makes the type
; of z becomes into rectangle
; With that fact, (apply-generic 'magnitude z) gets parsed into
; (get 'magnitude '(rectangle)) and so on so forth

; (magnitude z)
;     |
;     v
;
; (apply-generic 'magnitude z)
;     |
;     v
;
; ((get 'magnitude '(complex)) (contents z))
;     |
;     v
;
; (magnitude (contents z))
;     |
;     v
;
; (apply-generic 'magnitude (contents z))
;     |
;     v
;
; ((get 'magnitude '(rectangle)) (contents (contents z)))
;     |
;     v
;
; ((lambda (z) (sqrt (+ (square (real-part z)) (square (imag-part z))))) (contents (contents z)))
;
