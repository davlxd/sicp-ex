; For generic operations with explicit dispatch
; when a new type is to be added:
; A list of type specific operations with new suffix need to be implemented
; And a new condition needs to be added to generic dispatch for each generic operation

; when a new operation is to be added:
; A new operation need to be added to every type specific operations
; And a new generic operation needs to be implemented with conditional dispatch inside





; For Data-directed style
; when a new type is to be added:
; A new install-XXX-package needs to be implemented, with every existing operation inside

; when a new operation is to be added:
; For every existing install-XXX-package procedures, new operation needed to be implemented and 
; a new (put ...) needs to be put as well





; For messaging-passing
; when a new type is to be added:
; A new make-from-XXX needs to be implemented

; when a new operation is to be added:
; For every existing make-from-XXX, new operation needed to be added




; So generic operation with explict dispath is always a bad idea,
; For data-directed style and message-passing, they are basically the same thing
; it's just message-passing omits table interation part, hence neater

; For exsiting implementation, both d-d m-p involve modify existing code if new operation is to be added
; So if we want to fix this, we need to swap the axis, and that's impossible for m-p, but for d-d:


(define (install-real-part)
  (define (real-part-rect z) (car z))
  (define (real-part-polar z)
    (* (magnitude z) (cos (angle z))))
  (put 'real-part '(rectangular) real-part-rect)
  (put 'real-part '(polar) real-part-polar)
  'done)

(define (install-imag-part)
  (define (imag-part-rect z) (cdr z))
  (define (imag-part-polar z)
    (* (magnitude z) (sin (angle z))))
  (put 'imag-part '(rectangular) imag-part-polar)
  (put 'imag-part '(polar) imag-part-polar)
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	(apply proc (map contents args))
	(error
	  "No method for these types -- APPLY-GENERIC"
	  (list op type-tags))))))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
