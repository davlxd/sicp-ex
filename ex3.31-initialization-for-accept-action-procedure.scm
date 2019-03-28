;
; The initialization will set the signal value of output wires of gates to initial values
; based on gate logics, before any signal fluctruations happens
;
; It's necessary because that's the way make it sense, so initial values comply with gate logics
;
;
; Without initialization:

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
sum 0  New-value = 0  ; <- nothing happens here because initial probe not triggered
(probe 'carry carry)
carry 0  New-value = 0 ; <- nothing happens here because of same reason



(half-adder input-1 input-2 sum carry)
ok
(set-signal! input-1 1)
done
(propagate)
sum 8  New-value = 1 ; <- nothing happens here because 2nd input wire of last add-gate of half adder is 0, so sum remains 0
done


(set-signal! input-2 1)
done
(propagate)
carry 11  New-value = 1  ; <- This remains
sum 16  New-value = 0 ; <- this would be gone because 2nd input wire of last add-gate of half adder still 0
done
