
; The issue with this proposal is for exchange, we would have two serializer outside
; and serializers inside for individual withdrawals and deposits which is not necessary
; But I don't think it will cause any problem
; because two exchange involving same bank account will be executed serializedly anyway
;


; on schemewiki erben said 
; According to the implementation of the serializer, if a process has already acquired a mutex and it wants to acquire that mutex again, the busy waiting will never halt.
;
; Then this implementation is not smart enough.
