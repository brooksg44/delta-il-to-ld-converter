; LDmicro Style Test IL code
; Test various OR logic patterns

; Simple OR
LD X0
OR X1  
OUT Y0

; AND then OR (common pattern)
LD X2
AND X3
OR X4
OUT Y1

; Multiple OR branches
LD X5
OR X6
OR X7
OR X8
OUT Y2

; Complex with normally closed
LDI X9
AND X10
ORI X11
OUT Y3