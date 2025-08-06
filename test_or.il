; Test IL code for OR logic
LD X0
AND X1
OR X2
OUT Y0

; Simple OR
LD X3
OR X4
OUT Y1

; Multiple OR branches
LD X5
OR X6
OR X7
OR X8
OUT Y2

; Complex mixed logic
LDI X9
AND X10
OR X11
AND X12
OUT Y3