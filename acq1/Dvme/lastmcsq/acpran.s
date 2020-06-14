* fcom  -O -X20 -X22 -X29 -X33 -X35 -X66 -X78 -X99 -X130 -X140
	SECTION	9
	XDEF	acpran
acpran:
          MOVEA.L     $0004(SP),A1          !
          MOVE.L      (A1),D0               !
          MULU.L      #$00010DCD,D0         !
          ADDQ.L      #1,D0                 !
          MOVE.L      D0,(A1)               !
          LSR.L       #1,D0                 !
	RTS
	SECTION	14
* allocations for acpran_
*	A0	iseed
*	4(SP)	iseed
	SECTION	9
	SECTION	14
* allocations for module
	SECTION	9
	END
