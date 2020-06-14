	SECTION	9
******************************************************************************
*
*  Routine to test for hadware device present.  Try to read a location
*  and if we do not get a Bus Error, we conclude that the device is
*  present.
*
*  int device_test_(int size,void *loc)
*
*  Call:  size  - access size: 1 = byte, 2 = word, and 4 = long word
*         loc   - address to access
*
* Return:  0 - means access caused Bus Error
*          1 - means access successful
*
* Revisions:
*
*  3/ 3/97  MCSQ     Fix to work with either a 68040 or 68060.
*                    Stack frames for bus error have different format
*                    for 68040 and 68060.
*****************************************************************************
	
device_test_:
	MOVE.L	4(SP),D0	;Get test access size in bytes
	MOVEA.L	8(SP),A0	;Get address to test
	MOVEA.L	$8,A1		;Save normal Bus Fault handler address
	MOVE.L	#BUSS_ERR,$8	;Put in our handler
	CMPI.B	#1,D0		;Check access size
	BNE.S	DEV1		;Not byte
	MOVE.B	(A0),D0		;Try byte access
	MOVE.L	A1,$8		;Restore normal Bus Fault handler
	MOVEQ	#1,D0		;Device access Ok
	RTS
*
DEV1:
	CMPI.B	#2,D0		;Word access?
	BNE.S	DEV2		;No.
	MOVE.W	(A0),D0		;Yes. Try access
	MOVE.L	A1,$8		;Restore normal Bus Fault handler
	MOVEQ	#1,D0		;Device access Ok
	RTS
*
DEV2:
	MOVE.L	(A0),D0		;Try long word access
	MOVE.L	A1,$8		;Restore normal Bus Fault handler
	MOVEQ	#1,D0		;Device access Ok
	RTS
*
BUSS_ERR:
	MOVE.W	(SP),D0		;Save interrupt SR
	MOVE.W	6(SP),D1	;Get interrupt vector
	ANDI.W	#$FFF,D1
	SWAP	D1
	MOVE.W	6(SP),D1	;Get interrupt vector again
	ANDI.W	#$F000,D1	;Extract stack frame format
	CMPI.W	#$7000,D1	;Test for 68040
	BNE.S	BUSS_ERR1	;Must be 68060
	ADDA.W	#44,SP		;68040 has 30 word stack
*				;68060 has 8 word stack
BUSS_ERR1:
	ADDA.W	#16,SP		;Cleanup stack
	SWAP	D1
	MOVE.W	D1,-(SP)	;Prepare for exception return
	MOVE.L	#DEV_ERR,-(SP)
	MOVE.W	D0,-(SP)
	RTE
*
DEV_ERR:
	MOVE.L	A1,$8		;Restore normal Bus Fault handler
	MOVEQ	#0,D0		;Trial access Failed!!
	RTS
	
	SECTION	14
	SECTION	9
	SECTION	14
	XDEF	device_test_
	SECTION	9
	END
