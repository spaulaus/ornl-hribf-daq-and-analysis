******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/vme/bus_fault.s
*
*    Description:  Routines needed to provide 68040 Access Fault recovery
*                  in vmebug.c.  An external function, bus_error_, must
*                  be provided to display the fault address to the user.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/ 1/93    MCSQ         
******************************************************************************
	SECTION	9
******************************************************************************
*
*   void bus_fault_init_(void)
*
*  A call to this function initializes the handler for the 68040 Access
*  Faults.  The function saves the processor mode, user stack pointer,
*  supervisor stack pointer and return program counter.  An Access Fault
*  restores the processor mode, stack pointers and returns to the same
*  location as we did after fault handler setup.  
*
*  The caller must insure that C statement after this call is a proper
*  place to restart the program.
*****************************************************************************
	
bus_fault_init_:
	MOVE.L	(SP),fault_rtn		;Save PC
	MOVEM.L	D0-A6,fault_regs	;Save registers
	MOVE.L	SP,A0
	DC.W	$A02C			;XSUP - Go to super mode
	CMP.L	A0,SP
	ADDA.W	#4,A0
	BNE.S	FAULT_INITA		;Assume user mode call
	MOVE.L	A0,fault_ssp		;Save supervisor stack pointer
	MOVEC	USP,A0
	MOVE.L	A0,fault_usp		;Save user stack pointer
	MOVE.W	#$2000,D0
	BRA.S	FAULT_INITB
*
FAULT_INITA:
	MOVE.L	A0,fault_usp		;Save user stack pointer
	MOVE.L	SP,fault_ssp		;Save supervisor stack pointer
	MOVEQ	#0,D0
FAULT_INITB:
	MOVE.W	D0,fault_sr
	MOVE.L	8,A0			;Save old bus handler address
	MOVE.L	A0,fault_def		;
	MOVE.L	#BUS_ERR,8		;Put in our handler
	MOVEQ	#-1,D0			;Get our task number
	DC.W	$A012			; XRTS
	MOVE.L	D1,fault_tsk		;Save task number
	MOVE.W	fault_sr,D0
	MOVE.W	D0,SR
	RTS
*
******************************************************************************
*
*   void bus_fault_reset_(void)
*
*  This function restores the default Access Fault handler.
*****************************************************************************
bus_fault_reset_:
	MOVE.L	fault_def,D0		;Restore old handler
	BEQ.S	FAULT_CLR
	MOVE.L	D0,8
FAULT_CLR:
	RTS
*
******************************************************************************
*
*    Access Fault Handler
*
*  This funciton handles only Access Faults in the task which called
*  bus_fault_init_.  Access Faults from other tasks are passed to the default
*  handler.
*
*  Some of the stacked fault information is passed to the routine bus_error_.
*  This is, in the case of vmebug.c, a C function which displays the
*  address which caused the fault.
*****************************************************************************
BUS_ERR:
	MOVEM.L	D0-D1,-(SP)
	MOVEQ	#-1,D0
	DC.W	$A012		; XRTS - Get task number
	CMP.L	fault_tsk,D1    ;Is it the task which called bus_fault_init_?
	MOVEM.L	(SP)+,D0-D1
	BNE.S	BUS_ERRC	;No. Pass the fault to the default handler
	MOVEQ	#0,D0		;Yes.  Setup to call bus_error_
	MOVE.W	$C(SP),D0	;special status word
	MOVE.L	D0,-(SP)
	MOVE.L	$18(SP),D1	;fault address
	MOVE.L	D1,-(SP)
	MOVE.L	$A(SP),D1	;pc
	MOVE.L	D1,-(SP)
	MOVE.W	$C(SP),D0	;sr
	MOVE.L	D0,-(SP)
	JSR	bus_error	;Call the C funciton.
	MOVE.L	fault_ssp,SP	;Restore stacks to condition of the call
	MOVE.L	fault_usp,A0	;to bus_fault_init_
	MOVE	A0,USP
	MOVE.W	#8,-(SP)	;Prepare for exception return
	MOVE.L	fault_rtn,-(SP)	;Will return from this exception in the
	MOVE.W	fault_sr,-(SP)	;state as the return from bus_fault_init_
	MOVEM.L	fault_regs,D0-A6
	RTE
*
BUS_ERRC:
	MOVE.L	fault_def,-(SP)	;Fault not in our task. Send it to the
	RTS			;default handler.
	
******************************************************************************
*
*   Routine for testing the code.  This is a C callable routine which
*   returns all the stack pointers in the 68040.
*
*   CAUTION:  This function MUST be called in 68040 user mode!
******************************************************************************
stacks_:
	MOVE.L	4(SP),A0
	MOVE.L	SP,(A0)
	MOVE.L	SP,A0
	DC.W	$A02C
	MOVE.L	8(A0),A1
	MOVE.L	SP,(A1)
	MOVE.L	12(A0),A1
	MOVEC	MSP,D0
	MOVE.L	D0,(A1)
	MOVE.L	16(A0),A1
	MOVEC	ISP,D0
	MOVE.L	D0,(A1)
	ANDI.W	#$FFF,SR
	RTS

	SECTION	14
fault_def:	DC.L	0
fault_rtn:	DC.L	0
fault_regs:	DS.L	15
fault_ssp:	DC.L	0
fault_usp	DC.L	0
fault_tsk:	DC.L	-1
fault_sr:	DC.W	0
	SECTION	9
	SECTION	14
	XREF	bus_error
	XDEF	bus_fault_init_
	XDEF	bus_fault_reset_
	XDEF	stacks_
	SECTION	9
	END
