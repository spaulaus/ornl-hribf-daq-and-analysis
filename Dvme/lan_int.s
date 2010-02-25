*****************************************************************************        
*
*    Ethernet interrupt service routine
*
*    1) Save the LANCE status for later processing
*    2) Check for reboot request (special protocol word) and reboot
*       if so requested.  This is the main reason that the LANCE interrupt
*       is a CPU interrupt level 7 (highest priority).
*    3) Set event flag #56 to signal that the interrupt should be processed
*       by the ethernet driver.
*
*   3/10/92   MCSQ.
****************************************************************************
	SECTION 9
	
lan_int_:
	MOVEM.L D0-D1/A0,-(SP)  ;Save registers we use
	MOVE.B  #128,$FFD00498  ;Clear FGA interrupt request
	MOVE.L  #$FEF80000,A0   ;Address of LANCE controller
	MOVE.W  2(A0),D1        ;Save register address pointer
	CLR.W   2(A0)           ;Select control/status register
	MOVE.W  (A0),D0         ;Get LANCE status
	OR.W    D0,lance        ;Save status for later processing
	ANDI.W  #$7F40,D0       ;Extract status bits
	MOVE.W  D0,(A0)         ;Clear status flags
	MOVE.W  D1,2(A0)        ;Restore register address pointer
	BTST.L  #12,D0          ;Check for missed packet
	BNE.S   L10             ;We missed one!!!
	ANDI.W  #$400,D0        ;Test for receive interrupt
	BEQ.S   L4              ;Not receive
	MOVE.L  rec_int_ptr,A0  ;Have received a packet
	MOVEQ   #0,D0
	MOVE.W  (A0),D0
	ADDI.L  #$FEF00000,D0   ;Compute packet data address
	MOVE.L  D0,A0           ;Check for a Reboot request
	CMPI.W  #$4F52,12(A0)   ;If protocol word is is $4F52 it is
	BNE.S   L6              ;a request to reboot the CPU40.
	CLR.B   $FFD00E00       ;Reboot this mother!
L6:
	ADDQ.L  #8,rec_int_ptr  ;Increment receive ring pointer
	MOVE.L  rec_ptr_end,D0  ;Check for wrap around
	CMP.L   rec_int_ptr,D0
	BGT.S   L10
	MOVE.L  rec_ptr_str,rec_int_ptr ;Set pointer back to start
L10:
	MOVE.L  rec_int_ptr,A0  ;Test next buffer for availablity
	TST.B   2(A0)           ;If it is in use FREE it for next
	BMI.S   L11             ;receive packet.
	MOVE.B  #$80,2(A0)
L11:
	MOVE.B  #56,D1          ;Set event flag #56
	DC.W    $A046           ;XSEV call
L4:
*       MOVE.B  $FCFF0000,D0
	MOVEM.L (SP)+,D0-D1/A0  ;Restore the registers we used
	RTE
L12:
	DC.L    0
	
	SECTION 14
*       import  lance
*       import  rec_ptr_str
*       import  rec_ptr_end
*       import  rec_int_ptr
	XREF    rec_int_ptr
	XREF    rec_ptr_end
	XREF    rec_ptr_str
	XREF    lance
	XDEF    lan_int_
	SECTION 9
	END
