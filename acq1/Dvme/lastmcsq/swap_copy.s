*****************************************************************************
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
*    File:         /usr/users/mcsq/Dvme3/swap_copy.s
*
*    Description:  This routine swaps bytes and copyies ethernet packets.
*                  The ethernet header(first 20 bytes) is copied without
*                  modification.  The data part of the packet is byte
*                  swaped.  This is very specific to the data acquisition
*                  system.
*
*                  WARNING:  Any changes to the ethernet packet structure
*                            WILL require changes to this routine!
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/ 5/93    MCSQ         
******************************************************************************
		SECTION	9
*******************************************************************************
*
*      void swap_copy_(char * dst, char *src, char* src_end)
*
*   Call:    dst   - Destination starting address
*            src   - Source starting address
*          src_end - Source ending address
*******************************************************************************
swap_copy_:
		MOVEM.L	D2-D3/A2,-(SP)	;Save some registers
		MOVE.L	16(SP),A0	;Get destination starting address
		MOVE.L	20(SP),A1	;Get source start address
		MOVE.L	24(SP),A2	;Get source ending address
		MOVE.L	#$FF00FF00,D2	;Setup our byte swap masks
		MOVE.L	D2,D3
		NOT.L	D3
		MOVE.L	(A1)+,(A0)+	;Copy ethernet header without
		MOVE.L	(A1)+,(A0)+	;modification.  Header is 20 bytes.
		MOVE.L	(A1)+,(A0)+
		MOVE.L	(A1)+,(A0)+
		MOVE.L	(A1)+,(A0)+
*
SWAP_COPY:
		MOVE.L	(A1)+,D0	;Get a long word
		MOVE.L	D0,D1		;Make another copy
		AND.L	D2,D0		;Mask off two bytes and
		LSR.L	#8,D0		;shift into place
		AND.L	D3,D1		;Mask off other two bytes
		LSL.L	#8,D1		;and put into place
		OR.L	D1,D0		;Combine to form new long word
		MOVE.L	D0,(A0)+	;with swaped bytes and store
		CMPA.L	A1,A2		;Check for finished
		BHI	SWAP_COPY
*
		MOVEM.L	(SP)+,D2-D3/A2	;Restore registers
		RTS

		SECTION 14
		XDEF    swap_copy_
		END
