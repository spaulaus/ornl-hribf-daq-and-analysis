*****************************************************************************        
*
*    DMA controller interrupt service routines
*
*    Set event flag #50 to signal that the interrupt should be processed
*     by the ethernet driver.
*
*   4/ 1/93   MCSQ.
****************************************************************************
	SECTION 9
	
dma_norm_:
	MOVEM.L	D0-D1/A0,-(SP)
	CLR.L	dma_err
	CLR.B	$ffd004b0
	MOVE.B  #50,D1          ;Set event flag #50
	DC.W    $A046           ;XSEV call
	MOVEM.L (SP)+,D0-D1/A0  ;Restore the registers we used
	RTE
	
dma_error_:
	MOVEM.L	D0-D1/A0,-(SP)
	MOVEQ	#1,D0
	MOVE.L	D0,dma_err
	CLR.B	$ffd004b4
	MOVE.B  #50,D1          ;Set event flag #50
	DC.W    $A046           ;XSEV call
	MOVEM.L (SP)+,D0-D1/A0  ;Restore the registers we used
	RTE

	SECTION 14
	XREF    dma_err
	XDEF    dma_norm_
	XDEF    dma_error_
	SECTION 9
	END
