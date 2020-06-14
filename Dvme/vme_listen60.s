*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    Environment:  Force CPU-60 and VMEPROM
*
*    File:         /usr/users/mcsq/Dvme3/vme_listen60.s
*
*    Description:  Ethernet listener.  Displays all packets received by the
*                  VME processor.  Useful for troubleshooting the connection
*                  between a host and the VME via private Ethernet.  Will
*                  not be very useful if the VME processor is connected
*                  to a public Ethernet.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/18/92    MCSQ         
*
*    1/ 2/93    MCSQ       Made the code position independent so that it
*                          can be stored in nonvolatile RAM and moved
*                          to DRAM for execution.
*
*    5/25/93    MCSQ       Add startup message.  Add startup option for
*                          mode of Lance controller.  Start at LISTEN for
*                          promiscuous mode and start at LISTENA for
*                          normal mode.
*
*    2/27/97    MCSQ       Modified vme_listen.s for the CPU-40 to run with
*                          the new CPU-60s.
*******************************************************************************
		ORG   $FFC09000
*    ORG	$80000
*
		INCLUDE	vmeprom.inc
*
CR		EQU     $0D
LF		EQU     $0A
*
*******************************************************************************
*******************************************************************************
LISTEN: 
		MOVE.W	#$8000,D0	;Promiscuous mode
		BRA.S	LISTENB
LISTENA:
		MOVEQ.L	#0,D0		;Normal mode
LISTENB:
		BSR.W	LAN_STOP
*
*******************************************************************************
*
*   Initialization and Main Loop
*
*******************************************************************************
STR:
		LEA	LAN_MODE(PC),A0
		MOVE.W	D0,(A0)
		MOVE.W  #$8000,D0       ;Set RAM shared by CPU and LANC
		MOVE.W  #$FFFF,D1       ;controller to all ones.
		MOVE.L  #LAN_RAM,A0
IPL             MOVE.W  D1,(A0)+
		SUBQ.W  #1,D0
		BNE     IPL
		LEA     REC_CNT(PC),A0  ;Clear record counter.
		CLR.W   (A0)
		LEA	START_MESS(PC),A1
		BSR	CRLF
		BSR	OUT_TXT
		BSR	CRLF
*
*    Main Listen loop
*
MAIN            BSR     LAN_INIT        ;Initialize LANC controller
		BSR.S   CRLF
MAIN1           BSR     LAN_READ        ;Get a packet
		BSR.S   LIST_BUF        ;List contents of packet on console
		BSR     LAN_READ_ADV    ;Release buffer for futher use
		BRA     MAIN1
*
*******************************************************************************
*   Output a carraige return and line feed to console
*******************************************************************************
CRLF            MOVE.B  #CR,D0
		OUT_CHR
		MOVE.B  #LF,D0
		OUT_CHR
		RTS
*
*******************************************************************************
*  Convert one byte to ASCII Hex and output to console
*
*  Call:    D0 = byte to convert and output
*******************************************************************************
OUT_BYTE        MOVE.W  D0,-(SP)
		LSR     #4,D0
		ANDI.B  #$F,D0
		ADD.B   #'0',D0
		CMP.B   #'9',D0
		BLE.S   OUT_BYTE1
		ADD.B   #7,D0
OUT_BYTE1       OUT_CHR
		MOVE.W  (SP)+,D0
		ANDI.B  #$F,D0
		ADD.B   #'0',D0
		CMP.B   #'9',D0
		BLE.S   OUT_BYTE2
		ADD.B   #7,D0
OUT_BYTE2       OUT_CHR
		RTS
*
*******************************************************************************
*  Output a text string to the console.  End of string marked by a zero byte.
*
*  Call:   A1 - pointer to string
*******************************************************************************
OUT_TXT         MOVE.B  (A1)+,D0
		BEQ.S   OUT_TXT1
		OUT_CHR
		BRA     OUT_TXT
*
OUT_TXT1        RTS
*
*******************************************************************************
*  List contents of a received packet.
*
*  Call:   A0  - pointer to packet
*          D1  - packet byte count
*******************************************************************************
LIST_BUF        LEA     REC_MSG1(PC),A1 ;Output record counter
		BSR     OUT_TXT
		LEA     REC_CNT(PC),A3  ;Get address of record counter
		MOVE.B  (A3)+,D0        ;Convert counter to hex and
		BSR.S   OUT_BYTE        ;output to console.
		MOVE.B  (A3)+,D0
		BSR.S   OUT_BYTE
		BSR.S   CRLF
		ADD.W   #1,-(A3)        ;Increment record counter
		LEA     REC_MSG2(PC),A1 ;List Destination address
		BSR     OUT_TXT
		MOVEQ   #6,D3           ;First six bytes of packet
LIST_BUF1       MOVE.B  (A0)+,D0
		BSR.S   OUT_BYTE
		SUBQ.B  #1,D3
		BEQ.S   LIST_BUF2
		MOVE.B  #'-',D0
		OUT_CHR
		BRA     LIST_BUF1
*
LIST_BUF2       BSR     CRLF
		LEA     REC_MSG3(PC),A1 ;List Source address
		BSR     OUT_TXT
		MOVEQ   #6,D3           ;Second six bytes of packet
LIST_BUF3       MOVE.B  (A0)+,D0
		BSR.S   OUT_BYTE
		SUBQ.B  #1,D3
		BEQ.S   LIST_BUF4
		MOVE.B  #'-',D0
		OUT_CHR
		BRA     LIST_BUF3
*
LIST_BUF4       BSR     CRLF
		LEA     REC_MSG4(PC),A1 ;Protocol word
		BSR     OUT_TXT
		MOVE.B  (A0)+,D0
		BSR     OUT_BYTE
		MOVE.B  #'-',D0
		OUT_CHR
		MOVE.B  (A0)+,D0
		BSR     OUT_BYTE
		BSR     CRLF
		LEA     REC_MSG5(PC),A1 ;Output buffer length
		BSR     OUT_TXT
		MOVE.W  D1,D0
		LSR.W   #8,D0
		BSR     OUT_BYTE        ;output to console.
		MOVE.W  D1,D0
		BSR     OUT_BYTE
		BSR     CRLF
		SUB.W   #14,D1          ;Calculate remaining bytes
LIST_BUF5       MOVE.L  A0,A2
		CLR.L   D5
		MOVE.W  #16,D3          ;Remainder of packet is data.
		CMP.W   D3,D1           ;Output a most 16 bytes per line.
		BGE.S   LIST_BUF6 
		MOVE.W  D3,D5
		SUB.W   D1,D5
		MULU    #3,D5
		MOVE.W  D1,D3
LIST_BUF6       MOVE.W  D3,D4
LIST_BUF7       MOVE.B  (A0)+,D0        ;Output upto 16 bytes in hex
		BSR     OUT_BYTE
		MOVE.B  #' ',D0
		OUT_CHR
		SUBQ.B  #1,D3
		BNE     LIST_BUF7
LIST_BUFA       MOVE.B  #' ',D0
		OUT_CHR
		SUBQ.W  #1,D5
		BGE     LIST_BUFA
		MOVE.L  A2,A0
		MOVE.B  D4,D3
LIST_BUF8       MOVE.B  (A0)+,D0        ;Output same bytes as ASCII chars
		CMP.B   #' ',D0
		BLT.S   LIST_BUF9
		CMP.B   #'z',D0
		BLT.S   LIST_BUF10
LIST_BUF9       MOVE.B  #'.',D0         ;Output nonprinting chars as periods
LIST_BUF10      OUT_CHR
		SUBQ.B  #1,D3
		BNE     LIST_BUF8
		BSR     CRLF
		SUB.W   #16,D1          ;reduce packet byte count.
		BGT     LIST_BUF5       ;More bytes to list?
		BSR     CRLF
		RTS
*
*******************************************************************************
*  
*******************************************************************************
START_MESS	DC.B	'Ethernet listen',0
REC_MSG1        DC.B    'Received Record #',0
REC_MSG2        DC.B    'Destination Addr ',0
REC_MSG3        DC.B    'Source Addr      ',0
REC_MSG4        DC.B    'Protocol         ',0
REC_MSG5        DC.B    'Buffer Length    ',0
REC_CNT         DC.W    0
LAN_ADDR	EQU	$FFC08002
LAN_HASH        DC.B    0,0,0,0,0,0,0,0
LAN_MODE        DC.W    $8000
AD60            DC.B    $00,$80,$42,$0d,$03,$62
	ALIGN   2
*******************************************************************************
*
*  Am79C965 LAN controller and data buffer parameters
*
*******************************************************************************
NUM_RBUF        EQU     16              ;Number of receiver buffers
NUM_TBUF        EQU     16              ;Number of transmit buffers
RBUF_SIZE       EQU     1536            ;Size of receive buffer
TBUF_SIZE       EQU     1536            ;Size of transmit buffer
LANC            EQU     $FFF00010       ;Am79C965 LAN controller base address
RAP             EQU     0		;Register address offset
RDP             EQU     2		;Register data offset
BDP             EQU     4		;Bus config data offset
RST		EQU	6		;Reset register offset
LAN_RAM         EQU     $003e0000       ;CPU Address of shared RAM
LAN_IBLK        EQU     0               ;Offset of initalize block in shared RAM
LAN_R_RING      EQU     $80             ;Offset of receive ring from init block
LAN_T_RING      EQU     LAN_R_RING+(NUM_RBUF*8) ;Offset of transmit ring
LAN_BUFS        EQU     LAN_T_RING+(NUM_TBUF*8) ;Offset of data buffers
*
RBUF_PTR        EQU     $003e0040
RBUF_PTR_STR    EQU     $003e0044
RBUF_PTR_END    EQU     $003e0048
TBUF_PTR        EQU     $003e004C
TBUF_PTR_STR    EQU     $003e0050
TBUF_PTR_END    EQU     $003e0054
*
*******************************************************************************
*   Stop Am79C965 LAN controller
*******************************************************************************
LAN_STOP	MOVE.L  #LANC,A3
		MOVE.W	#0,(A3)
		MOVE.W	#4,RDP(A3)
		RTS
*******************************************************************************
*
*  Initialize Am79C965 LAN controller.
*
*  1) Build initialization block for Am79C965 LAN controller
*
*  2) Build receive and transmit buffer headers
*
*  3) Start Am79C965 LAN controller
*
*******************************************************************************
LAN_INIT        MOVE.L	#LANC,A3        ;Am79C965 LAN chip base address
		MOVE.W	#0,(A3)		;Select CSR0
		MOVE.W	#4,RDP(A3)      ;Stop LAN controller
		MOVE.W	RST(A3),D0      ;Reset Am79C695 controller
		MOVE.W  #1,(A3)		;Load address of initialization block
		MOVE.L  #LAN_RAM,D0
		ADD.L   #LAN_IBLK,D0
		MOVE.W  D0,RDP(A3) 
		SWAP    D0
		MOVE.W  #2,(A3)
		MOVE.W  D0,RDP(A3)
		SWAP    D0
		MOVE.L  D0,A0
		MOVE.W  LAN_MODE(PC),(A0)+  ;Put in mode word
		MOVEQ   #3,D2
		CLR.L   D0
		MOVE.L	#LAN_ADDR,A1
		MOVE.L	(A1),A1         ;Put in physical address of this
LAN_INIT1       MOVE.B  (A1)+,D1        ;Ethernet controller.  NOTE: A byte
		MOVE.B  (A1)+,D0        ;swap is required only here.  After
		LSL.W   #8,D0           ;initialization, the chip is programmed
		OR.B    D1,D0           ;to do in hardware a byte swap on all
		MOVE.W  D0,(A0)+        ;data buffers.
		SUBQ.B  #1,D2
		BNE     LAN_INIT1
		MOVEQ   #4,D2
		LEA	LAN_HASH(PC),A1	;Put the HASH filter 64 bit word in
LAN_INIT2       MOVE.B  (A1)+,D1        ;the initialization block.  A byte
		MOVE.B  (A1)+,D0        ;swap is also done here.
		LSL.W   #8,D0
		OR.B    D1,D0
		MOVE.W  D0,(A0)+
		SUBQ.B  #1,D2
		BNE     LAN_INIT2
		CLR.L   D1
		MOVE.W  #NUM_RBUF,D0    ;Format the control word for number
LAN_INIT3       LSR.W   #1,D0           ;of receive buffers.  The most
		BEQ.S   LAN_INIT4       ;significant 3 bits are number of
		ADDQ.W  #1,D1           ;buffers expressed as a power of 2.
		BRA     LAN_INIT3
*
LAN_INIT4       LSL.W   #5,D1		;Build the receive ring descriptors
		LSL.W   #8,D1
		MOVE.L  #LAN_RAM,D0
		ADD.L   #LAN_R_RING,D0
		MOVE.W  D0,(A0)+
		SWAP    D0
		ANDI.W	#$00FF,D0
		OR.W    D1,D0
		MOVE.W  D0,(A0)+
		CLR.L   D1
		MOVE.W  #NUM_TBUF,D0
LAN_INIT5       LSR.W   #1,D0
		BEQ.S   LAN_INIT6
		ADDQ.W  #1,D1
		BRA     LAN_INIT5
*
LAN_INIT6       LSL.W   #5,D1		;Build the transmit ring descriptors
		LSL.W   #8,D1
		MOVE.L  #LAN_RAM,D0
		ADD.L   #LAN_T_RING,D0
		MOVE.W  D0,(A0)+
		SWAP    D0
		ANDI.W	#$00FF,D0
		OR.W    D1,D0
		MOVE.W  D0,(A0)+
		MOVE.L  #6,D1		;Do a word swap on the initialization
		MOVE.L  #LAN_RAM,A0	;block
LAN_INIT7       MOVE.L  (A0),D0
		SWAP    D0
		MOVE.L  D0,(A0)+
		SUBQ    #1,D1
		BNE     LAN_INIT7
*
*   Build the receive and transmit ring structures
*
                MOVE.L  #LAN_RAM,D1
		ADD.L   #LAN_BUFS,D1    ;Offset of beginning of data buffers
		MOVE.L  #LAN_R_RING,A0  ;Offset of start of receive ring
		ADD.L   #LAN_RAM,A0     ;CPU address of start of shared memory
		MOVE.L  A0,RBUF_PTR_STR ;Init pointers to receive ring for
		MOVE.L  A0,RBUF_PTR     ;buffer management routines.
		MOVE.L  #-RBUF_SIZE,D2  ;Get size of each buffer and
		MOVE.W  #NUM_RBUF,D3    ;the number of receive buffers.
LAN_INIT8	MOVE.L	D1,D0
		MOVE.W  D1,(A0)+
		SWAP    D1
		ANDI.W	#$00FF,D1
		OR.W    #$8000,D1       ;Mark receive buffer available
		MOVE.W  D1,(A0)+
		MOVE.L  D0,D1
		SUB.L   D2,D1
		MOVE.W  D2,(A0)+
		CLR.W   (A0)+
		SUBQ.W  #1,D3
		BNE     LAN_INIT8
		MOVE.L  A0,RBUF_PTR_END ;Init buffer pointers for memory
		MOVE.L  A0,TBUF_PTR     ;manager
		MOVE.L  A0,TBUF_PTR_STR
		MOVE.L  #-TBUF_SIZE,D2
		MOVE.W  #NUM_TBUF,D3    ;Build transmit ring.
LAN_INIT9	MOVE.L	D1,D0
		MOVE.W  D1,(A0)+
		SWAP    D1
		ANDI.W	#$00FF,D1
		OR.W    #$300,D1
		MOVE.W  D1,(A0)+
		MOVE.L	D0,D1
		SUB.L   D2,D1
		MOVE.W  D2,(A0)+
		CLR.W   (A0)+
		SUBQ.W  #1,D3
		BNE     LAN_INIT9
		MOVE.L  A0,TBUF_PTR_END
		MOVE.L  #NUM_RBUF,D1
		ADD.L   #NUM_TBUF,D1
		LSL.L   #1,D1
		MOVE.L  #LAN_RAM,D0
		ADD.L   #LAN_R_RING,D0
		MOVE.L  D0,A0
LAN_INIT10      MOVE.L  (A0),D0		;Word swap the ring buffer
		SWAP    D0		;descriptors
		MOVE.L  D0,(A0)+
		SUBQ    #1,D1
		BNE     LAN_INIT10
                
		MOVE.W  #0,(A3)
		MOVE.W  #1,RDP(A3)      ;Initialize Am79C965 LAN controller
LAN_INIT11      MOVE.W  RDP(A3),D1      ;Wait till done
                MOVE.W  D1,D0
		ANDI.W  #$100,D0
		BNE     LAN_INIT12
		MOVE.W  D1,D0
		ANDI.W  #$800,D0
		BEQ     LAN_INIT11
		TRAP    #4
		ADDQ.W  #1,D0
		MOVE.W  D0,RDP(A3)
		BRA     LAN_INIT11
*
LAN_INIT12	MOVE.W  #3,(A3)		;Select and load CSR3
		MOVE.W  #4,RDP(A3)      ;Big Ingine mode
		MOVE.W  #0,(A3)
		MOVE.W  #$102,RDP(A3)   ;Clear Init done and start Am79C965 LAN
		MOVE.W  RDP(A3),D1
		RTS
*******************************************************************************
*
*  Returns a pointer to a ready receiver buffer in A0 with the byte
*  count in D1.  The data buffer does not include addresses or CRC characters.
*
*******************************************************************************
LAN_READ        MOVE.L  RBUF_PTR,A0     ;Get address of current buffer
		MOVE.W  (A0),D0         ;Buffer ready?
		BPL.S   LAN_READ1       ;Yes. Return it to user
		BRA     LAN_READ
*
LAN_READ1       MOVE.L  #LAN_RAM,D0     ;Calculate address of packet data
		ADD.W   2(A0),D0
		MOVE.L	D0,-(SP)
		MOVE.W  4(A0),D1        ;Get buffer byte count
		SUB.W   #4,D1           ;Delete CRC bytes
		MOVE.L  #LANC,A0        ;Clear controller status
		CLR.W   (A0)
		MOVE.W  RDP(A0),D0
		AND.W	#$7F00,D0
		MOVE.W  D0,RDP(A0)
		MOVE.L  (SP)+,A0        ;Get pointer to protocol word
		RTS
*******************************************************************************
*
*   Routine releases current buffer for future use by the Am79C965 LAN chip
*
*******************************************************************************
LAN_READ_ADV    MOVE.L  RBUF_PTR,A0
		MOVE.B  #$80,(A0)
		ADD.W   #8,A0
		CMP.L   RBUF_PTR_END,A0
		BLT.S   LAN_READ_ADV1
		MOVE.L  RBUF_PTR_STR,A0
LAN_READ_ADV1   MOVE.L  A0,RBUF_PTR
		RTS
		END
