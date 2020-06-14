*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1996
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
*    Environment:  Force CPU-40 with Eagle 01 and VMEPROM
*
*    File:         /usr/users/mcsq/Dvme/vme_boot.s
*
*    Description:  Power-up bootstrap loader
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/21/92    MCSQ        Original
*
*    2/23/92    MCSQ        Make the loader a new task under VMEPROM.
*
*    3/20/92    MCSQ        Change to new protocol word for boot request
*                           message.  After a download, setup an interrupt
*                           handler for the Ethernet.  This handler only
*                           looks for reboot requests from the host.  Put
*                           a pointer to our hardware ethernet address in
*                           a fixed location for reference by other codes.
*                           All other codes should get the hardware address
*                           from here.
*
*    4/17/92    MCSQ        In the beginning, the packet protocol word was
*                           0x6006 and S record data followed this word.
*                           This format is still in use but more addvanced
*                           host software now uses a different protocol
*                           with additional header info.  Changes here
*                           detect the new protocol word and provide proper
*                           pointers to the S record decoder.  Also corrected
*                           error in S record decoder to allow records to
*                           begin with lower case s.     
*
*    5/14/92   MCSQ         Padded IPL message so that all the text is in the 
*                           data part of the packet for our current protocol.
*
*    6/29/92   MCSQ         Make code position independent so that FLASH EPROM
*                           can be used as a backup storage for this loader.
*                           NOTE:  The ethernet interrupt handler, which is
*                           initalized after a download, is NOT position
*                           independent!!  If the SRAM version is corrupted,
*                           the FLASH EPROM version should be used to restore
*                           the code in SRAM.
*
*   10/31/92   MCSQ         Prefix the 'GO XXXXXXXX' command with an ASCII
*                           escape character.  The escape forces VMEPROM
*                           to exit commands such as 'md' etc. so that
*                           the GO command will be processed correctly.
*
*    3/20/93   MCSQ         Ignore packets which do not have one of the
*                           two protocols used by the loader.
*
*    4/16/93   MCSQ         Boot request packets are Broadcast so any
*                           available host can receive then and respond.
*
*    4/30/93   MCSQ         The Ethernet address of the host which loaded
*                           VME system is now saved in nonvolatile RAM
*                           as our default host.  Change boot request from
*                           a Broadcast to a Multicast.
*
*    5/12/94   MCSQ         This one is for a new processor, vme3.
*
*    9/ 7/94   MCSQ         This one is for a new processor, vme4.
*
*    1/20/95   MCSQ         The original version of the boot loader assumed
*                           that the VME processor could process S record
*                           packets faster than the host could send them.
*                           That now appears to be a bad assumption.  This
*                           version allows the host to request an acknowlegement
*                           packet for each packet the host sends.  The
*                           handshake will slow the host to match the VME.
*                           In addition, transmission errors can be handled
*                           since the VME can provide a positive or negative
*                           (ACK or NAK) to each packet.  If the host receives
*                           a NAK it retransmits the packet received in error.
*                           All of this is compatable with old versions of
*                           vmeboot and srload.  Those versions do not request
*                           an acknowlege and the VME sends none.  Therefore,
*                           to get the benifits of the new boot loader, you
*                           must upgrade to new versions of both vmeboot
*                           and srload.
*
*                           S7 records have a 32-bit start address and S8 
*                           records have a 24-bit start address.  The original
*                           code had S7 and S8 records interchanged.
*
*                           Changed the boot request message so that the
*                           workstation user can determine which version of
*                           the boot loader is running.
*
*    4/18/95   MCSQ         This one is for a new processor, vme7.
*
*    6/28/96   MCSQ         New processor for LN fill system.
*******************************************************************************
*
	ORG     $FFC10000
*	ORG   $80000
*
CR      EQU     $0D
LF      EQU     $0A
ESC	EQU	$1B

PROTO_PREFIX            EQU     $4F
PROTO_REQUEST_BOOT      EQU     $51
PROTO_FORCE_BOOT        EQU     $52
PROTO_CODE		EQU	$53

ACK			EQU	-1		;ACK request flag
NOACK			EQU	0		;Don't ACK packet
ACKPKT			EQU	1		;ACK response
NAKPKT			EQU	2		;NAK response

LAN_VEC                 EQU     246             ;LANCE vector
LAN_CTL                 EQU     $FFD002A4
LAN_STAT                EQU     $FFD00498

RINT                    EQU     $400            ;Receive flag

CPU40_BOOT              EQU     $FFD00E00

DUSCC1_TPR              EQU     $FF802004       ;Port 1 Transmitter param reg
*
		INCLUDE vmeprom.inc
*
*******************************************************************************
*  Move loader code from nonvolatile memory to top end of RAM.
*  Code is too slow if run in  nonvolatile memory!
*******************************************************************************
BOOT            BRA.S   BOOT_GO
		DC.L    LAN_ADDR        ;Pointer to our ethernet hardware
*                                        address.  NOTE: moving this may cause
*                                        pain and suffering for others!!
*
		DC.L	DECSTATION	;Pointer to ethernet hardware
*					 address of our default host.
BOOT_GO:        
		MEM_LIM                 ;Get memory limits of parent task
		MOVE.L  A1,D3
		MOVE.L  #C_END-C_START,D0
		MOVE.L  D0,D1
		ADD.L   #$1400,D0
		CLR.B   D0
		SUB.L   D0,A1
		ADDQ.L  #2,A1
		MOVE.L  A1,A2
		LEA	C_START(PC),A0
STRUP           MOVE.B  (A0)+,(A1)+     ;Move code
		SUBQ.L  #1,D1
		BNE     STRUP
		MOVE.B  DUSCC1_TPR,D1   ;Disable RTS/CTS handshake
		AND.B   #$F3,D1         ;for the console port
		MOVE.B  D1,DUSCC1_TPR
		MOVE.W  #2*256+63,D1    ;Time slice/priority
		MOVEQ   #0,D0
		MOVEQ   #1,D2           ;I/O port number
		MOVE.L  A2,A0
		SUB.L   #$1002,A0       ;Low memory address
		SUB.L   #$400,D3
		MOVE.L  D3,A1           ;High memory address
		SPAWN_TASK              ;Spawn new task
		TASK_SWAP               ;Swap to new task
		EXIT                    ;Exit to VMEPROM
*
*             Start of code to be relocated
*
C_START:
*******************************************************************************
*  Boot loader initalize and main loop
*******************************************************************************
IPL             LOCK
		BSR.W   LAN_INIT        ;Initialize LANCE controller
		LEA     MESSG(PC),A1    ;Output start-up message to console
		MOVE.W  (A1)+,D7
		OUT_MSG
*
MAIN            UNLOCK
		DLY_SET_EVT     1,1000  ;Set event #1 after 10 seconds
MAIN1           LEA     MULTICAST(PC),A1  ;Build message for host
		BSR     LAN_WRITE       ;Get a buffer
		CLR.L   D1
		LEA     IPL_MES(PC),A1  ;Message text
MAIN2           MOVE.B  (A1)+,D0        ;Move to LAN buffer
		CMP.B	#$FF,D0
		BEQ.S   MAIN3
		MOVE.B  D0,(A0)+
		ADDQ.W  #1,D1
		BRA     MAIN2
*
MAIN3		CMP.B	#50,D1		;Fill remainder of min packet with zero
		BGE.S	MAIN4
		CLR.B	(A0)+
		ADDQ.L	#1,D1
		BRA	MAIN3
*
MAIN4           BSR     LAN_SEND        ;Send message to host
MAIN5           MOVE.L  RBUF_PTR,A0     ;Check for receive buffer
		MOVE.W  2(A0),D0
		BMI.S   MAIN6           ;Buffer not ready
		BSR     LAN_READ        ;Buffer ready. Get pointer and byte
		TST	D1
		BEQ.S   MAIN6           ;count.
		BSR     LOAD_BUF        ;Check for S records and load.
		BSR     LAN_READ_ADV    ;Release this buffer.
		DLY_SET_EVT     1,1000  ;Reset delayed event #1.
MAIN6           TEST_EVT        1       ;Test event #1
		BEQ     MAIN5           ;Not set. Loop testing receive
		BRA     MAIN            ;Event #1 set.  Send power-up message
**                                      ;again.
*******************************************************************************
*  Loader Data
*******************************************************************************
IPL_MES:
	DC.B	0,0,0,0,0,0
	DC.B    'V2.0 IPL Force CPU-40 : Ready for download',CR,0,$FF
*
MESSG   DC.W    MESSG1-MESSG-1
	DC.B    CR,LF
	DC.B    '*************************************************************'
	DC.B    '*****************',CR,LF
	DC.B    '*                                                            '
	DC.B    '                *',CR,LF
	DC.B    '*                           ornl Physics Division            '
	DC.B    '                *',CR,LF
	DC.B    '*                                                            '
	DC.B    '                *',CR,LF
	DC.B    '*                     VME Processor ready for download       '
	DC.B    '                *',CR,LF
	DC.B    '*                                                            '
	DC.B    '                *',CR,LF
	DC.B    '*************************************************************'
	DC.B    '*****************',CR,LF
MESSG1  DC.B    0
	ALIGN   2
MESSG2  DC.W    MESSG3-MESSG2-1
	DC.B    CR,LF
	DC.B    '*************************************************************'
	DC.B    '*****************',CR,LF
	DC.B    '*                                                            '
	DC.B    '                *',CR,LF
	DC.B    '*                       CPU-40 Loaded with '
MESSG3  DC.B    0
	ALIGN   2
MESSG4:
	DC.W    MESSG5-MESSG4-1
	DC.B    CR,LF
	DC.B    '*                                                            '
	DC.B    '                *',CR,LF
	DC.B    '*************************************************************'
	DC.B    '*****************',CR,LF
MESSG5  DC.B    0
	ALIGN   2
*
*   Buffer for file name
*
PROG    DCB.B   20,0
*
	ALIGN   2
GOGO    DC.B    ESC,'GO '
	ALIGN   2
*
*   Ethernet parameters
*
LAN_ADDR        DC.B    $00,$80,$42,$04,$18,$68         ;Our address
DECSTATION      DC.B    $ff,$ff,$ff,$ff,$ff,$ff         ;Host address
MULTICAST       DC.B    $03,'m','c','s','q',$00         ;Boot request host
LAN_HASH        DC.B    0,0,0,0,0,0,0,0
LAN_PROTO       DC.B    PROTO_PREFIX
		DC.B    PROTO_REQUEST_BOOT
LAN_MODE        DC.W    0
	ALIGN   2
*******************************************************************************
*
*   Process any S records in the buffer.
*
*   CALL     A0   -   Pointer to buffer
*            D1   -   Buffer character count
*
*******************************************************************************
LOAD_BUF:
		MOVE.B  (A0)+,D0
		SUBQ.W  #1,D1
		CMP.W   #10,D1
		BLT     LOAD_RTN
		TST.B	D0
		BEQ	LOAD_BUF
		CMP.B   #' ',D0         ;Ignore leading and trailing spaces
		BEQ     LOAD_BUF
		CMP.B   #CR,D0
		BEQ     LOAD_BUF
		CMP.B   #LF,D0
		BEQ     LOAD_BUF
		CMP.B   #'S',D0         ;Have a char other than space, CR or LF
		BEQ.S   LOAD_1          ;Check for start of an S record.  The
		CMP.B   #'s',D0         ;nonspace char must be S or s.
		BEQ.S   LOAD_1
LOAD_BUFA       MOVE.B  (A0)+,D0        ;Not an S record. Skip to next
		SUBQ.W  #1,D1           ;CR or LF.
		BEQ     LOAD_RTN
		CMP.B   #CR,D0
		BEQ     LOAD_BUF
		CMP.B   #LF,D0
		BEQ	LOAD_BUF
		TST.B	D0
		BEQ     LOAD_BUF
		BRA     LOAD_BUFA
*
LOAD_1:         MOVE.B  (A0)+,D0        ;Get record type
		SUBQ.W  #1,D1
		CMP.B   #'5',D0
		BEQ     LOAD_BUFA
		MOVE.B  D0,D7           ;Save record type for later testing
		CLR.L   D3              ;Clear checksum
		MOVE.L  D3,D5           ;and load/start address
		MOVEQ   #2,D2           ;So illegal hex char detect works
		BSR     GET_BYTE        ;Get byte count
		BMI     LOAD_ERR1
		MOVE.B  D4,D2
		SUBQ.B  #1,D2
		BSR     GET_BYTE        ;Get first address byte
		BMI     LOAD_ERR1
		OR.B    D4,D5
		LSL.L   #8,D5
		BSR     GET_BYTE        ;Get second address byte
		BMI     LOAD_ERR1
		OR.B    D4,D5
		CMP.B   #'0',D7         ;Type 0, program name.
		BEQ     LOAD_5
		CMP.B   #'1',D7         ;Type 1 load record
		BEQ     LOAD_3
		CMP.B   #'9',D7         ;Type 9. 16 bit start address
		BEQ.S   LOAD_2
		BSR     GET_BYTE        ;Get next address byte - 24 bits now
		BMI     LOAD_ERR1
		LSL.L   #8,D5
		OR.B    D4,D5
		CMP.B   #'2',D7         ;Type 2 load record.
		BEQ.S   LOAD_3
		CMP.B   #'8',D7         ;Type 8. 24 bit start address.
		BEQ.S   LOAD_2
		LSL.L   #8,D5
		BSR     GET_BYTE        ;Get last address byte - 32 bits now
		BMI     LOAD_ERR1
		OR.B    D4,D5
		CMP.B   #'3',D7         ;Type 3 load record
		BEQ.S   LOAD_3
LOAD_2:         BSR     GET_BYTE        ;Get checksum byte
		ADDQ.B  #1,D3
		BNE     LOAD_ERR2
		LOCK
		TST.L   D5              ;Check for start address
		BEQ     LOAD_DONE       ;No start address given!
		MOVE.L  D5,D1
****************************************************************************
*
*   The following is a real kluge!!  However I know no other way to do it!.
*   We have a starting address in D1.  So convert the start address to ASCII
*   in the channel 1 input buffer.  Then an ASCII command to VMEPROM which
*   is  'GO  XXXXXXXX'.  Next set the channel input flag and kill this task.
*   Upon return to task 0, VMEPROM executes the command we built in its
*   input buffer.  This starts the program just loaded such that it can
*   do input from the console device.  There must be a better way to do this
*   but I be dammed if I know what it is.
*
****************************************************************************
		LEA     GOGO(PC),A2
		MOVE.L  #$219C,A1       ;Address of the channel 1 buffer
		MOVE.W  #13*256,(A1)+   ;Init pointers for message
		MOVE.L  (A2),(A1)+      ;Move ESC,'GO ' to buffer
		ASCII_HEX               ;Convert start address to ASCII
		ADDQ    #8,A1           ;Put CR at end of command string
		MOVE.B  #CR,(A1)+
		CLR.B   (A1)+
		MOVE.L	RBUF_PTR,A1	;Save the Ethernet address of the
		MOVE.L	#LAN_RAM,D0	;host that loaded this code.
		ADD.W	(A1),D0		;That host is now our default
		ADD.W	#6,D0		;host.
		MOVE.L	D0,A1
		MOVE.L	#DECSTATION,A2
		MOVEQ	#6,D0
LOAD_2A:	MOVE.B	(A1)+,(A2)+
		SUBQ.W	#1,D0
		BNE	LOAD_2A
		UNLOCK
		SET_EVT 97              ;Set channel 1 input flag
		BRA     LOAD_EXIT       ;Go kill this task.
*
*   Load record
*
LOAD_3:         MOVE.L  D5,A1   
LOAD_4:         BSR     GET_BYTE
		BMI.S   LOAD_ERR1
		MOVE.B  D4,(A1)+
		TST.B   D2
		BNE     LOAD_4
		BSR     GET_BYTE        ;Get checksum byte
		BNE     LOAD_ERR2
		BRA     LOAD_BUF
*
*   Program name
*
LOAD_5          LEA     PROG(PC),A1     ;Program name buffer
		CLR.W   (A1)+           ;Clear character count
LOAD_5A         BSR.S   GET_BYTE        ;Load file name
		BMI.S   LOAD_ERR1
		MOVE.B  D4,(A1)+
		TST.B   D2
		BNE     LOAD_5A
		CLR.B   (A1)
		BSR.S   GET_BYTE        ;Get checksum byte
		ADDQ.B  #1,D3
		BNE     LOAD_ERR2
		LEA     PROG+2(PC),A1
		MOVEQ   #-1,D3
LOAD_5B         ADDQ.W  #1,D3           ;Count characters in file name
		TST.B   (A1)+
		BNE     LOAD_5B
		LEA     PROG(PC),A1
		MOVE.W  D3,(A1)         ;Save count for string output
		BRA     LOAD_BUF
*
*   Illegal hex character
LOAD_ERR1:      BRA     LOAD_BUF
*
*   Checksum error
LOAD_ERR2:      BRA     LOAD_BUF
*
LOAD_RTN:       RTS
*
*  Finished loading program.  No start address was given.
LOAD_DONE:
		LEA     MESSG2(PC),A1   ;Output load finished messages
		MOVE.W  (A1)+,D7
		OUT_MSG
		LEA     PROG(PC),A1     ;Output name of file loaded
		MOVE.W  (A1)+,D7
		OUT_MSG
		LEA     MESSG4(PC),A1   ;Finish up load message
		MOVE.W  (A1)+,D7
		OUT_MSG
LOAD_EXIT       DLY_SET_EVT     1,0     ;Remove event #1 from queue
		CLR_EVT         1       ;Clear event #1
		BSR.W   INTR_SETUP
		UNLOCK
		MOVEQ   #0,D0           ;Kill current task
		TASK_KILL
		EXIT                    ;Return to VMEPROM monitor
*******************************************************************************
*
*   Get one byte of an S record
*
*   CALL:   A0  -  Pointer to ASCII buffer
*           D1  -  Buffer character counter
*           D2  -  S record byte counter
*           D3  -  S record checksum
*
*  RETURN:  D4  - Byte data if status is positive.
*                 If status is negative, illegal Hex char error.
*******************************************************************************
GET_BYTE:       MOVE.B  (A0)+,D0        ;Get first ASCII Hex char
		SUBQ.W  #1,D1           ;Update buffer character counter
		CLR.L   D4              ;Clear byte accumulator
		CMP.B   #'a',D0
		BLT.S   GET_BYTE_1
		AND.B   #$DF,D0         ;Convert character to upper case
GET_BYTE_1:     SUB.B   #'0',D0         ;Convert ASCII to hex
		BLT.S   GET_BYTE_ERR    ;Illegal char
		CMP.B   #9,D0
		BLE.S   GET_BYTE_2      ;Character is 0 thru 9
		SUBQ.B  #7,D0
		CMP.B   #15,D0
		BGT.S   GET_BYTE_ERR    ;Illegal char if not A thru F
GET_BYTE_2:     MOVE.B  D0,D4
		MOVE.B  (A0)+,D0        ;Get next ASCII Hex char
		SUBQ.W  #1,D1
		CMP.B   #'a',D0
		BLT.S   GET_BYTE_3
		AND.B   #$DF,D0         ;and convert to hex.
GET_BYTE_3:     SUB.B   #'0',D0
		BLT.S   GET_BYTE_ERR    ;Illegal char
		CMP.B   #9,D0
		BLE.S   GET_BYTE_4
		SUBQ.B  #7,D0
		CMP.B   #15,D0
		BGT.S   GET_BYTE_ERR    ;Illegal char
GET_BYTE_4:     LSL.B   #4,D4           ;Conbine to form a byte
		OR.B    D0,D4
		ADD.B   D4,D3           ;Add byte to checksum
		SUBQ.B  #1,D2           ;Update byte counter
		RTS
*
GET_BYTE_ERR:   SUBQ.W  #2,D1           ;Illegal char
		MOVEQ   #-1,D0          ;Return negative to indicate error.
		RTS
*
*******************************************************************************
*
*  LANCE controller and data buffer parameters
*
*******************************************************************************
NUM_RBUF        EQU     16              ;Number of receiver buffers
NUM_TBUF        EQU     16              ;Number of transmit buffers
RBUF_SIZE       EQU     1536            ;Size of receive buffer
TBUF_SIZE       EQU     1536            ;Size of transmit buffer
LANC            EQU     $FEF80000       ;LANCE controller base address
LAN_RAM         EQU     $FEF00000       ;CPU Address of shared RAM
LAN_IBLK        EQU     0               ;Offset of initalize block in shared RAM
LAN_R_RING      EQU     $80             ;Offset of receive ring from init block
LAN_T_RING      EQU     LAN_R_RING+(NUM_RBUF*8) ;Offset of transmit ring
LAN_BUFS        EQU     LAN_T_RING+(NUM_TBUF*8) ;Offset of data buffers
*
RBUF_PTR        EQU     $FEF00040
RBUF_PTR_STR    EQU     $FEF00044
RBUF_PTR_END    EQU     $FEF00048
TBUF_PTR        EQU     $FEF0004C
TBUF_PTR_STR    EQU     $FEF00050
TBUF_PTR_END    EQU     $FEF00054
*
*******************************************************************************
*
*  Initialize LANCE controller.
*
*  1) Build initialization block for LANCE controller
*
*  2) Build receive and transmit buffer headers
*
*  3) Start LANCE controller
*
*******************************************************************************
LAN_INIT        MOVE.L  #LANC,A3        ;LANCE chip base address
		MOVE.W  #0,2(A3)        ;Select CSR0
		MOVE.W  #4,(A3)         ;Stop LAN controller
		MOVE.W  #3,2(A3)        ;Select and load CSR3
		MOVE.W  #4,(A3)         ;Big Ingine mode
		MOVE.W  #1,2(A3)        ;Load address of initialization block
		MOVE.L  #LAN_IBLK,D0
		MOVE.W  D0,(A3) 
		SWAP    D0
		MOVE.W  #2,2(A3)
		MOVE.W  D0,(A3)
		MOVE.L  #LAN_RAM,A0     ;Get CPU address of init block
		SWAP    D0
		ADD.L   D0,A0
		MOVE.W  LAN_MODE(PC),(A0)+  ;Put in mode word
		MOVEQ   #3,D2
		CLR.L   D0
		LEA	LAN_ADDR(PC),A1 ;Put in physical address of this
LAN_INIT1       MOVE.B  (A1)+,D1        ;Ethernet controller.  NOTE: A byte
		MOVE.B  (A1)+,D0        ;swap is required only here.  After
		LSL.W   #8,D0           ;initialization, the chip is programmmed
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
		MOVE.W  #LAN_R_RING,(A0)+ ;Offset of the receive ring
		CLR.L   D1              ;structure.
		MOVE.W  #NUM_RBUF,D0    ;Format the control word for number
LAN_INIT3       LSR.W   #1,D0           ;of receive buffers.  The most
		BEQ.S   LAN_INIT4       ;significant 3 bits are number of
		ADDQ.W  #1,D1           ;buffers expressed as a power of 2.
		BRA     LAN_INIT3
*
LAN_INIT4       LSL.W   #5,D1
		LSL.W   #8,D1
		MOVE.W  D1,(A0)+
		MOVE.W  #LAN_T_RING,(A0)+ ;Offset of the transmit ring
		CLR.L   D1              ;structure.
		MOVE.W  #NUM_TBUF,D0
LAN_INIT5       LSR.W   #1,D0
		BEQ.S   LAN_INIT6
		ADDQ.W  #1,D1
		BRA     LAN_INIT5
*
LAN_INIT6       LSL.W   #5,D1
		LSL.W   #8,D1
		MOVE.W  D1,(A0)+
*
*   Build the receive and transmit ring structures
*
		MOVE.L  #LAN_BUFS,D1    ;Offset of beginning of data buffers
		MOVE.L  #LAN_R_RING,A0  ;Offset of start of receive ring
		ADD.L   #LAN_RAM,A0     ;CPU address of start of shared memory
		MOVE.L  A0,RBUF_PTR_STR ;Init pointers to receive ring for
		MOVE.L  A0,RBUF_PTR     ;buffer management routines.
		MOVE.L  #-RBUF_SIZE,D2  ;Get size of each buffer and
		MOVE.W  #NUM_RBUF,D3    ;the number of receive buffers.
LAN_INIT7       MOVE.W  D1,(A0)+
		SWAP    D1
		OR.W    #$8000,D1       ;Mark receive buffer available
		MOVE.W  D1,(A0)+
		SWAP    D1
		SUB.L   D2,D1
		MOVE.W  D2,(A0)+
		CLR.W   (A0)+
		SUBQ.W  #1,D3
		BNE     LAN_INIT7
		ANDI.L  #$FFFFFF,D1
		MOVE.L  A0,RBUF_PTR_END ;Init buffer pointers for memory
		MOVE.L  A0,TBUF_PTR     ;manager
		MOVE.L  A0,TBUF_PTR_STR
		MOVE.L  #-TBUF_SIZE,D2
		MOVE.W  #NUM_TBUF,D3    ;Build transmit ring.
LAN_INIT8       MOVE.W  D1,(A0)+
		SWAP    D1
		OR.W    #$300,D1
		MOVE.W  D1,(A0)+
		SWAP    D1
		SUB.L   D2,D1
		MOVE.W  D2,(A0)+
		CLR.W   (A0)+
		SUBQ.W  #1,D3
		BNE     LAN_INIT8
		MOVE.L  A0,TBUF_PTR_END
		MOVE.W  #0,2(A3)
		MOVE.W  #1,(A3)         ;Initialize LANCE controller
LAN_INIT9       MOVE.W  (A3),D1         ;Wait till done
		ANDI.W  #$100,D1
		BEQ     LAN_INIT9
		MOVE.W  #$102,(A3)      ;Clear Init done and start LANCE
		MOVE.W  (A3),D1
		RTS
*******************************************************************************
*  Return LANCE controller status in D0
*******************************************************************************
LAN_STATUS      MOVE.L  #LANC,A0
		CLR.W   2(A0)
		MOVE.W  (A0),D0
		MOVE.W  D0,-(SP)
		CLR.B   D0
		MOVE.W  D0,(A0)
		MOVE.W  (SP)+,D0
		RTS
*******************************************************************************
*
*  Returns a pointer to a ready receiver buffer in A0 with the byte
*  count in D1.  The data buffer does not include addresses or CRC characters.
*
*  If the packet is not one of our protocols, advance to next buffer and
*  return with D1 equal to zero.
*******************************************************************************
LAN_READ        MOVE.L  RBUF_PTR,A0     ;Get address of current buffer
		MOVE.W  2(A0),D0        ;Buffer ready?
		BPL.S   LAN_READ1       ;Yes. Return it to user
		DELAY   1               ;No. Wait till buffer ready
		BRA     LAN_READ
*
LAN_READ1       MOVE.L  #LAN_RAM,D0     ;Calculate address of packet data
		ADD.W   (A0),D0
		ADD.W   #12,D0		;Point to protocol word
		CLR.L	-(SP)
		MOVE.L  D0,-(SP)
		MOVE.W  6(A0),D1
		SUB.W   #18,D1          ;Compute data byte count
		CLR.L	D0
		MOVE.B  2(A0),D0        ;Get receive buffer status
		MOVE.W  D0,4(SP)
		MOVE.L  #LANC,A0        ;Clear controller status
		CLR.W   2(A0)
		MOVE.W  (A0),D0
		AND.W	#$7F00,D0
		MOVE.W  D0,(A0)
		MOVE.L  (SP)+,A0        ;Get pointer to protocol word
		MOVE.W	(A0)+,D0	;Get protocol word
		CMP.W	#(PROTO_PREFIX<<8+PROTO_CODE),D0
		BNE.S	LAN_READ5
		MOVE.B	(A0),D0		;Get ACK request flag
		CMP.B	#ACK,D0		;ACK requested?
		BNE.S	LAN_READ4	;No. Don't send ACK
		MOVE.L	D2,-(SP)	;Yes. Send ACK or NACK
		MOVE.L	#ACKPKT,D2
		MOVE.L	4(SP),D0	;Get receive buffer status
		ANDI.B	#$7C,D0		;Check for receive error
		BEQ.S	LAN_READ2
		MOVE.L	#NAKPKT,D2	;Receive error. Send NAK
LAN_READ2	MOVEM.L	D1/A0-A1,-(SP)
		SUBQ.L	#2,A0
		MOVE.L	A0,-(SP)	;Save receive buffer pointer
		MOVE.L	A0,D0
		SUBQ.L	#6,D0		;Point to sender address
		MOVE.L	D0,A1
		BSR.S	LAN_WRITE	;Get transmit buffer pointer
		SUBQ.L	#2,A0
		MOVE.L	(SP)+,A1	;Get back receiver buffer pointer
		MOVE.W	(A1)+,(A0)+	;Move proto word to xmit buffer
		MOVE.B	D2,(A0)+	;Put ACK or NACK code in xmit buffer
		ADDQ.L	#1,A1		;Skip over receive ACK flag
		MOVE.B	(A1)+,(A0)+	;Move order code to xmit buffer
		MOVE.L	(A1)+,(A0)+	;Move PID to xmit buffer
		MOVE.L	#44,D1		;Clear rest of buffer
LAN_READ3	CLR.B	(A0)+
		SUBQ.L	#1,D1
		BNE	LAN_READ3
		BSR	LAN_SEND	;Send ACK/NACK packet
		MOVEM.L	(SP)+,D1/A0-A1
		MOVE.B	D2,D0
		MOVE.L	(SP)+,D2
		CMP.B	#ACKPKT,D0
		BNE.S	LAN_READ6	;Bad packet. Trash it.
LAN_READ4	ADDQ.L	#6,A0
		SUBQ.W	#6,D1
                BRA.S   LAN_READ7
*
LAN_READ5	CMP.W	#$6006,D0	;This protocol retained for historical
		BEQ.S	LAN_READ7	;reasons!
LAN_READ6	BSR.S	LAN_READ_ADV	;Not one of our protocols.
		CLR.L	D1		;Return with zero byte count.
*
LAN_READ7	MOVE.L  (SP)+,D0        ;Return buffer status
		RTS
*******************************************************************************
*
*   Routine releases current buffer for future use by the LANCE chip
*
*******************************************************************************
LAN_READ_ADV    MOVE.L  RBUF_PTR,A0
		MOVE.B  #$80,2(A0)
		ADD.W   #8,A0
		CMP.L   RBUF_PTR_END,A0
		BLT.S   LAN_READ_ADV1
		MOVE.L  RBUF_PTR_STR,A0
LAN_READ_ADV1   MOVE.L  A0,RBUF_PTR
		RTS
*******************************************************************************
*
*   Get an output buffer.
*     CALL:   A1  -  points to destination address array.
*
*    Return:  A0  - points to data buffer.
*             D1  - negative size of buffer in bytes.
*
*******************************************************************************
LAN_WRITE       MOVE.L  TBUF_PTR,A0
		MOVE.W  2(A0),D0
		BPL.S   LAN_WRIT1
		DELAY   1
		BRA     LAN_WRITE
*
LAN_WRIT1       MOVE.L  #LAN_RAM,D0
		ADD.W   (A0),D0
		MOVE.L  D0,A0
		MOVEQ   #6,D1
LAN_WRIT2       MOVE.B  (A1)+,(A0)+
		SUBQ    #1,D1
		BNE     LAN_WRIT2
		LEA	LAN_ADDR(PC),A1
		MOVE.W  (A1)+,(A0)+
		MOVE.W  (A1)+,(A0)+
		MOVE.W  (A1)+,(A0)+
		MOVE.W  LAN_PROTO(PC),(A0)+
		MOVE.W  #TBUF_SIZE,D1
		SUB.W   #14,D1
		RTS
*******************************************************************************
*
*    Send the current output buffer.
*
*   CALL:  D1  -  number of user data bytes in buffer.
*
*******************************************************************************
LAN_SEND        MOVE.L  TBUF_PTR,A0
		CLR.W   6(A0)
		ADD.W   #14,D1
		CMP.W   #64,D1
		BGE.S   LAN_SEND1
		MOVE.W  #64,D1
LAN_SEND1       NEG.W   D1
		MOVE.W  D1,4(A0)
		MOVE.B  #$83,2(A0)
		ADD.W   #8,A0
		CMP.L   TBUF_PTR_END,A0
		BLT.S   LAN_SEND2
		MOVE.L  TBUF_PTR_STR,A0
LAN_SEND2       MOVE.L  A0,TBUF_PTR
		RTS
*******************************************************************************
*
*    Setup and interrupt handler for the ethernet conroller on completion of
*    a download.  The handler executes in nonvolatile RAM and only looks
*    for a reboot request from the host.
*
*******************************************************************************
INTR_SETUP:     MOVE.L  RBUF_PTR,A0             ;Release the last received
		MOVE.B  #$80,2(A0)              ;packet and update pointers.
		ADD.W   #8,A0
		CMP.L   RBUF_PTR_END,A0
		BNE.S   INTR_SETUP1
		MOVE.L  RBUF_PTR_STR,A0
INTR_SETUP1     MOVE.L  A0,RBUF_PTR
		MOVE.L  #LAN_INTR,LAN_VEC*4	;Set interrupt vector
		MOVE.B  #$F,LAN_CTL             ;Enable local 6 at level 7
		MOVE.W  #$40,LANC               ;Ebable LANCE interrupts.
		RTS
*
*             End of code to be relocated
*
C_END:
*******************************************************************************
*
*   Interrupts form Ethernet.  The boot loader does not use interrupts.  But
*   after a completed download, interrupts are enabled and this is the
*   default handler.   It ignores and discards all received packets except
*   one from the host requesting a reboot.  This makes it possible to load
*   and execute programs not having an ethernet driver and still be able
*   to regain control remotely.   How about them apples, Mrs Mull!
*
*******************************************************************************
LAN_INTR:       MOVEM.L D0/A0-A1,-(SP)
		MOVE.B  LAN_STAT,D0
		MOVE.L  #LANC,A1        ;lance controller base address
		CLR.W   2(A1)           ;Select CSR0
		MOVE.W  (A1),D0         ;Get status
		ANDI.W  #$7F40,D0       ;Clear all flags and enable interrupts
		MOVE.W  D0,(A1)
		ANDI.W  #RINT,D0        ;Check for receive interrupt
		BEQ.S   LAN_INTR3       ;Not receive, just exit
		MOVE.L  RBUF_PTR,A0     ;Get address of current buffer
		MOVE.L  #LAN_RAM,A1     ;Calculate address of packet data
		ADD.W   (A0),A1
		CMP.W   #(PROTO_PREFIX<<8+PROTO_FORCE_BOOT),12(A1)
		BNE.S   LAN_INTR1
		CLR.B   CPU40_BOOT      ;REBOOT the CPU-40
*
LAN_INTR1:
		MOVE.B  #$80,2(A0)      ;Release packet buffer and update
		ADD.W   #8,A0           ;pointers.
		CMP.L   RBUF_PTR_END,A0
		BLT.S   LAN_INTR2
		MOVE.L  RBUF_PTR_STR,A0
LAN_INTR2       MOVE.L  A0,RBUF_PTR
LAN_INTR3       MOVEM.L (SP)+,D0/A0-A1
		RTE

		END
