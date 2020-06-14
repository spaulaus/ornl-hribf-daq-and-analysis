*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-1995
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
*    File:         /usr/users/mcsq/Dvme3/vmeprom.s
*
*    Description:  C callable routines for functions provided by the Force
*                  VMEPROM.  Current VMEPROM version is 2.74  9-Apr-91.
*                  Changes may be required if the version changes!!!
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/4/92      MCSQ         Original
*
*    4/23/92     MCSQ         Fix stack problem in routines super_mode_
*                             and user_mode_.
*
*    7/29/92     MCSQ         Add routine set_intr_level_ to allow user
*                             mode codes to modify the interrupt level.
*
*   10/13/92     MCSQ         Add routine cache_inval_ which selectively
*                             invalidates pages of the cache - either
*                             data or instruction or both.
*
*   11/ 1/92     MCSQ         Add 'get_line_' routine for C programs.
*                             Uses the VEMPROM call XGLB.
*
*    3/18/95     MCSQ         Corrected type of second argument in wait_evts_
*                             prototype.
*******************************************************************************
               SECTION 9
*
FALSE           EQU     0
TRUE            EQU     1
*******************************************************************************
*
*    int clr_evt_(unsigned char event) 
* Returns TRUE if the event was clear prior to call.  Otherwise, return FALSE.
*
clr_evt_:       MOVE.B  7(SP),D1        ;Get event number
		NEG.B   D1              ;Make event number negative
		MOVEQ   #FALSE,D0
		DC.W    $A046           ;XSEV call
		BNE.S   CLR_EVT
		MOVEQ   #TRUE,D0
CLR_EVT:        RTS
*******************************************************************************
*
*    void delay_(unsigned long ticks)
* Returns after ticks*10 Milliseconds.
*
delay_:         MOVE.W  #128,D1         ;Local event number
		MOVE.L  4(SP),D0        ;Get delay time
		DC.W    $A032           ;XDEV call
		DC.W    $A01C           ;XSUI call
		RTS
*******************************************************************************
*
*   int dly_clr_evt_(unsigned char event,unsigned long time)
* Returns 83 if event stack is full.  Otherwise returns 0.
*
dly_clr_evt_:   CLR.L   D1
		OR.B    7(SP),D1        ;Get event number
		NEG.B   D1              ;Make it negative for clear
		MOVE.L  8(SP),D0        ;Get time delay
		DC.W    $A032           ;XDEV call
		BNE.S   DLY_CLR
		CLR.L   D0
DLY_CLR:        RTS
*******************************************************************************
*
*   int dly_set_evt_(unsigned char event,unsigned long time)
* Returns 83 if event stack is full.  Otherwise returns 0.
*
dly_set_evt_:   CLR.L   D1
		OR.B    7(SP),D1        ;Get event number
		MOVE.L  8(SP),D0        ;Get delay time
		DC.W    $A032           ;XDEV call
		BNE.S   DLY_SET
		CLR.L   D0
DLY_SET:        RTS
*******************************************************************************
*
*   void log_phys_(unsigned char event0,unsigned char event1,char **ad0,
*                                       char **ad1,int *desc)
*
log_phys_:      CLR.L   D1
		MOVE.B  11(SP),D1       ;Get event 1
		LSL     #8,D1
		MOVE.B  7(SP),D1        ;Get event 0
		DC.W    $A110           ;XTLP call
		EXG     A2,D0
		MOVE.L  12(SP),A2
		MOVE.L  A0,(A2)         ;Return event 0 address
		MOVE.L  16(SP),A2
		MOVE.L  A1,(A2)         ;Return event 1 address
		MOVE.L  20(SP),A2
		MOVE.L  D1,(A2)         ;Return event descriptors
		EXG     D0,A2
		RTS
*******************************************************************************
*
*    int set_evt_(unsigned char event) 
* Returns TRUE if the event was set prior to call.  Otherwise, return FALSE.
*
set_evt_:       MOVE.B  7(SP),D1        ;Get event number
		MOVEQ   #FALSE,D0
		DC.W    $A046           ;XSEV call
		BEQ.S   SET_EVT
		MOVEQ   #TRUE,D0
SET_EVT:        RTS
*******************************************************************************
*
*   int test_evt_(int event)
* Returns TRUE if flag is set and FALSE if flag is clear.
*
test_evt_:      MOVE.L  4(SP),D1        ;Get event number
		MOVEQ   #TRUE,D0
		DC.W    $A01A           ;XTEF call
		BNE.S   TEST_EVT
		MOVEQ   #FALSE,D0
TEST_EVT:       RTS
*******************************************************************************
*
*    int set_phys_(int desc,char *addr) 
* Returns TRUE if the event was set prior to call.  Otherwise, return FALSE.
*
set_phys_:      MOVE.B  7(SP),D1        ;Get bit number
		MOVEQ   #FALSE,D0
		MOVE.L  8(SP),A0        ;Get address
		BSET    D1,(A0)         ;Set bit
		BEQ.S   SET_PHYS
		MOVEQ   #TRUE,D0
SET_PHYS:       RTS
*******************************************************************************
*
*    int wait_evts_(unsigned char event1, unsigned char event2)
* Returns event number which caused reschedule.
*    
wait_evts_:     CLR.W   D1
		OR.B    11(SP),D1
		LSL.W   #8,D1
		OR.B    7(SP),D1
		DC.W    $A01C           ;XSUI call
		RTS
*******************************************************************************
*
*   int wait_phys_(int desc,char *ad0,char *ad1)
* Returns 0 if reschedule due to event 0 or 1 if reschedule due to event 1.
*
wait_phys_:     MOVE.L  4(SP),D1        ;Get event1/event0 descriptor
		MOVEM.L 8(SP),A0-A1     ;Get event 0/event 1 addresses 
		CLR.L   D0
		DC.W    $A112           ;XSOE call
		TST.W   D0
		BPL.S   WAIT_PHYS
		CLR.L   D0
WAIT_PHYS:      RTS

*******************************************************************************
*
*   int receive_ptr_(int slot,int *task,char **msg)
* Returns FALSE if no message available.  Otherwise returns TRUE.
*
receive_ptr_:   CLR.L   D0
		MOVE.B  7(SP),D0        ;Get message slot number
		DC.W    $A004           ;XGMP call
		BNE.S   RECEIVE_PTR
		MOVE.L  8(SP),A0        ;Return task number
		MOVE.L  D0,(A0)
		MOVE.L  12(SP),A0       ;Return mesage pointer
		MOVE.L  A1,(A0)
		MOVEQ   #TRUE,D0
		RTS
*
RECEIVE_PTR:    MOVEQ   #FALSE,D0
		RTS
*******************************************************************************
*
*  int send_ptr_(int slot,char *msg)
* Returns FALSE if message pointer not sent.  Otherwise TRUE.
*
send_ptr_:      CLR.L   D0
		MOVE.B  7(SP),D0
		MOVE.L  8(SP),A1
		DC.W    $A002           ;XSMP call
		BNE.S   SEND_PTR
		MOVEQ   #TRUE,D0
		RTS
*
SEND_PTR:       MOVEQ   #FALSE,D0
		RTS
*******************************************************************************
*
*  int send_msg_(int task,char *msg)
* Returns FALSE if message not sent.  Otherwise TRUE.
*
send_msg_:      CLR.L   D0
		MOVE.B  7(SP),D0
		MOVE.L  8(SP),A1
		DC.W    $A020          ;XSTM call
		BNE.S   SEND_MSG
		MOVEQ   #TRUE,D0
		RTS
*
SEND_MSG:       MOVEQ   #FALSE,D0
		RTS

*******************************************************************************
*
*   void cache_inval_(unsigned char cache,void *start,void *end)
*
* Call:   cache - code for which cache to invalidate
*                 1 = data
*                 2 = instruction
*                 anything else = both
*
*         start - start address
*         end   - end address
*
cache_inval_:   CLR.L   D1
		OR.B    7(SP),D1        ;Get cache flag
		MOVE.L  8(SP),A0        ;Get start address
                CMP.B   #1,D1           ;Data cache?
                BNE.S   CACHE2          ;No.
CACHE1:         CINVP   #1,(A0)         ;Yes.
                ADDA    #$1000,A0
                CMPA.L  12(SP),A0
                BLO.S   CACHE1
                BRA.S   CACHE5
*
CACHE2:         CMP.B   #2,D1           ;Instruction cache?
                BNE.S   CACHE4          ;No.
CACHE3:         CINVP   #2,(A0)         ;Yes.
                ADDA    #$1000,A0
                CMPA.L  12(SP),A0
                BLO.S   CACHE2
                BRA.S   CACHE5
*
CACHE4:         CINVP   #3,(A0)         ;Do both data and instruction
                ADDA    #$1000,A0
                CMPA.L  12(SP),A0
                BLO.S   CACHE4
CACHE5:         RTS
*******************************************************************************
*
*   char *mem_alloc_high_(int size)
*
****************  CAUTION.   BEWARE.  ********************
*  The following 2 EQUs assume VMEPROM Version 2.74 9-Apr-91
*
_MAPS           EQU         $119C
MEM_MAX         EQU         $400000
*
mem_alloc_high_:
		MOVE.L      4(SP),D0            ;Get number of 1K blocks
		ADDQ.L      #1,D0
		LSR.L       #1,D0
		MOVEM.L     D2/A2,-(SP)         ;to allocate and save regs
		MOVEQ       #-1,D1              ;we use.
		LEA         _MAPS,A1            ;Get start and end addresses
		LEA         _MAPS+MEM_MAX/(8*2048),A2   ;of bit map.
		MOVEA.L     #MEM_MAX,A0         ;End of memory address
alloc_high1:
		MOVE.W      D0,D2
alloc_high2:
		ADDQ        #1,D1               ;Incr bit number for test
		ANDI        #7,D1               ;Finished with this byte?
		BNE.S       alloc_high3         ;No. Go test bit
		SUBQ.L      #1,A2               ;Yes. Go to next byte
		CMPA.L      A1,A2               ;Check all memory?
		BCC.S       alloc_high3         ;No. Begin testing this byte
		MOVEQ       #-1,D0              ;Yes. ERROR
		MOVEM.L     (SP)+,D2/A2
		RTS
*
alloc_high3:
		SUBA        #2048,A0            ;Begining address of block
		BTST        D1,(A2)             ;Test bit map
		BEQ         alloc_high1         ;Block already allocated
		SUBQ        #1,D2               ;Not allocated
		BGT         alloc_high2
alloc_high4:
		BCLR        D1,(A2)             ;Have found a contiguous block
		ANDI        #7,D1               ;of requested size.
		BNE.S       alloc_high5         ;Now mark the block as 
		ADDQ.L      #1,A2               ;allocated in bit map
alloc_high5:
		SUBQ        #1,D1
		ADDQ        #1,D2
		CMP         D0,D2
		BNE         alloc_high4         ;Mark all blocks used
		MOVE.L      A0,D0               ;Return begining address
		MOVEM.L     (SP)+,D2/A2
		RTS
*
*******************************************************************************
*
*   char *mem_alloc_low_(int size)
*
mem_alloc_low_:
		MOVE.L      4(SP),D0            ;Get number of 1K blocks
		ADDQ.L      #1,D0
		LSR.L       #1,D0
		MOVEM.L     D2/A2,-(SP)         ;to allocate and save regs
		MOVEQ       #8,D1               ;we use.
		LEA         _MAPS,A1            ;Get start and end addresses
		LEA         _MAPS+MEM_MAX/(8*2048),A2   ;of bit map.
		MOVEA.W     #-2048,A0           ;Start of memory address
alloc_low1:
		MOVE.W      D0,D2
alloc_low2:
		SUBQ        #1,D1               ;Incr bit number for test
		BPL.S       alloc_low3          ;Not finished, go test bit
		MOVEQ       #7,D1
		ADDQ.L      #1,A1               ;Yes. Go to next byte
		CMPA.L      A1,A2               ;Check all memory?
		BCC.S       alloc_low3          ;No. Begin testing this byte
		MOVEQ       #-1,D0              ;Yes. ERROR
		MOVEM.L     (SP)+,D2/A2
		RTS
*
alloc_low3:
		ADDA        #2048,A0            ;Begining address of block
		BTST        D1,(A1)             ;Test bit map
		BEQ         alloc_low1          ;Block already allocated
		SUBQ        #1,D2               ;Not allocated
		BGT         alloc_low2
alloc_low4:
		BCLR        D1,(A1)             ;Have found a contiguous block
		ADDQ        #1,D1               ;of requested size.
		ANDI        #7,D1
		BNE.S       alloc_low5
		SUBQ.L      #1,A1               ;allocated in bit map
alloc_low5:
		ADDQ        #1,D2
		CMP         D0,D2
		BNE         alloc_low4          ;Mark all blocks used
		SUBQ.L      #1,D2
		MULU.W      #2048,D2
		MOVE.L      A0,D0               ;Return begining address
		SUB.L       D2,D0
		MOVEM.L     (SP)+,D2/A2
		RTS
*
*******************************************************************************
*
*   int free_mem_(int size,char *addr)
* Returns 79 on memory error.  Otherwise, retuens 0.
*
free_mem_:      MOVE.W  6(SP),D0        ;Get size in K bytes.
		MOVE.L  8(SP),A0        ;Start address.
		DC.W    $A040           ;XFUM call
		BNE.S   FREE_MEM
		CLR.W   D0
FREE_MEM:       RTS
*******************************************************************************
*
*   int get_mem_(int size,char **start,char **end)
* Returns 73 if not enough memory.  Otherwise, returns 0.
*
get_mem_:       MOVE.L  4(SP),D0
		DC.W    $A03E           ;XGUM call
		BNE.S   GET_MEM
		EXG     D1,A2
		MOVE.L  8(SP),A2
		MOVE.L  A0,(A2)         ;Return start address
		MOVE.L  12(SP),A2
		MOVE.L  A1,(A2)         ;Return end address
		EXG     A2,D1
		CLR.L   D0
GET_MEM:        RTS
*******************************************************************************
*
*    void mem_lim_(char **prog,char **eom,char **last,char **syram,char **tcb)
*
mem_lim_:       MOVEM.L A2-A6,-(SP)
		DC.W    $A010           ;XGML call
		MOVEM.L 24(SP),A3-A4
		MOVE.L  A0,(A3)         ;End of TCB (i.e. start + $1000)
		MOVE.L  A1,(A4)         ;End of memory - $100
		MOVE.L  A2,D1
		MOVEM.L 32(SP),A0-A2
		MOVE.L  D1,(A0)         ;Last loaded address
		MOVE.L  A5,(A1)         ;SYRAM address
		MOVE.L  A6,(A2)         ;TCB address
		MOVEM.L (SP)+,A2-A6
		RTS
*******************************************************************************
*
*   void move_stack(char *new_top,char *old_top)
*
move_stack_:
		MOVE.L  4(SP),A1        ;Get new stack top address
		MOVE.L  8(SP),D1        ;Get old stack top
		MOVE.L  A2,-(SP)        :Save A2 for return
		DC.W    $A02C           ;XSUP call
		MOVE.L  USP,A0          ;Get old stack pointer
		LINK    A6,#0
MOVE_STACK:     MOVE.L  (A6),A2
		MOVE.L  A2,-(SP)
		CMP.L   A0,A2
		BCS.S   MOVE_STACK1
		CMP.L   A2,D1
		BCS.S   MOVE_STACK1
		MOVE.L  D1,D0
		SUB.L   A2,D0
		MOVE.L  A1,A2
		SUB.L   D0,A2
		MOVE.L  A2,(A6)
		MOVE.L  (SP)+,A6
		BRA     MOVE_STACK
*
MOVE_STACK1:    ADDQ.L  #4,SP
		MOVE.L  (SP)+,A6
		SUB.L   A0,D1
		ADDQ.L  #1,D1
		LSR.L   #1,D1
		LSL.L   #1,D1
		SUB.L   D1,A1
		MOVE.L  A1,USP
MOVE_STACK2:    MOVE.W  (A0)+,(A1)+
		SUBQ.L  #2,D1
		BNE     MOVE_STACK2
		ANDI    #$DFFF,SR       ;Return to user mode
		MOVE.L  (SP)+,A2
		RTS

*******************************************************************************
*
*      void byte_swap_(char * buf, int count)
*
byte_swap_:
		MOVE.L  4(SP),A0
		MOVE.L  A0,A1
		ADD.L   8(SP),A1

		BRA.S   BYTE_SWAP2
BYTE_SWAP1:
		MOVE.B  (A0)+,D0
		MOVE.B  (A0),-1(A0)
		MOVE.B  D0,(A0)+
BYTE_SWAP2:
		CMPA.L  A0,A1
		BHI     BYTE_SWAP1
		RTS
*******************************************************************************
*
*     long word_swap_(short *buf, int count)
*
word_swap_:
		MOVE.L  4(SP),A0
		MOVE.L  A0,A1
		MOVE.L  8(SP),D0
		LSL.L   #1,D0
		ADD.L   D0,A1

		BRA.S   WORD_SWAP2
WORD_SWAP1:
		MOVE.L  (A0),D0
		SWAP    D0
		MOVE.L  D0,(A0)+
WORD_SWAP2:
		CMPA.L  A0,A1
		BHI     WORD_SWAP1
		RTS

*******************************************************************************
*
*   int spawn_task_(int size,int pri,int port,char *low_mem,char *high_mem,
*                                                           char *start)
* Returns task number of new task.
*
spawn_task_:    MOVE.L  D2,-(SP)
		MOVE.L  A2,-(SP)
		MOVEM.L 12(SP),D0-D2/A0-A2
		DC.W    $A026           ;XCTB call
		MOVE.L  (SP)+,A2
		MOVE.L  (SP)+,D2
		RTS
*******************************************************************************
*
*   void task_exit_(void)
*
task_exit_:     ADDQ.L  #4,SP
		DC.W    $A00E           ;XEXT call
*******************************************************************************
*
*   void task_kill_(int task)
*
task_kill_:     
		MOVE.L  4(SP),D0
		DC.W    $A0FA           ;XKTB call
		RTS
*******************************************************************************
*
*   int task_status_(int task)
*
task_status_:   MOVE.L  4(SP),D0        ;Get task number
		DC.W    $A012           ;XRTS call
		MOVE.L  D1,D0
		RTS
*******************************************************************************
*
*    void task_swap_(void)
*
task_swap_:     DC.W    $A000           ;XSWP call
		RTS
*******************************************************************************
*
*
*  int task_priority_(int task,int time,int priorty)
*
task_priority_: MOVE.L  4(SP),D0
		MOVE.L  8(SP),D1
		LSL.L   #8,D1
		OR.B    15(SP),D1
		DC.W    $A03C          ;XSTP call
		RTS
*
*******************************************************************************
*
*    int lock_(void)
* Returns TRUE if task already locked.  Otherwise, returns FALSE.
*
lock_:          MOVEQ   #TRUE,D0
		DC.W    $A014           ;XLKT call
		BNE.S   LOCK
		MOVEQ   #FALSE,D0
LOCK:           RTS
*******************************************************************************
*
*    void unlock_(void)
*
unlock_:        DC.W    $A016           ;XULT call
		RTS
*******************************************************************************
*
*   void super_mode_(void)
*
super_mode_:
		MOVE.L	(SP)+,D0
                DC.W    $A02C           ;XSUP call
		MOVE.L	D0,-(SP)
		RTS
*******************************************************************************
*
*   void user_mode_(void)
*
user_mode_:
		MOVE.L	(SP)+,D0
		ANDI.W  #$DFFF,SR
		MOVE.L	D0,-(SP)
		RTS
*******************************************************************************
*
*   int set_intr_level_(int new_level)
*
*  Call:  new_level = New interrupt level * 256.  Example: 0x700 is level 7.
*
*  Return: Returns the old status register which includes the old level.
*          This return value may be used to restore the old_level in a
*          sebsequent call.
*
set_intr_level_:
		MOVE.L	4(SP),D1
		ANDI	#$700,D1
		DC.W	$A042		;XRSR
		MOVE.W	D0,-(SP)
		ANDI	#$F8FF,D0
		OR	D1,D0
		BTST	#13,D0
		BNE.S	SET_LEVEL
		DC.W	$A02C		;XSUP
SET_LEVEL:
		MOVE	D0,SR
		MOVE.W	(SP)+,D0
		RTS

*******************************************************************************
*
*   int INPUT(void)
*
INPUT:          DC.W    $A09E           ;XGCP call;
		RTS
*
*******************************************************************************
*
*   void OUTPUT(char ch)
*
OUTPUT:         MOVE.B  7(SP),D0
		DC.W    $A0BA           ;XPCR call
		RTS
*******************************************************************************
*
*      int  get_line_(void **buffer)
*
*  Call:   buffer - pointer to a pointer to char
*
*  Return: int    - number of characters in buffer
*          buffer - pointer to buffer
*
get_line_:	LEA	LINE_BUF(PC),A1
		MOVEA.L	4(SP),A0
		MOVE.L	A1,(A0)
		DC.W	$A07C		;XGLB call
                BLT	get_line_
		MOVE.L	D1,D0
		RTS
*
LINE_BUF:	DCB.B	82,0
*******************************************************************************
*
*      char *get_time_(void)
*
get_time_:      DC.W    $A05E           ;XRTM call
		MOVE.L  A1,D0
		RTS
*******************************************************************************
*
*      char *get_date_(void)
*
get_date_:      DC.W    $A05C           ;XRDT call
		MOVE.L  A1,D0
		RTS
*******************************************************************************
*
		SECTION 14
		XDEF    clr_evt_
		XDEF    delay_
		XDEF    dly_clr_evt_
		XDEF    dly_set_evt_
		XDEF    log_phys_
		XDEF    set_evt_
		XDEF    set_phys_
		XDEF    test_evt_
		XDEF    wait_evts_
		XDEF    wait_phys_
		XDEF    receive_ptr_
		XDEF    send_ptr_
		XDEF    send_msg_
                XDEF    cache_inval_
		XDEF    mem_alloc_high_
		XDEF    mem_alloc_low_
		XDEF    free_mem_
		XDEF    get_mem_
		XDEF    mem_lim_
		XDEF    move_stack_
		XDEF    byte_swap_
		XDEF    word_swap_
		XDEF    spawn_task_
		XDEF    task_exit_
		XDEF    task_kill_
		XDEF    task_status_
		XDEF    task_swap_
		XDEF    task_priority_
		XDEF    lock_
		XDEF    unlock_
		XDEF    super_mode_
		XDEF    user_mode_
		XDEF	set_intr_level_
		XDEF    INPUT
		XDEF    OUTPUT
		XDEF	get_line_
		XDEF    get_time_
		XDEF    get_date_
		END
