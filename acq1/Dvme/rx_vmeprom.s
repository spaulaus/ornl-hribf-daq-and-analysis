*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
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
*    File:         /usr/users/mcsq/Dvme3/rx_vmeprom.s
*
*    Description:  This code is the A Line handler for our FORCE Computers
*                  VME CPU-40.  Most systems call to VMEPROM are just
*                  passed to the original VMEPROM code for processing.
*                  Several calls are intercepted and handled in this code.
*                  The reason for this was to improve the speed of those
*                  calls used in our data acquisition system.  With this
*                  code installed, the system is approx. 15% faster.
*
*                  The code herein was produced by disassembling the
*                  Force VMEPROM and then modifing the critical functions.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*     4/ 7/93   MCSQ         
*
*     5/12/94   MCSQ       This code had one absolute address in FORCE's
*                          VMEPROM built in to the code.  That was the
*                          address of the A line handler in the VMEPROM.
*                          Version 2.84 of VMEPROM has a different address
*                          for the A line handler than the previous versions
*                          of VMEPROM we have used.  That, of course, broke
*                          this code.
*
*                          The fix is what I should have done to begin with.
*                          When rx_vmeprom is started, the address stored
*                          in vector 10 (the A line vector) is saved.
*                          When my A line handler passes the exception
*                          to VMEPROM, the saved address is used instead
*                          of an absolute address built into the code!.
*
*     5/30/94   MCSQ       The big reason this code is faster than Force's
*                          has to do with how much of the cache is 
*                          invalidated on a context switch.  Force invalidates
*                          the entire cache(data and instruction).  In this
*                          system, only on page of the instruction cache
*                          needs to be invalidated.  That page contains
*                          the flag test routine which may change on each
*                          context switch.  The original code invalidated the
*                          cache page prior to building the flag test
*                          routine.  This works most of the time but in general
*                          has two flaws: 1) the flag test routines are
*                          not aligned on 16 byte boundaries(cache line) and
*                          2) the processor does instruction prefetch.
*                          Therefore, the cache page invalidation MUST be
*                          after the flag test routine is built!.
*
*    10/16/95   MCSQ       Original code enables all interrupts while 
*                          testing for a process to be scheduled.  This
*                          can be a problem with high interrupt rates since
*                          the reschedule can be reentered by another clock
*                          interrupt before finished.  When this happens
*                          the stack grows until something crashes.  Change
*                          so that all interrupts are disabled while we
*                          schedule next task. See code just before XSWP
*****************************************************************************/
          ORG   $17000
*
DEBUG   EQU     0
*
XIT0	MACRO
	DC.W	$A100
	ENDM

XGUM	MACRO
	DC.W	$A03E
	ENDM

XSTP	MACRO
	DC.W	$A03C
	ENDM

XSWP	MACRO
	DC.W	$A000
	ENDM

XEXT	MACRO
	DC.W	$A00E
	ENDM

XERR	MACRO
	DC.W	$A00C
	ENDM

XRTS	MACRO
	DC.W	$A012
	ENDM

XCDB	MACRO
	DC.W	$A056
	ENDM

XULT	MACRO
	DC.W	$A016
	ENDM

XLKT	MACRO
	DC.W	$A014
	ENDM

XFUM	MACRO
	DC.W	$A040
	ENDM

XRST	MACRO
	DC.W	$A0B4
	ENDM

XSUP	MACRO
	DC.W	$A02C
	ENDM

XUSP	MACRO
	DC.W	$A008
	ENDM

SYRAM     EQU   $41C
*
*     System RAM
*
bios	EQU	$0	; address of bios ROM
mail	EQU	$4	; mail array address
rdkn	EQU	$8	; ram disk #
rdks	EQU	$A	; ram disk size
rdka	EQU	$C	; ram disk address

f681	EQU	$12	; 68000/68010 flag
fcnt	EQU	$1A	; fine counter
tics	EQU	$1C	; 32 bit time counter
smon	EQU	$20	; month
shrs    EQU     $24
ssec1	EQU	$26	; seconds
patb	EQU	$28	; input port allocation table
brkf	EQU	$38	; input break flags
f8bt	EQU	$48	; port flag bits
utyp	EQU	$58	; port uart type
urat	EQU	$68	; port rate table
evtb	EQU	$78	; 0-79 event table
evto	EQU	$82	; 80-95 output events
evti	EQU	$84	; 96-111 input events
evts	EQU	$86	; 112-117 system events
ev128	EQU	$88	; task 128 events
evtm	EQU	$98	; events 112-115 timers
bclk	EQU	$A8	; clock adjust constant
tltp	EQU	$AC	; task list pointer
utcb	EQU	$B0	; user tcb ptr
sptn	EQU	$B8	; spawn task number
utim	EQU	$B9	; user time slice counter
tpry	EQU	$BA	; task priority
tskn	EQU	$BB	; current task number
tqux	EQU	$BD	; task queue offset flag/no
tlck1	EQU	$BE	; task lock/reschedule flags
tlck2	EQU	$BF

ccnt	EQU	$D6	; control C count
iord	EQU	$F6	; i/o redirect
fect	EQU	$FA	; file expand count
pidn	EQU	$FB	; processor ident byte
begn	EQU	$FC	; abs address of K1$BEGN table
mapb	EQU	$198	; memory map bias
maps	EQU	$19C	; system memory bitmap
port	EQU	$119C	; character input buffers
iout	EQU	$157A	; character output buffers
rdtb	EQU	$1958	; redirect table
tque	EQU	$1968	; task queue
tlist	EQU	$19EA	; task list
tsev	EQU	$1DEA	; task schedule event table
tmtf	EQU	$25EA	; to/from/INDEX.W
tmbf	EQU	$26EA	; task message buffers
tmsp	EQU	$36EA	; task message pointers
deiq	EQU	$374A	; delay event insert queue
devt	EQU	$3894	; delay events
bsct	EQU	$3B16	; basic screen command table
xchi	EQU	$3B56	; channel buffer queue
xchb	EQU	$3B66	; channel buffers
xfsl	EQU	$4366	; file slots
l2lk	EQU	$4CE6	; level 2 lock (file prims, event 120)
l3lk	EQU	$4CE7	; level 3 lock (disk prims, event 121)
drvl	EQU	$4CE8	; driver link list entry point
utll	EQU	$4CEC	; utility link list entry point
rdkl	EQU	$4CF0	; RAM disk list
clk100  EQU     $4D52   ; Acq system 100 Hz counter
evtlst  EQU     $5600   ; 
*
*     Task RAM - a copy for each task
*
ubuf	EQU	$0      ; 256 byte user buffer
clb	EQU	$100    ; 80 byte command line buffer
mwb	EQU	$150    ; 32 byte monitor parameter buffer
mpb	EQU	$170    ; monitor parameter buffer
cob	EQU	$1AC    ; character out buffer
swb     EQU     $1B4    ; system work buffer/task pdos stack
tsp	EQU	$3B0    ; task stack pointer
kil     EQU     $3b4    ; kill self pointer
sfp	EQU	$3B8    ; 
svf	EQU	$3BC    ; save FP registers flag
iff     EQU     $3BD    ;
trp	EQU	$3BE    ; user TRAP vectors
zdv     EQU     $3FE    ; zero divide trap
chk     EQU     $402    ; CHCK instruction trap
trv     EQU     $406    ; TRAPV instruction trap
trc     EQU     $40A    ; trace vector
fpa     EQU     $40E    ; floating point accumulator
fpe     EQU     $416    ; fp error process address
clp	EQU	$41A    ; command line pointer
bum	EQU	$41E    ; beginning of user memory
eum	EQU	$422    ; end of user memory
ead	EQU	$426    ; entry address
imp	EQU	$42A    ; internal memory pointer
aci	EQU	$42E    ; assigned input file ID
aci2	EQU	$430    ; assigned input file ID's
len	EQU	$432    ; last error number
sfi	EQU	$434    ; spool file id
flg	EQU	$436    ; task flags
slv     EQU     $437    ; directory level
fec     EQU     $438    ; file expansion count
spare1  EQU     $439
csc     EQU     $43A    ; clear screen characters
psc     EQU     $43C    ; position cursor characters
sds     EQU     $43E    ; alternate system disks
sdk     EQU     $441    ; system disk
ext	EQU	$442    ; XEXT address
err	EQU	$446    ; XERR address
cmd	EQU	$44A    ; command line delimiter
tid	EQU	$44B    ; task id
ecf	EQU	$44C    ; echo flag
cnt	EQU	$44D    ; output column counter
mmf	EQU	$44E    ; memory modified flag
prt	EQU	$44F    ; input port number
spu	EQU	$450    ; spooling unit mask
unt	EQU	$451    ; output unit mask
u1p	EQU	$452    ; unit 1 port number
u2p	EQU	$453    ; unit 2 port number
u4p	EQU	$454    ; unit 4 port number
u8p	EQU	$455    ; unit 8 port number
bpact	EQU	$7F6    ; break point active flag
savesp	EQU	$7F8    ; save VMEprom stack during GO/T etc
csave	EQU	$FC0    ; save Cache control register
*
ISAV_SR	EQU	$3C
*
*  Startup and enable this A Line handler
*
          TST.L       Old_Ahandle           !Already started?
          BNE.S       STR2                  !Yes. Just exit
          MOVEA.L     SYRAM,A5              !No.
          LEA         evtlst(A5),A0
          MOVEA.L     A0,A1
          ADDA        #32*16,A1
STR       CLR.L       (A0)+                 !Clear the event list
          CMPA.L      A1,A0
          BLT         STR
          MOVE.L      $28,A0                !Save Force's A Line handler
          MOVE.L      A0,Old_Ahandle        !address.  Use their's if we
          MOVE.L      #Ahandle,$28          !don't handle the call.
          MOVE.L      #TIMER,$3C8           !Set vectors for this A Line
STR2      RTS                               !handler and timer.
          TRAP        #0
  IFNE  DEBUG
SAVEAP	DC.L	CALLA
  ENDC
*
*  Save Floating Point registers
*
FPSAVE:
          FSAVE       -(A1)                 !
          FMOVEM.X    FP0-FP7,-(A1)         !
          FMOVEM.L    FPCR/FPSR,-(A1)       !
          RTS                               !
*
*  Restore Floating Point registers
*
FPRESTORE:
          FMOVEM.L    (A1)+,FPCR/FPSR       !
          FMOVEM.X    (A1)+,FP0-FP7         !
          FRESTORE    (A1)+                 !
          RTS                               !
*
*   Timer interrupt handler
*
*  Delayed event insert queue header and format
*
* deiq  W  XXXX      - Count of events to insert
*       L  XXXXXXXX  - Insert pointer
*       L  XXXXXXXX  - Remove pointer
* 
*   32 entries of the following format.
*       L  XXXXXXXX  - flag address
*       W  XXXX      - Set/Clear and bit number
*       L  XXXXXXXX  - time value
*
*  Delayed event header and flag list
*
* devt  W  XXXX      - count of events in list
*
*   64 entries in the following format.  NOTE: top of list always has
*   the shortest wait time.
*
*       L  XXXXXXXX  - wait time in tics
*       L  XXXXXXXX  - flag address
*       W  XXXX      - Set/Reset flag and bit number
*
TIMER     MOVEM.L     D0-A6,-(SP)           !Save all registers
          MOVEA.L     SYRAM,A5              !Get system RAM address
          MOVEA.L     deiq+6(A5),A3         !Process the delayed event queue
TIM1      MOVE.L      (A3)+,D1              !Get flag test address
          BEQ.S       TIM11                 !Queue is empty
          MOVE.W      (A3),D4               !Get flag sense and bit number
          LEA         devt(A5),A0           !Pointer to delayed event list
          MOVEA.L     A0,A2                 !Copy pointer to A2
          MOVE.W      (A0),D0               !Get count of delayed events
TIM2      ADDQ        #2,A0                 !Increment delayed event pointer
          SUBQ        #1,D0                 !Decrement count
          BLT.S       TIM4                  !End of list
          MOVEA.L     A0,A1                 !Copy pointer to this entry to A1
          MOVE.L      (A0)+,D2              !Get delay time
          CMP.L       (A0)+,D1              !Compare flag address with insert
          BNE         TIM2                  !No match
          CMP.B       1(A0),D4              !Compare bit number with insert
          BNE         TIM2                  !No match
          ADDQ        #2,A0                 !Flag address and bit number in
          ADD.L       D2,(A0)               !the insert queue matches a list
TIM3      MOVE.L      (A0)+,(A1)+           !entry.  Delete this delayed event
          MOVE.L      (A0)+,(A1)+           !from the list
          MOVE.W      (A0)+,(A1)+           !
          SUBQ        #1,D0                 !Move remainder fo list
          BGT         TIM3                  !
          SUBQ        #1,(A2)               !Decrement list count
TIM4      MOVE.L      2(A3),D2              !Get insert time value
          BEQ.S       TIM10                 !
          MOVEA.L     A2,A0                 !Pointer to top of delayed list
          MOVE.W      (A0)+,D0              !Get count of delayed events
          BEQ.S       TIM9                  !There are none. Just insert new one
TIM5      TST.L       (A0)                  !Test time
          BEQ.S       TIM6                  !
          CMP.L       (A0),D2               !Compare list time to new time
          BLO.S       TIM7                  !New time less than entry
          SUB.L       (A0),D2               !
TIM6      ADDQ        #8,A0                 !Increment delayed list pointer
          ADDQ        #2,A0                 !to next entry.
          SUBQ        #1,D0                 !Decrement count in list
          BGT         TIM5                  !Test next entry
          BRA.S       TIM9                  !New event later than all others.
*
TIM7      SUB.L       D2,(A0)               !
          MULU.W      #10,D0                !Move list down to make room for
          LEA         0(A0,D0.W),A1         !new delayed event.
TIM8      MOVE.W      -(A1),10(A1)          !
          CMPA.L      A0,A1                 !
          BHI         TIM8                  !
TIM9      MOVE.L      D2,(A0)+              !Put in new delayed event
          MOVE.L      D1,(A0)+              !
          MOVE.W      D4,(A0)               !
          ADDQ        #1,(A2)               !Increment list count
TIM10     CLR.L       -(A3)                 !Clear insert queue entry
          ADDQ        #8,A3                 !Increment insert queue pointer 
          ADDQ        #2,A3                 !
          PEA         devt(A5)              !Check for end of insert queue
          CMPA.L      (SP)+,A3              !
          BLO         TIM1                  !Loop till all inserts are processed
          LEA         deiq+10(A5),A3        !Point back to top of insert queue
          BRA         TIM1                  !
*
*  Have finished processing the insert queue list(or queue was empty).
*
TIM11     CLR         deiq(A5)              !Clear count in insert queue
          SUBQ        #4,A3                 !
          MOVE.L      A3,deiq+6(A5)         !Save extract queue pointer
*
*  Now process the delayed event list.
*
          LEA         devt(A5),A1           !Pointer to top of list
          MOVE.W      (A1)+,D0              !Get count in list
          BEQ.S       TIM17                 !Nothing there
          SUBQ.L      #1,(A1)+              !Decrement time value
          BNE.S       TIM17                 !Delay time remains
TIM12     MOVEA.L     (A1)+,A0              !Delay expiration.  Get flag address
          MOVE.W      (A1)+,D1              !and sense and bit number
          SUBQ        #1,D0                 !Decrement delayed event list count
          MOVE.W      D0,devt(A5)           !
          BEQ.S       TIM14                 !List is now empty
          LEA         -10(A1),A2            !Move remaining events up one slot
TIM13     MOVE.L      (A1)+,(A2)+           !
          MOVE.L      (A1)+,(A2)+           !
          MOVE.W      (A1)+,(A2)+           !
          SUBQ        #1,D0                 !
          BGT         TIM13                 !
TIM14     TST         D1                    !Test sense
          BMI.S       TIM15                 !positive = clear flag
          BCLR        D1,(A0)               !Clear event flag
          BRA.S       TIM16                 !
*
TIM15     BSET        D1,(A0)               !Set event flag
TIM16     LEA         devt(A5),A1           !Point back to top of delayed event 
          MOVE.W      (A1)+,D0              !list.  Get new count of delayed
          BEQ.S       TIM17                 !events. If empty, we are done.
          TST.L       (A1)+                 !See if other events also expire 
          BEQ         TIM12                 !at this time.
*
*   Update tics counter and process system timer events
*
TIM17     ADDQ.L      #1,tics(A5)           !Update tics counter
          ADDQ        #1,fcnt(A5)           !Update fine counter
          ADDQ.L      #1,clk100(A5)         !Update Acq system 100 Hz counter
          MOVEA.L     (A5),A0               !
          LEA         evtm(A5),A2           !Events 112-115 timers
          MOVEQ       #$80,D0               !
          MOVEQ       #-4,D1                !
TIM18     SUBQ.L      #1,(A2)+              !Decrement counter
          BGT.S       TIM19                 !
          OR.B        D0,evts(A5)           !Set event flag
          MOVE.L      16(A0,D1.W),-4(A2)    !Timer preset value
TIM19     ADDQ        #4,D1                 !
          BNE.S       TIM20                 !
          CMPI        #100,fcnt(A5)         !
          BLO.S       TIM23                 !
          SUBI        #100,fcnt(A5)         !
TIM20     ROR.B       #1,D0                 !
          CMPI        #12,D1                !
          BLO         TIM18                 !Loop till all counters updated
          MOVE.B      ssec1(A5),D0          !
          JSR         $42(A0)               !
          ADDA.L      $5E(A0),A0            !
          JSR         (A0)                  !
          LEA         TIM_LIMIT(PC),A0      !Update calendar time
          CLR         D1                    !
TIM21     MOVE.B      (A0)+,D1              !
          LEA         0(A5,D1.W),A1         !
          ADDQ.B      #1,(A1)               !
          MOVE.B      (A0)+,D0              !
          BNE.S       TIM22                 !
          CLR         D0                    !
          MOVE.B      smon(A5),D0           !
          MOVE.B      DAYS_MON(PC,D0.W),D0  !
TIM22     CMP.B       (A1),D0               !
          BHI.S       TIM23                 !
          MOVE.B      (A0)+,(A1)            !
          BRA         TIM21                 !
*
*
TIM_LIMIT DC.B        38,60,0,37,60,0,36,24,0,33,0,1
          DC.B        32,13,1,34,139
DAYS_MON  DC.B        0,32,30,32,31,32,31,32,32,31,32,31,32
*
TIM23     SUBQ.B      #1,utim(A5)           !Decrement time slice counter
          BGT         XSWP10                !User has time left!
          CLR.B       utim(A5)              !Clear time slice counter.
          MOVEQ       #7,D0                 !Extract interrupt mask of
          AND.B       ISAV_SR(SP),D0        !the interrupted task.
          SUBI.B      #0,D0                 !Allow reschedule by the clock
          BGT         XSWP10                !only if the interrupted task
*
*          MOVE        #$2000,SR             !had all interrupts enabled.
*
          MOVE        #$2700,SR             !DISABLE all maskable interrupts.
*
*  XSWP - Swap task
*
XSWP      TAS         tlck1(A5)             !Lock this task
          BPL.S       XSWP1                 !Was not already locked.
          CLR.B       tlck2(A5)             !Task was already locked so just
          BRA         XSWP10                !exit
*
XSWP1     MOVEA.L     utcb(A5),A6           !Get current task TCB address
          MOVE        USP,A1                !Get user stack pointer into A1
          TST.B       svf(A6)               !Do we save FP registers?
          BEQ.S       XSWP2                 !NO.
          BSR         FPSAVE                !YES. go save them
XSWP2     MOVE.L      A1,-(SP)              !Save user stack pointer on stack
          MOVE.L      SP,tsp(A6)            !Save current task stack pointer
XSWP3     LEA         tque(A5),A2           !Get pointer to highest priority
          TAS         tqux(A5)              !task.
          BMI.S       XSWP4                 !
          BSR         PRIQUE                !
XSWP4     MOVE.W      (A2)+,D2              !Get priority and task number
          BEQ         XSWP3                 !End of task queue list. Loop.
          MOVE.W      D2,tpry(A5)           !Save priority and task number
          EXT         D2                    !extract task number
          LSL         #4,D2                 !index into task list
          LEA         (tlist,A5,D2.W),A3    !Get pointer to task entry
          MOVE.W      8(A3),D1              !Get Event 2/Event 1 numbers
          BEQ.S       XSWP6                 !Task is NOT in event wait state.
          JSR         (tsev,A5,D2.W*2)      !Execute event flag test routine
          BEQ         XSWP4                 !No test is true. Check next task.
          BGT.S       XSWP5                 !Event 1 is true
          LSR         #8,D1                 !Event 2 is true
XSWP5     MOVE.B      D1,11(A3)             !Save flag which was true.
          CLR         8(A3)                 !Clear Event 2/Event 1 numbers
XSWP6     MOVE.W      -(A2),D0              !Get priority and task no again
          MOVE.B      (A2),D3               !Get just the priority
          MOVE        #$2700,SR             !
XSWP7     MOVE.W      2(A2),(A2)+           !Reorder tasks with same priority
          BEQ.S       XSWP8                 !End of task queue list
          CMP.B       (A2),D3               !compare priority of this task
          BLS         XSWP7                 !with the one selected to run
XSWP8     MOVE.W      D0,-(A2)              !Put selected task back in queue
          MOVE.L      A3,tltp(A5)           !Save task list pointer
          MOVE.W      (A3)+,sptn(A5)        !
          MOVEA.L     (A3)+,A6              !Get new task TCB address
          MOVE.L      A6,utcb(A5)           !Save it
          MOVE.W      (A3)+,D0              !
          MOVEA.L     tsp(A6),SP            !Get new task stack pointer
          MOVEA.L     (SP)+,A1              !Get new task USP
          TST.B       svf(A6)               !Were FP registers saved?
          BEQ.S       XSWP9                 !NO.
          BSR         FPRESTORE             !YES. Restore FP registers.
XSWP9     MOVE        A1,USP                !
          MOVE.L      (A3),D1               !
          BEQ.S       XULT                  !New task was not in wait state.
          CLR.L       (A3)                  !
          CMPI        #-2,D1                !
          BHI.S       XSWP11                !
          EXT         D1                    !Return the event number which
          EXT.L       D1                    !caused reschedule in D0.L
          MOVE.L      D1,(SP)               !Replace D0.L on stack.
          CLR.B       tlck1(A5)
          TAS         tlck2(A5)
          BRA.S       XSWP10
*
*  XULT - Unlock task
*
XULT      CLR.B       tlck1(A5)             !Unlock task
          TAS         tlck2(A5)             !Was there a reschedule attempt?
          BPL         XSWP                  !Yes.  Reschedule.
XSWP10:
          MOVEM.L     (SP)+,D0-A6           !No.
          RTE
*
XSWP11    LEA         tsp-2(A6),SP          !
          SF          (SP)                  !
          BEQ.S       XSWP13                !
          PEA         KILTSK(PC)            !
XSWP12    CLR         -(SP)                 !
          MOVEM.L     D0-A6,-(SP)           !
          BRA         XULT                  !
*
XSWP13    BSR         XSWP12                !
          BSR         RSETDSK               !
          MOVEQ       #85,D0                !
          XERR                              !
*
KILTSK    BSR         RSETDSK               !
          MOVEQ       #$FF,D1               !
          XRST                              !
          MOVEQ       #0,D0                 !
          MOVE.B      tskn(A5),D0           !Get task number
          XLKT                              !Lock task
          BSR.S       KILSUB                !
          MOVEQ       #0,D1                 !
          MOVE.B      prt(A6),D1            !
          CLR.B       patb(A5,D1.W)         !
          MOVEA.L     tltp(A5),A2           !Get pointer to task list entry
          TST.B       (A2)                  !Test task number
          BLT.S       KILTSK1               !Do not deallocate memory
          MOVEA.L     A6,A0                 !
          MOVE.L      eum(A6),D0            !
          SUB.L       A6,D0                 !
          LSR.L       #8,D0                 !
          LSR.L       #2,D0                 !
          XFUM                              !Free task memory
KILTSK1   CLR         (A2)                  !Clear task list entry
          LEA         tque(A5),A2           !Remove this task from task queue
KILTSK2   MOVE.W      (A2)+,D2              !
          CMP.B       tskn(A5),D2           !
          BNE         KILTSK2               !
KILTSK3   MOVE.W      (A2)+,-4(A2)          !
          BNE         KILTSK3               !
          XULT                              !Unlock task
          XSWP                              !
*
KILSUB    LEA         sfp(A6),A0            !
          CLR.L       imp(A6)               !
          TST.L       (A0)                  !
          BEQ.S       KILSUB2               !
KILSUB1   MOVEA.L     (A0),A1               !
          MOVE.L      A1,eum(A6)            !
          MOVE.L      -8(A1),(A0)           !
          BNE         KILSUB1               !
KILSUB2   RTS                               !
*
PRIQUE    MOVEQ       #$7F,D2               !
          AND.B       tqux(A5),D2           !
PRIQUE1   MOVE.W      (A2)+,D1              !
          BEQ.S       PRIQUE2               !
          CMP.B       D1,D2                 !
          BNE         PRIQUE1               !
          SUBQ        #2,A2                 !
          RTS                               !
*
PRIQUE2   LEA         tque(A5),A2           !
          RTS                               !
*
RSETDSK   MOVE.B      tskn(A5),D0           !
          MOVEQ       #$C0,D1               !
          AND.B       evts+1(A5),D1         !
          BPL.S       RSETDSK1              !
          CMP.B       l2lk(A5),D0           !
          BNE.S       RSETDSK1              !
          BCLR        #7,evts+1(A5)         !
RSETDSK1  TST.B       D1                    !
          BEQ.S       RSETDSK2              !
          CMP.B       l3lk(A5),D0           !
          BNE.S       RSETDSK2              !
          BCLR        #6,evts+1(A5)         !
RSETDSK2  RTS                               !
*
*
*   A-Line exception handler
*
Ahandle:
          MOVEM.L     D7/A6,-(SP)           !Save some registers
          MOVEA.L     10(SP),A6             !Get address of the exception
          MOVE.W      (A6),D7               !Get the A line instruction
          ANDI        #$FFE,D7              !
          CMPI        #$116,D7
          BLE.S       Ahandle1
A_PASS    MOVEM.L     (SP)+,D7/A6           !We are not interested in these.
          MOVE.L      Old_Ahandle,-(SP)
          RTS
*
*  Save here, at startup, the address stored in vector 10.
*
Old_Ahandle:
          DC.L        0
*
Ahandle1:
    IFNE  DEBUG
	MOVE.L	A1,-(SP)
	MOVE.L	#CALLA,A1
	MOVE.L	A6,(A1)+
	MOVE.W	D7,(A1)+
        ADDQ.L  #1,(A1)
	MOVE.L	(SP)+,A1
    ENDC
          MOVEA.W     ADISPATCH(PC,D7.W),A6
          JMP         ADISPATCH(PC,A6.L)
*
ADISPATCH:
*
          DC.W     LXSWP-ADISPATCH   !XSWP - Swap to next task
          DC.W      XSMP-ADISPATCH   !XSMP - Send message pointer
          DC.W      XGMP-ADISPATCH   !XGMP - Get message pointer
          DC.W    A_PASS-ADISPATCH   !X881 - Save 68881 enable
          DC.W      XUSP-ADISPATCH   !XUSP - go to user mode
          DC.W    A_PASS-ADISPATCH   !XPAD - Pack ASCII date
          DC.W    A_PASS-ADISPATCH   !XERR - Return to VEMPROM with error
          DC.W    A_PASS-ADISPATCH   !XEXT - Return to VMEPROM
*  A010
          DC.W    A_PASS-ADISPATCH   !XGML - Get memory limits
          DC.W    A_PASS-ADISPATCH   !XRTS - Reads task status
          DC.W    A_PASS-ADISPATCH   !XLKT - Lock task
          DC.W    A_PASS-ADISPATCH   !XULT - Unlock task
          DC.W      XSEF-ADISPATCH   !XSEF - Set/Reset event flag & swap
          DC.W      XTEF-ADISPATCH   !XTEF - Test event flag
          DC.W      XSUI-ADISPATCH   !XSUI - Suspend until interrupt
          DC.W      XGTM-ADISPATCH   !XGTM - Get task message
*  A020
          DC.W      XSTM-ADISPATCH   !XSTM - Send task message
          DC.W    A_PASS-ADISPATCH   !XGTP
          DC.W    A_PASS-ADISPATCH   !XDTV - Define trap vectors
          DC.W      XCTB-ADISPATCH   !XCTB - Create task control block
          DC.W      XKTM-ADISPATCH   !XKTM - Kill task message
          DC.W    A_PASS-ADISPATCH   !XRDM - Dump registers
          DC.W      XSUP-ADISPATCH   !XSUP - go to supervisor mode
          DC.W      XLSR-ADISPATCH   !XLSR - Load status register
*  A030
          DC.W    A_PASS-ADISPATCH   !XEXC - Execute PDOS call
          DC.W      XDEV-ADISPATCH   !XDEV - Delay Set/Reset event flag
          DC.W    A_PASS-ADISPATCH   !XRTP - Read time parameters
          DC.W    A_PASS-ADISPATCH   !XUAD - Unpack ASCII date
          DC.W    A_PASS-ADISPATCH   !XBUG
          DC.W    A_PASS-ADISPATCH   !XLER - Load error register
          DC.W    A_PASS-ADISPATCH   !XSTP - Set/Read task priority
          DC.W    A_PASS-ADISPATCH   !XGUM - Get user memory
*  A040
          DC.W    A_PASS-ADISPATCH   !XFUM - Free user memory
          DC.W      XRSR-ADISPATCH   !XRSR - Read status register
          DC.W    A_PASS-ADISPATCH   !XRTE - Return from interrupt
          DC.W      XSEV-ADISPATCH   !XSEV - Set/Reset event flag
          DC.W    A_PASS-ADISPATCH   !XGCB - Conditional get character
          DC.W    A_PASS-ADISPATCH   !XDMP - dump memory from stack
          DC.W    A_PASS-ADISPATCH   !XEXZ
          DC.W    A_PASS-ADISPATCH   !XPCB
*  A050
          DC.W    A_PASS-ADISPATCH   !XCBD - Convert to decimal
          DC.W    A_PASS-ADISPATCH   !XCBH - Convert to hex
          DC.W    A_PASS-ADISPATCH   !XCBM - Convert to decimal w/message
          DC.W    A_PASS-ADISPATCH   !XCDB - Convert ASCII to binary
          DC.W    A_PASS-ADISPATCH   !XFTD - Fix time and date
          DC.W    A_PASS-ADISPATCH   !XGNP - Get next parameter
          DC.W    A_PASS-ADISPATCH   !XRDT - Read date
          DC.W    A_PASS-ADISPATCH   !XRTM - Read time
*  A060
          DC.W    A_PASS-ADISPATCH   !XUDT - Unpack date
          DC.W    A_PASS-ADISPATCH   !XUTM - Unpack time
          DC.W    A_PASS-ADISPATCH   !XWDT - Write date
          DC.W    A_PASS-ADISPATCH   !XWTM - Write time
          DC.W    A_PASS-ADISPATCH   !XCHX - Convert to hex in buffer
          DC.W    A_PASS-ADISPATCH   !XCBX - Convert to decimal in buffer
          DC.W    A_PASS-ADISPATCH   !XAIM - 
          DC.W    A_PASS-ADISPATCH   !XPEL - Put encoded line to console
*  A070
          DC.W    A_PASS-ADISPATCH   !XBCP - Baud console port
          DC.W    A_PASS-ADISPATCH   !XCBC - check for break
          DC.W    A_PASS-ADISPATCH   !XCBP - Check for break or pause
          DC.W    A_PASS-ADISPATCH   !XCLS - Clear screen
          DC.W    A_PASS-ADISPATCH   !XGCC - Get character conditional
          DC.W    A_PASS-ADISPATCH   !XGCR - Get character
          DC.W    A_PASS-ADISPATCH   !XGLB - Get line in buffer
          DC.W    A_PASS-ADISPATCH   !XGLM - Get line in monitor buffer
*  A080
          DC.W    A_PASS-ADISPATCH   !XGLU - Get line in user buffer
          DC.W    A_PASS-ADISPATCH   !XGLX
          DC.W    A_PASS-ADISPATCH   !XPBC - Put buffer to console
          DC.W    A_PASS-ADISPATCH   !XPCC - Put character(s) to console
          DC.W    A_PASS-ADISPATCH   !XPCL - Put CRLF to console
          DC.W    A_PASS-ADISPATCH   !XPLC - Put line to console
          DC.W    A_PASS-ADISPATCH   !XPMC - Put message to console
          DC.W    A_PASS-ADISPATCH   !XPSC - Position cursor
*  A090
          DC.W    A_PASS-ADISPATCH   !XTAB - Tab to column
          DC.W    A_PASS-ADISPATCH   !XRCP - Read port cursor position
          DC.W    A_PASS-ADISPATCH   !XRPS - Read port status
          DC.W    A_PASS-ADISPATCH   !XPDC - Put data to console
          DC.W    A_PASS-ADISPATCH   !XPSP - Put space to console
          DC.W    A_PASS-ADISPATCH   !XSPF - 
          DC.W    A_PASS-ADISPATCH   !XPEM - Put encode message to console
          DC.W    A_PASS-ADISPATCH   !XGCP - Get port character
*  A0A0
          DC.W    A_PASS-ADISPATCH   !XFFN - Fix file name
          DC.W    A_PASS-ADISPATCH   !XLFN - Look for name in file slots
          DC.W    A_PASS-ADISPATCH   !XLST - 
          DC.W    A_PASS-ADISPATCH   !XRDE - Read next directory entry
          DC.W    A_PASS-ADISPATCH   !XRDN - Read directory entry by name
          DC.W    A_PASS-ADISPATCH   !XAPF - Append file
          DC.W    A_PASS-ADISPATCH   !XCHF - 
          DC.W    A_PASS-ADISPATCH   !XCPY - Copy file
*  A0B0
          DC.W    A_PASS-ADISPATCH   !XLDF - Load file
          DC.W    A_PASS-ADISPATCH   !XRCN - Reset console inputs
          DC.W    A_PASS-ADISPATCH   !XRST - Reset disk
          DC.W    A_PASS-ADISPATCH   !XSZF - Get disk size
          DC.W    A_PASS-ADISPATCH   !XBFL - 
          DC.W    A_PASS-ADISPATCH   !XPCR - Put character raw
          DC.W    A_PASS-ADISPATCH   !XPCP - Place character in port buffer
          DC.W    A_PASS-ADISPATCH   !XBER - 
*  A0C0
          DC.W    A_PASS-ADISPATCH   !XISE - Initialize sector
          DC.W    A_PASS-ADISPATCH   !XRSE - Read sector
          DC.W    A_PASS-ADISPATCH   !XRSZ - Read sector zero
          DC.W    A_PASS-ADISPATCH   !XWSE - Write sector
          DC.W    A_PASS-ADISPATCH   !
          DC.W    A_PASS-ADISPATCH   !
          DC.W    A_PASS-ADISPATCH   !
          DC.W    A_PASS-ADISPATCH   !XFAC - File altered check
*  A0D0
          DC.W    A_PASS-ADISPATCH   !XCFA - Close file w/attribute
          DC.W    A_PASS-ADISPATCH   !XCLF - Close file
          DC.W    A_PASS-ADISPATCH   !XDFL - Define file
          DC.W    A_PASS-ADISPATCH   !XDLF - Delete file
          DC.W    A_PASS-ADISPATCH   !XLKF - Lock file
          DC.W    A_PASS-ADISPATCH   !XNOP - Open shared random file
          DC.W    A_PASS-ADISPATCH   !XPSF - Position file
          DC.W    A_PASS-ADISPATCH   !XRBF - Read bytes from file
*  A0E0
          DC.W    A_PASS-ADISPATCH   !XRFA - Read file attributes
          DC.W    A_PASS-ADISPATCH   !XRLF - Read line from file
          DC.W    A_PASS-ADISPATCH   !XENF - 
          DC.W    A_PASS-ADISPATCH   !XROO - Open random read only file
          DC.W    A_PASS-ADISPATCH   !XROP - Open random
          DC.W    A_PASS-ADISPATCH   !XRWF - Rewind file
          DC.W    A_PASS-ADISPATCH   !XSOP - Open sequential file
          DC.W    A_PASS-ADISPATCH   !XULF - Unlock file
*  A0F0
          DC.W    A_PASS-ADISPATCH   !XWBF - Write bytes to file
          DC.W    A_PASS-ADISPATCH   !XWFA - Write file attributes
          DC.W    A_PASS-ADISPATCH   !XWLF - Write line to file
          DC.W    A_PASS-ADISPATCH   !XZFL - Zero file
          DC.W    A_PASS-ADISPATCH   !XFBF - Flush buffers
          DC.W      XKTB-ADISPATCH   !XKTB - Kill task
          DC.W    A_PASS-ADISPATCH   !XWFP - Write file parameters
          DC.W    A_PASS-ADISPATCH   !XRFP - Read file position
*  A100
          DC.W    A_PASS-ADISPATCH   !XIT0
          DC.W    A_PASS-ADISPATCH   !XIT1
          DC.W    A_PASS-ADISPATCH   !XIT2
          DC.W    A_PASS-ADISPATCH   !XIT3
          DC.W    A_PASS-ADISPATCH   !XIT4
          DC.W    A_PASS-ADISPATCH   !XIT5
          DC.W    A_PASS-ADISPATCH   !XIT6
          DC.W    A_PASS-ADISPATCH   !XIT7
*  A110
          DC.W    A_PASS-ADISPATCH   !XTLP - Translate logical to physical
          DC.W      XSOE-ADISPATCH   !XSOE - Suspend on physical event
          DC.W      XDPE-ADISPATCH   !XDPE - Delay physical event flag
          DC.W    A_PASS-ADISPATCH   !XVEC - Set/Read exception vector
*
*  LXSWP - Swap task
*
LXSWP:
    IFNE  DEBUG
          ADDQ.L      #1,XSWPCNT
    ENDC
          ADDQ.L      #2,10(SP)       !Increment return PC
          MOVE.L      (SP)+,D7        !Clean up stack
          MOVEM.L     D0-A5,-(SP)     !Save all registers for task swap
          MOVEA.L     SYRAM,A5        !Get SYRAM address
          BRA         XSWP            !Go swap to next task
*
*  XSOE  - Suspend on physical event
*
* Call:   D1.L  -  Event 1 descriptor/ Event 2 descriptor
*         A0    -  Event 2 address (0 = no event to suspend on)
*         A1    -  Event 1 address (0 = no event to suspend on)
*
* Return: D0    -  - 1 = Return due to event 2
*                  + 1 = Return due to event 1
*
XSOE:
    IFNE  DEBUG
          ADDQ.L      #1,XSOECNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      (SP)+,D7              !Clean up stack
          MOVEM.L     D0-A5,-(SP)           !Save all registers for task swap
          MOVE.L      SYRAM,A5              !Get SYRAM base address
          MOVE.W      #$1FF,D4              !Physical event flag wait
          BRA.S       XSUI1
*
*  XSUI -  Suspend until interrupt
*
* Call:   D1.W  -  Event 1/ Event 2 (positive event means wait till set
*                  negative event means wait till clear)
* Return: D0.L  -  Event # which caused return
*
XSUI:
    IFNE  DEBUG
          ADDQ.L      #1,XSUICNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      (SP)+,D7              !Clean up stack
          MOVEM.L     D0-A5,-(SP)           !Save all registers for task swap
          MOVE.L      SYRAM,A5              !Get SYRAM base address
          MOVE.L      D1,D4                 !Save event flags in D4
          BSR         EVTCOM                !Convert logical events to physical
XSUI1     CLR.L       (SP)                  !Clear return D0.L
          MOVEQ       #0,D0                 !
          MOVE.B      tskn(A5),D0           !Get task number
          LSL         #4,D0                 !Task number * 16
          LEA         evtlst(A5),A2         !Address of event parameter table
          ADDA        D0,A2                 !Index to this task's entry
          CMP.L       (A2)+,D1              !Check for change in event parameters
          BNE.S       XSUI1A                !If changed, must rebuild.
          CMPA.L      (A2)+,A0
          BNE.S       XSUI1A
          CMPA.L      (A2),A1
          BEQ.S       XSUI1B                !No need to rebuild flag test array
*
*  The event flag parameters have changed.  Must rebuild the flag test array.
*
XSUI1A    LSL         #1,D0                 !Task number * 32
          LEA         tsev(A5),A2           !Base address of flag test array
          ADDA        D0,A2                 !Add offset for this task
          BSR         XSUISET               !Build flag test routine
XSUI1B    MOVE.L      D4,D1                 !Get events
          TAS         tlck1(A5)             !Check for task locked
          BMI.S       XSUI2                 !Task is locked so wait here
          MOVEA.L     tltp(A5),A0           !Task is NOT locked
          SWAP        D1                    !
          CLR         D1                    !
          MOVE.L      D1,8(A0)              !Put wait flags in task list
          BRA         XSWP1                 !Reschedule
*
*  Task is locked.  Loop here until at least one event test satisfied.
*
XSUI2     JSR         (A2)                  !Execute event test routine
          BEQ         XSUI2                 !Do it again
          BGT.S       XSUI3                 !
          LSR         #8,D1                 !
XSUI3     EXT         D1                    !
          EXT.L       D1                    !
          MOVE.L      D1,(SP)               !Return in D0.L the event flag
          MOVEM.L     (SP)+,D0-A6           !which caused return
          RTE
*
*  XLSR  -  Load status register
*
* Call:   D1.W  - value to load into status register (SR)
*
XLSR:
    IFNE  DEBUG
          ADDQ.L      #1,XLSRCNT
    ENDC
          MOVE.W      D1,8(SP)      !Load return SR
          ADDQ.L      #2,10(SP)     !Increment return PC
          MOVEM.L     (SP)+,D7/A6
          RTE
*
*  XUSP  -  Switch to user mode
*
* Call:  no arguments
*
XUSP:
    IFNE  DEBUG
          ADDQ.L      #1,XUSPCNT
    ENDC
          BCLR        #5,8(SP)      !Clear Supervisor bit in return SR
          ADDQ.L      #2,10(SP)     !Increment return PC
          MOVEM.L     (SP)+,D7/A6 
          RTE
*
*  XSUP -  Switch to supervisor mode
*
XSUP:
    IFNE  DEBUG
          ADDQ.L      #1,XSUPCNT
    ENDC
          BSET        #5,8(SP)      !Set Supervisor bit in return SR
          ADDQ.L      #2,10(SP)     !Increment return PC
          MOVEM.L     (SP)+,D7/A6
          RTE
*
*  XRSR - Read status register
*
* Return:  D0.W  -  Status register SR
*
XRSR:
    IFNE  DEBUG
          ADDQ.L      #1,XRSRCNT
    ENDC
          MOVE.W      8(SP),D0      !Return call SR in D0.W
          ADDQ.L      #2,10(SP)     !Increment return PC
          MOVEM.L     (SP)+,D7/A6
          RTE
*
*  XTEF -  Test event flag
*
* Call:  D1.B - Event number - 128 is the task local event
*
* Return: Test status in SR
*
XTEF:
    IFNE  DEBUG
          ADDQ.L      #1,XTEFCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      SYRAM,A6              !Get system ram address
          MOVE.L      D1,-(SP)              !Save D1
          MOVEQ       #0,D7                 !
          MOVE.B      D1,D7                 !Get event number
          BPL.S       XTEF1                 !
          MOVE.B      tskn(A6),D7           !Local event 128. Get task number
          MOVE.B      D7,D1                 !
          ADDI        #(ev128-evtb)*8,D7    !Add local event array offset
XTEF1     LSR         #3,D7                 !compute byte offset
          NOT.B       D1                    !and bit test number
          BTST        D1,evtb(A6,D7.W)      !Test the bit
          MOVE        SR,D7                 !Return test status
          MOVE.B      D7,13(SP)
          MOVEM.L     (SP)+,D1/D7/A6        !restore registers
          RTE
*
*  XSEF -  Set/Reset event flag and swap task
*
* Call:  D1.B - event number (+ = set, - = reset except that -128 is
*               the local event flag).
* Return:  SR is NE if flag was already set and EQ is it was clear
*
XSEF:
    IFNE  DEBUG
          ADDQ.L      #1,XSEFCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      D1,-(SP)              !Save D1
          BSR.S       XSEV1                 !Test the flag
          MOVE        SR,D7                 !
          MOVE.B      D7,13(SP)             !Return the flag status
          MOVEM.L     (SP)+,D1/D7           !restore registers
          MOVEM.L     D0-A5,-(SP)
          BRA         XSWP                  !Reschedule
*
*  XSEV - Set/Reset event flag
*
* Call:  D1.B - event number (+ = set, - = reset except that -128 is
*               the local event flag).
* Return:  SR is NE if flag was already set and EQ is it was clear
*
XSEV:
    IFNE  DEBUG
          ADDQ.L      #1,XSEVCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      D1,-(SP)              !Save D1
          BSR.S       XSEV1                 !Test the event flag
          MOVE        SR,D7                 !Return test status
          MOVE.B      D7,13(SP)
          MOVEM.L     (SP)+,D1/D7/A6        !restore registers
          RTE

XSEV1:
          MOVE.L      SYRAM,A6              !Get system ram address
          CLR         D7                    !
          MOVE.B      D1,D7                 !Check for set or clear
          BMI.S       XSEV2                 !Clear or local 128
*
*  Event flag is positive.  Set flag and return previous flag status
*
          LSR         #3,D7                 !Compute byte address
          NOT.B       D1                    !
          BSET        D1,evtb(A6,D7.W)      !Set event flag
          RTS
*
*  Event flag is negative.  Clear flag and return previous flag status
*
XSEV2     NEG.B       D7                    !Check for local event 128
          BPL.S       XSEV3                 !Not local 128
          MOVE.B      tskn(A6),D7           !Local event 128.  Get task number.
          MOVE.B      D7,D1                 !
          NEG.B       D1                    !
          ADDI        #(ev128-evtb)*8,D7    !
XSEV3     LSR         #3,D7                 !Compute byte address of event flag
          SUBQ.B      #1,D1                 !Fix bit number
          BCLR        D1,evtb(A6,D7.W)      !Clear event flag
          RTS
*
EXTSWP    MOVE.B      D7,tlck1(A6)          !Restore lock state
EXTSWP1   TAS         tlck2(A6)             !Was there a reschedule attempt?
          BMI.S       EXTSWP2               !No.
          XSWP                              !Yes.  Reschedule.
EXTSWP2   RTS
*
*   XGMP  - Receive message pointer
*
* Call:   D0.L  -  Message slot number (0 thru 15)
*
* Return: D0.L  -  83 means message slot empty OR
*                  Source task number
*         A1    -  Pointer to message
*         SR  - EQ means message received 
*               NE means message slot not empty
*
XGMP:
    IFNE  DEBUG
          ADDQ.L      #1,XGMPCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVEM.L     D1/D6/A0,-(SP)        !Save registers we will use
          MOVEA.L     SYRAM,A6
          BSR.S       MSGCOM                !Check message slot for available
          BPL.S       MSGERR1               !message.  Positive means no
          MOVE        SR,-(SP)              !message available.
          ORI         #$700,SR
          NOT.B       D0                    !
          BCLR        D0,evtb(A6,D6.W)      !Clear flag
          NOT.B       D0                    !Restore D0
          CLR.B       0(A0,D0.W)            !Clear source task number
          MOVE.L      (16,A0,D0.W*4),A1     !Get message pointer
          NOT.B       D1                    !
          MOVE.L      D1,D0                 !Return source task number in D0.L
          MOVE        (SP)+,SR
          BSR         EXTSWP
          MOVEM.L     (SP)+,D1/D6/A0
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #4,1(SP)
          RTE
*
*   Message slot error.  On send message error, only the SR is set to indicate
*   that the message was not sent.  On get message error, both SR and D0.L
*   indicate that no message was available.
*
MSGERR:
          MOVE.L      (SP)+,D0
MSGERR1:
          MOVEQ       #83,D0
          BSR         EXTSWP
          MOVEM.L     (SP)+,D1/D6/A0
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #0,1(SP)
          RTE
*
*  Compute message pointer address and check for message present.
*  Return positive is message slot is empty and negative is  it is full.
*
MSGCOM    MOVEQ       #$F,D1                !Mask to 4 bits. I.E. 0 thru 15
          AND.L       D1,D0                 !
          MOVE.W      D0,D6                 !
          LSR         #3,D6                 !Divide slot by 8
          ADDQ        #8,D6                 !Compute byte address of flag
          TAS         tlck1(A6)             !Check for locked task
          SMI         D7                    !Remember lock status
          LEA         tmsp(A6),A0           !
          MOVE.B      0(A0,D0.W),D1         !Get source task number
          RTS
*
*   XSMP - Send message pointer
*
* Call:  D0.B  -  Message slot number (0 thru 15)
*        A1    -  pointer to message
*
* Return:  SR  - EQ means message sent
*                NE means message slot not empty and message not sent
*
XSMP:
    IFNE  DEBUG
          ADDQ.L      #1,XSMPCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVEM.L     D0-D1/D6/A0,-(SP)
          MOVEA.L     SYRAM,A6
          BSR         MSGCOM                !Check for message slot empty
          BMI         MSGERR                !NOT empty, return error.
          MOVE        SR,-(SP)
          ORI         #$700,SR
          NOT.B       D0                    !
          BSET        D0,evtb(A6,D6.W)      !Set associated flag
          NOT.B       D0                    !
          MOVE.B      tskn(A6),D1           !Get our task number
          NOT.B       D1                    !
          MOVE.B      D1,0(A0,D0.W)         !Store as source task number
          MOVE.L      A1,(16,A0,D0.W*4)     !
          MOVE        (SP)+,SR
          BSR         EXTSWP
          MOVEM.L     (SP)+,D0-D1/D6/A0
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #4,1(SP)
          RTE
*
*   Convert logical event flags to physical event format
*
EVTCOM    MULU.W      #1,D1                 !
          ROL.L       #8,D1                 !Put event 1 in upper half reg
          LSR         #8,D1                 !Restore event 2 in lower half
          SWAP        D1                    !
          SUBA.L      A0,A0                 !Clear A0
          TST         D1                    !test event 1
          BEQ.S       EVTCOM1               !No flag specified
          BSR.S       EVTCOM2               !Process event 1
EVTCOM1   MOVEA.L     A0,A1                 !
          SWAP        D1                    !Test event 2
          SUBA.L      A0,A0                 !Clear A0
          TST         D1                    !
          BEQ.S       EVTCOM7               !No flag specified
*
EVTCOM2   MOVE.L      D0,-(SP)              !Save D0
          MOVEQ       #0,D0                 !
          MOVE.B      D1,D0                 !Get event number into D0
          BPL.S       EVTCOM4               !
          NEG.B       D1                    !Test for event 128
          BMI.S       EVTCOM3               !It is local 128
          MOVE.B      D1,D0                 !
          BSET        #31,D0                !
          BRA.S       EVTCOM4               !
*
EVTCOM3   MOVE.B      tskn(A5),D0           !Get task number for local 128
          MOVE.B      D0,D1                 !
          ADDI        #(ev128-evtb)*8,D0    !
EVTCOM4   NOT.B       D1                    !
          ANDI        #7,D1                 !Extract just the bit number
          CMPI        #$40,D0               !
          BLO.S       EVTCOM5               !
          ORI         #$8000,D1             !
EVTCOM5   LSR         #3,D0                 !Compute byte address of event flag
          LEA         evtb(A5,D0.W),A0      !
          TST.L       D0                    !
          BMI.S       EVTCOM6               !
          ORI         #$80,D1               !
EVTCOM6   MOVE.L      (SP)+,D0              !
EVTCOM7   RTS                               !
*
*  Build flag test execution routine
*
XSUISET   MOVEM.L     D3/A2-A3,-(SP)        !
          MOVE.L      A2,D3
          CMPA        #0,A0                 !
          BEQ.S       XSUISET1              !
          MOVE.W      #$7001,-(SP)          ! MOVEQ  #1,D0
          MOVE.L      A0,-(SP)              !
          MOVE.W      D1,-(SP)              !
          BSR.S       XSUISET3              !
XSUISET1  CMPA        #0,A1                 !
          BEQ.S       XSUISET2              !
          MOVE.W      #$70FF,-(SP)          ! MOVEQ  #-1,D0
          MOVE.L      A1,-(SP)              !
          SWAP        D1                    !
          MOVE.W      D1,-(SP)              !
          BSR.S       XSUISET3              !
XSUISET2  MOVE.W      #$7000,(A2)+          ! MOVEQ  #0,D0
          MOVE.W      #$4E75,(A2)           ! RTS
          ANDI.L      #$FFFFF000,D3
          MOVEA.L     D3,A3                 !Page address to A3
          CINVP       #2,(A3)               !Invalidate instruction cache page
          ADDA        #4096,A3
          CMPA.L      A2,A3                 !Check for crossing a page
          BHI.S       XSUISET2A             !Did not cross page.
          CINVP       #2,(A3)               !Invalidate another instruction
*                                           !cache page.
XSUISET2A:
          LSR         #1,D0
          LEA         evtlst(A5),A2
          ADDA        D0,A2
          SWAP        D1
          MOVE.L      D1,(A2)+
          MOVE.L      A0,(A2)+
          MOVE.L      A1,(A2)
          MOVEM.L     (SP)+,D3/A2-A3        !
          RTS                               !
*
XSUISET3  MOVEA.L     (SP)+,A3              !
          CMPI        #$8007,(SP)           !
          BEQ.S       XSUISET6              !
          TST         (SP)                  !
          BMI.S       XSUISET4              !
          MOVE.W      #$839,(A2)+           ! BTST  #bit,Address
          BRA.S       XSUISET5              !
*
XSUISET4  MOVE.W      #$8B9,(A2)+           ! BCLR  #bit,Address
          TST.B       1(SP)                 !
          BMI.S       XSUISET5              !
          MOVE.W      #$8F9,-2(A2)          ! BSET  #bit,Address
XSUISET5  MOVE.W      (SP)+,(A2)+           !
          MOVE.L      (SP)+,(A2)+           !
          MOVE.W      #$6704,-(SP)          ! BEQ  (PC)+4
          TST.B       -5(A2)                !
          BMI.S       XSUISET7              !
          MOVE.W      #$6604,(SP)           ! BNE  (PC)+4
          BRA.S       XSUISET7              !
*
XSUISET6  ADDQ        #2,SP                 !
          MOVE.W      #$4AF9,(A2)+          ! TAS  #bit,Address
          MOVE.L      (SP)+,(A2)+           !
          MOVE.W      #$6B04,-(SP)          ! BMI  (PC)+4
XSUISET7  MOVE.W      (SP)+,(A2)+           !
          MOVE.W      (SP)+,(A2)+           !
          MOVE.W      #$4E75,(A2)+          !
          JMP         (A3)                  !
*
*  XDEV  - Delay Set/Reset event flag
*
* Call:   D0.L  - Time tics to wait
*         D1.B  - Event ( += set after delay, -=clear after delay)
*
XDEV:
    IFNE  DEBUG
          ADDQ.L      #1,XDEVCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      SYRAM,A6
          MOVEM.L     D1-D2/A0-A2,-(SP)
          TAS         tlck1(A6)             !lock task
          SMI         D7                    !Remember previous lock state
          MOVE.L      A5,-(SP)
          MOVE.L      A6,A5
          BSR         EVTCOM                !Convert logical to physical event
          MOVE.L      (SP)+,A5
          BSET        D1,(A0)               !Set event flag
          TST.B       D1                    !
          BPL.S       XDPE1                 !Clear event flag after time delay
          BCLR        D1,(A0)               !Clear event flag
          BRA.S       XDPE1                 !Set event flag after time delay
*
*  XDPE - Delay physical event
*
* Call:   A0   - event address
*         D0.L - Time tics to wait(100 tics per second)
*         D1.W - event descriptor
*
XDPE:
    IFNE  DEBUG
          ADDQ.L      #1,XDPECNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      SYRAM,A6
          MOVEM.L     D1-D2/A0-A2,-(SP)
          TAS         tlck1(A6)             !Test for locked task
          SMI         D7                    !Remember test result
XDPE1     LEA         deiq(A6),A2           !Get delayed event insert address
          MOVE.W      (A2)+,D2              !Get size of insert queue
          TST.L       D0                    !Test for zero time
          BEQ.S       XDPE2                 !Delete delayed event if time = 0
          ADD         devt(A6),D2           !Add number of events in delay list
          CMPI        #64,D2                !Compare with max
          BHI.S       XDPE5                 !Too many delayed events
XDPE2     MOVEA.L     (A2),A1               !Get queue insert pointer
          TST.L       (A1)                  !Test for empty slot
          BNE.S       XDPE5                 !NOT empty - how can this be??
          MOVE.L      D0,6(A1)              !Set delay time tics
          MOVE.W      D1,-(SP)              !Save event descriptor
          ANDI        #7,(SP)               !Extract just the bit number
          TST.B       D1                    !Test for Set or Reset
          SMI         (SP)                  !
          MOVE.W      (SP)+,4(A1)           !Put sense and bit number in list
          MOVE.L      A0,(A1)               !Put in flag address
          TST.L       D0                    !Test time value
          BEQ.S       XDPE3                 !Time is zero.
          ADDQ        #1,-2(A2)             !Nonzero time, increment count
XDPE3     ADDQ        #8,A1                 !Increment insert pointer
          ADDQ        #2,A1                 !
          PEA         devt(A6)              !Check for end of list
          CMPA.L      (SP)+,A1              !
          BLO.S       XDPE4                 !Not end of list
          LEA         deiq+10(A6),A1        !Reset pointer back to start of list
XDPE4     MOVE.L      A1,(A2)               !Save insert pointer
          BSR         EXTSWP
          MOVEM.L     (SP)+,D1-D2/A0-A2
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #4,1(SP)
          RTE
*
XDPE5     MOVEQ       #83,D0                !Delay event queue full
          BSR         EXTSWP
          MOVEM.L     (SP)+,D1-D2/A0-A2
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #0,1(SP)
          RTE
*
*
*  XCTB  -  Create task control block
*
* Call:  D0.W  -   Task size in 1K byte increments.
*                  D0.W positive and != 0, then A0 and A1 are undefined.
*                  D0.W negative, A0 and A1 are memory bounds. A2 points
*                                 to the command line string.
*                  D0.W = 0, A0 and A1 are memory bounds and A2 is start address
*        D1.W  -   Task time/Priority
*        D2.W  -   I/O port
*
*        A0    - Low memory address
*        A1    - High memory address
*        A2    - Command line pointer or start address
*
XCTB:
    IFNE  DEBUG
          ADDQ.L      #1,XCTBCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      (SP)+,D7
          MOVEM.L     D0-A5,-(SP)
          MOVE.L      SYRAM,A6
          MOVE.L      utcb(A6),A5
          TST         tque+63*2(A6)         !Check for available slot in queue
          BNE.S       XCTB1                 !No room in the inn
          TST         D0                    !Test size request
          BEQ.S       XCTB3                 !Memory specified in A0 and A1
          BMI.S       XCTB5                 !A2 is command line pointer
          XGUM                              !Otherwise, request memory
          BEQ.S       XCTB5                 !Memory was available.
          MOVEA.L     eum(A5),A1            !Memory not available.  Take it from
          MOVEA.L     A1,A0                 !parent task
          MOVE.L      (SP),D0               !Get size again
          ADDQ        #1,D0                 !
          ANDI        #-2,D0                !
          MULU.W      #1024,D0              !Get memory size in bytes
          SUBA.L      D0,A0                 !
          PEA         $1000(A5)             !See if size fits  within parent
          CMPA.L      (SP)+,A0              !task.
          BLS.S       XCTB2                 !Will not fit! Error.
          CMPA.L      A1,A0                 !
          BHI.S       XCTB2                 !
          MOVE.L      A0,eum(A5)            !Set new end of memory for parent
          MOVE        A0,USP                !Move parent user stack also
          CLR.B       mmf(A5)               !Clear memory modified flag
          BRA.S       XCTB5                 !
*
XCTB1     MOVEQ       #72,D0                !Too many tasks
          XIT0                              !
*
XCTB2     MOVEQ       #73,D0                !Not enough memory
          XIT0                              !
*
*  A2 is start address.  Save it in A3.
XCTB3     MOVEA.L     A2,A3                 !
          SUBA.L      A2,A2                 !Clear A2
          BRA.S       XCTB6                 !
*
XCTB4     XEXT                              !
*
XCTB5     LEA         XCTB4(PC),A3          !
XCTB6     TAS         tlck1(A6)             !Lock this task
          SMI         D7                    !Remember previous lock state
          LEA         tlist-16(A6),A4       !Get pointer to task list
          MOVEQ       #-1,D6                !
XCTB7     ADDQ.L      #1,D6                 !Search task list for a free slot
          ADDA        #16,A4                !
          TST         (A4)                  !
          BNE         XCTB7                 !
          MOVE.B      tid(A5),(A4)+         !Put parent task number in new slot
          MOVE.B      #1,(A4)+              !Default time slice
          MOVE.L      A0,(A4)+              !TCB address for new task
          CLR.L       (A4)+                 !Clear remainder of task entry
          CLR         (A4)                  !
          LEA         tque(A6),A4           !Get pointer to task queue
          MOVEQ       #-1,D0                !Set new task priority
          MOVE.B      D6,D0                 !Assign a task number
XCTB8     MOVE.W      (A4),D4               !Insert new task a top of queue
          MOVE.W      D0,(A4)+              !Move all existing entries down
          MOVE.W      D4,D0                 !until we find end of list
          BNE         XCTB8                 !
          CLR         (A4)                  !Mark end of task queue
          LEA         tsp(A0),A4            !Build stack for new task
          CLR         -(A4)                 !format/vector word
          EXG         A0,A3                 !
          LSL.L       #8,D1                 !
          MOVE.B      u2p(A5),D1            !
          LSL.L       #8,D1                 !
          MOVE.B      D2,D1                 !
          MOVEM.L     flg(A5),D0/D2-D5      !Get parameter child inherits from
          EXG         SP,A4                 !parent.
          PEA         STRTSK(PC)            !Put first execution address on stack
          MOVE.W      ISAV_SR(A4),-(SP)     !Parents status register
          MOVEM.L     D0-A6,-(SP)           !Save all registers
          MOVE.L      A1,-(SP)              !Save user stack pointer
          MOVE.L      SP,tsp(A3)            !Save new stack pointer in TCB
          CLR.B       tsp+12(A3)            !
          EXG         SP,A4                 !
          MOVE.L      D6,(SP)               !Return new task number to parent
          MOVE.L      D6,D0                 !
          MOVEQ       #$FF,D1               !
          MOVEA.L     A2,A1                 !
          MOVE.L      A1,D2                 !
          BEQ.S       XCTB9                 !
          BSR         XSTM2                 !
XCTB9     BSR         EXTSWP                !
          MOVEM.L     (SP)+,D0-A6
          MOVE.B      #4,1(SP)
          RTE
*
*  First execution of a new task is here
*
STRTSK    MOVEA.L     A1,SP                 !
          MOVEA.L     SYRAM,A5              !
          MOVEA.L     utcb(A5),A6           !
          MOVEA.L     A6,A4                 !
          MOVE.W      #$3FF,D6              !Clear all of new task TCB
STRTSK1   CLR.L       (A4)+                 !
          DBRA        D6,STRTSK1            !
          MOVE.W      #$A00E,(A4)           !Put XEXT at start of user memory
          MOVE.L      A4,bum(A6)            !
          MOVE.L      A4,ead(A6)            !
          MOVE.L      SP,eum(A6)            !
          MOVEM.L     D0/D2-D5,flg(A6)      !Put in parameters inherited from
          MOVE.B      tskn(A5),D0           !parent.
          MOVE.B      D0,tid(A6)            !Save our task number.
          NEG.B       D1                    !
          BPL.S       STRTSK2               !
          NEG.B       D1                    !
          CMPI.B      #16,D1                !
          BHI.S       STRTSK3               !
          MOVEQ       #0,D2                 !
          MOVE.B      D1,D2                 !
          LEA         patb(A5,D2.W),A4      !
          TST.B       (A4)                  !
          BNE.S       STRTSK2               !
          MOVE.B      D0,(A4)               !
          NOT.B       (A4)                  !
          MOVE.B      D1,prt(A6)            !
STRTSK2   ROR         #8,D1                 !
          MOVE.W      D1,u1p(A6)            !
          ADDQ.B      #1,unt(A6)            !
STRTSK3   SWAP        D1                    !
          XSTP                              !
          XSUP
          MOVE.L      SP,A4
          MOVE.L      A6,A3
STRTSK4   CINVP       #2,(A3)               !Invalidate instruction cache
          ADDA.W      #4096,A3
          CMPA.L      A4,A3
          BLO         STRTSK4
          XUSP
          MOVEQ       #0,D0                 !
          MOVE.B      tskn(A5),D0           !
          JMP         (A0)
*
*  XKTB - Kill task
*
* Call:  D0.B  _  Task number
*
XKTB:
    IFNE  DEBUG
          ADDQ.L      #1,XKTBCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVE.L      (SP)+,D7
          MOVEM.L     D0-A5,-(SP)
          MOVE.L      SYRAM,A5
          MOVE.B      tskn(A5),D1           !Get current task number
          MOVE.B      D0,D2                 !Test task number to kill
          BGT.S       XKTB1                 !Task is positive and > 0
          NEG.B       D2                    !
          BGT.S       XKTB1                 !Task is negative and < 0
          MOVE.B      D1,D2                 !Task number is zero: use current
XKTB1     BCLR        #7,evts+1(A5)         !
          MOVE.B      D2,D3                 !
          BEQ.S       XKTB3                 !
          EXT         D2                    !
          LSL         #4,D2                 !Compute index to task list
          LEA         (tlist,A5,D2.W),A3    !get pointer to entry in task list
          TST         (A3)                  !Check for active task
          BLE.S       XKTB3                 !Not an active task
          CMP.B       D1,D3                 !Delete current task?
          BEQ.S       XKTB2                 !Yes.
          CMP.B       (A3),D1               !Is task to delete child of current?
          BEQ.S       XKTB2                 !Yes.
          TST.B       D1                    !
          BNE.S       XKTB3                 !
XKTB2     XLKT                              !Lock task
          MOVE.L      #$FFFF,8(A3)          !Set wait flags
          MOVE.B      D0,(A3)               !
          MOVE.B      D3,D0                 !
          ST          D1                    !
          XSTP                              !Set priority
          XULT                              !Unlock task
          XSWP                              !Reschedule
          MOVEM.L     (SP)+,D0-A6
          MOVE.B      #4,1(SP)
          RTE
*
XKTB3     MOVEQ       #74,D0                !No such task
          XIT0
*
*  XGTM  - Get task message
*
* Call:  A1 - Buffer address
*
* Return:  D0.L  - Source task number
*                  -1 means no message available
*            SR  - EQ message found
*                  NE no message
*
XGTM:
    IFNE  DEBUG
          ADDQ.L      #1,XGTMCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVEM.L     D1-D3/A1-A3,-(SP)
          MOVE.L      SYRAM,A6
          MOVE.B      tskn(A6),D0          !Get destination task number
          BRA.S       XGTM1A
*
*  XKTM  - Kill task message
*
* Call:  A1   - Buffer address
*        D0.B - Task number
*
* Return:  D0.L  - Source task number
*                  -1 means no message available
*            SR  - EQ message found
*                  NE no message
*
XKTM:
    IFNE  DEBUG
          ADDQ.L      #1,XKTMCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVEM.L     D1-D3/A1-A3,-(SP)
          MOVE.L      SYRAM,A6
XGTM1A:
          BSR.S       TMCOM                 !
          TAS         tlck1(A6)             !Lock task
          SMI         D7                    !Remember previous lock status
XKTM1     CMP.B       (A2)+,D0              !Search for message from task D0
          BEQ.S       XKTM2                 !Found one.
          ADDQ        #3,A2                 !
          DBRA        D2,XKTM1              !Loop
          BSR         EXTSWP                !
          MOVEQ       #-1,D0                !Set return D0.L to -1
          MOVEM.L     (SP)+,D1-D3/A1-A3     !Means no message available
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #0,1(SP)              !Set return status SR
          RTE
*
XKTM2     MOVEQ       #0,D0
          MOVE.B      (A2)+,D0              !Return source task number
          ADDA        (A2),A3               !Compute pointer to message 
          SUBQ        #2,A2                 !
XKTM3     MOVE.B      (A3)+,(A1)+           !Move message to user buffer
          DBRA        D3,XKTM3              !Loop
          SUBQ        #1,D2                 !
          BMI.S       XKTM5                 !
          MOVE.L      (A2),D1               !
XKTM4     MOVE.L      4(A2),(A2)+           !Compact index list
          DBRA        D2,XKTM4              !
          MOVE.L      D1,(A2)               !
XKTM5     ST          (A2)                  !
          BSR         EXTSWP
          MOVEM.L     (SP)+,D1-D3/A1-A3
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #4,1(SP)              !Set return status SR
          RTE
*
TMCOM     LEA         tmtf(A6),A2           !Get pointer to to/from array
          LEA         tmbf(A6),A3           !Get pointer to message buffers
          MOVEQ       #$3F,D2               !Number of message buffers -1
          MOVEQ       #$3F,D3               !message length -1
          RTS                               !
*
*  XSTM  - Send task message
*
* Call:  D0.B  -  Destination task number
*        A1    - Buffer address
*
XSTM:
    IFNE  DEBUG
          ADDQ.L      #1,XSTMCNT
    ENDC
          ADDQ.L      #2,10(SP)             !Increment return PC
          MOVEM.L     D0-D3/A1-A3,-(SP)
          MOVE.L      SYRAM,A6
          MOVE.L      utcb(A6),A2
          MOVE.B      tskn(A6),D1           !Get task number
          EXT         D0                    !Test destination task number
          BPL.S       XSTM1                 !Positive - send message
          MOVE.B      D1,D0                 !Negative.
          TST.B       prt(A2)               !Does Caller have COM port assigned?
          BNE.S       XSTM1                 !Yes.
          MOVEQ       #0,D0                 !No. Send message to parent
          MOVE.B      D1,D0                 !
          MULU.W      #16,D0                !
          MOVE.B      (tlist,A6,D0.W),D0    !Get parent task number
XSTM1     BSR.S       XSTM2                 !Send message
          BEQ         XSTM1A                !
          XIT0
*
XSTM1A:
          MOVEM.L     (SP)+,D0-D3/A1-A3
          MOVEM.L     (SP)+,D7/A6
          MOVE.B      #4,1(SP)
          RTE
*
XSTM2     BSR         TMCOM                 !
          TAS         tlck1(A6)             !Lock task
          SMI         -(SP)                 !Remember previous lock status
XSTM3     BCLR        #7,(A2)+              !Search for empty message buffer
          BNE.S       XSTM4                 !Found one
          ADDQ        #3,A2                 !
          DBRA        D2,XSTM3              !
          MOVEQ       #78,D0                ! Message buffer full
          BRA.S       XSTM6                 !
*
XSTM4     ADDA        $1(A2),A3             !Compute pointer to this message
XSTM5     MOVE.B      (A1)+,(A3)+           !Move message text to buffer
          DBRA        D3,XSTM5              !
          MOVE.B      D1,(A2)               !Save source task number
          MOVE.B      D0,-(A2)              !Save destination task number
          MOVEQ       #0,D0                 !
XSTM6     MOVE.B      (SP)+,tlck1(A6)       !Restore previous lock status
          BSR         EXTSWP1               !
          TST.L       D0                    !
          RTS                               !
*
    IFNE  DEBUG
CALLA	DC.L	0
CODEA	DC.W	0
CALLCNT	DC.L	0
        DC.W    0
XSWPCNT DC.L	0
XSMPCNT	DC.L	0
XGMPCNT	DC.L	0
XUSPCNT	DC.L	0
XSEFCNT	DC.L	0
XTEFCNT	DC.L	0
XSUICNT	DC.L	0
XGTMCNT	DC.L	0
XSTMCNT	DC.L	0
XCTBCNT	DC.L	0
XKTMCNT	DC.L	0
XSUPCNT	DC.L	0
XLSRCNT	DC.L	0
XDEVCNT	DC.L	0
XRSRCNT	DC.L	0
XSEVCNT	DC.L	0
XKTBCNT	DC.L	0
XSOECNT	DC.L	0
XDPECNT	DC.L	0
	DC.L	0
	DC.L	0
    ENDC
          END
