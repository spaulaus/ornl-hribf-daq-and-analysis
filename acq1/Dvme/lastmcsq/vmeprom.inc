*******************************************************************************
*   MACROs for functions provided by Force Computers VMEPROM
*
*   VMEPROM Version 2.74  09-Apr-91
*
*   Ref: VMEPROM Version 2/32, Rev. 1, User's Manual
*        Chapter 4.
*******************************************************************************
*   Set event flag after time delay.  Time delay is in clock ticks.  For the
*   CPU-40 a tick is 10 milliseconds.
*
*   /1  = Event number. 1 thru 127
*   /2  = Time delay in ticks
*
*   The event flag is first cleared and after the specified delay is set.
*   If delay is zero, the event flag is cleared.  If a delayed set is
*   pending, the delay is replaced with the new value.
*
DLY_SET_EVT     MACRO
		MOVE.B  #\1,D1
		MOVE.L  #\2,D0
		DC.W    $A032           ;XDEV  page 1-24
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*   Clear event flag after time delay.  Time delay is in clock ticks.  
*   For the CPU-40 a tick is 10 milliseconds.
*
*   /1  = Event number. 1 thru 127
*   /2  = Time delay in ticks
*
*   The event flag is first set and after the specified delay is cleared.
*   If delay is zero, the event flag is cleared.  If a delayed set is
*   pending, the delay is replaced with the new value.
*
DLY_CLR_EVT     MACRO
		MOVE.B  #-\1,D1
		MOVE.L  #\2,D0
		DC.W    $A032           ;XDEV  page 1-24
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Suspend process until either \1 or \2 events occur.
*
*   \1  =  event number.  If positive, wait till set.  If negative wait
*          set.
*   \2  =  Second event number.
*
WAIT_EVTS       MACRO
		MOVE.W  #(\1*256+\2),D1
		DC.W    $A01C           ;XSUI  page 1-106
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Time delay.  Select special local event #128 to be set after delay
*  and suspend until event flag is set.
*
*  /1  = time delay in ticks.  For CPU-40, a tick is 10 milliseconds.
*
DELAY           MACRO
		MOVE.B  #128,D1         ;Local event flag
		MOVE.L  #\1,D0
		DC.W    $A032           ;XDEV
		MOVE.W  #128,D1
		DC.W    $A01C           ;XSUI  page 1-106
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*   Set event flag.
*
*  /1  =  Event number.  1 thru 127
*
*  Returns:  SR .eq. if event was cleared before call
*            SR .ne. if event was already set
*
SET_EVT         MACRO
		MOVE.B  #\1,D1
		DC.W    $A046           ;XSEV  page 1-99
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*   Clear event flag.
*
*  /1  =  Event number.  1 thru 128
*
*  Returns:  SR .eq. if event was already cleared
*            SR .ne. if event was set before call
*
CLR_EVT         MACRO
		MOVE.B  #-\1,D1
		DC.W    $A046           ;XSEV  page 1-99
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*   Test event flag.
*
*  /1  =  Event number.  1 thru 127 or 128
*
*   On return, status is NE if flag set or EQ is flag cleared.
*
*   Returns:   SR  .eq. Event flag clear
*              SR  .ne. Event flag set
*
TEST_EVT        MACRO
		MOVE.B  #\1,D1
		DC.W    $A01A           ;XTEF  page 1-111
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Output to console the character in D0
*
OUT_CHR         MACRO
		DC.W    $A0BA           ;XPCR  page 1-66
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Output to console.
*
*     D7 -  Character count
*     A1 -  Address of string
*
OUT_MSG         MACRO
		DC.W    $A096           ;XPDC  page 1-67
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Convert binary to ASCII hex in buffer
*
*     D1.L -  Number to be converted
*     A1   -  Address of output string
*
ASCII_HEX       MACRO
		DC.W    $A068           ;XCHX  page 1-18
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Get character from input buffer.  Returns character in D0
*
IN_CHR          MACRO
		DC.W    $A09E           ;XGCP  page 1-41
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Exit to VMEPROM
*
EXIT            MACRO
		DC.W    $A00E           ;XEXT  page 1-33
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Lock task
*
LOCK            MACRO
		DC.W    $A014           ;XLKT  page 1-58
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Unlock task
*
UNLOCK          MACRO
		DC.W    $A016           ;XULT  page 1-117
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Swap to next task
*
TASK_SWAP       MACRO
		DC.W    $A000           ;XSWP  page 1-108
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Get task status
*
*  Call:   D0.W  -  Task number.
*
TASK_STATUS     MACRO
		DC.W    $A012           ;XRTS  page 1-96
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Kill task.
*
*  Call:  D0.B  -  Task number.  Zero = current task.  Negative means
*                  return memory to free list.
*
TASK_KILL       MACRO
		DC.W    $A0FA           ;XKTB  page 1-52
		ENDM
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Get process memory limits.
*
*  Return:  A0 - End of TCB (i.e. start task memory + $1000)
*           A1 - End of user space(i.e. end of user memory - $100)
*           A5 - System RAM start address
*           A6 - TCB start address
*
MEM_LIM         MACRO
		DC.W    $A010           ;XGML  page 1-46
		ENDM
*
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
*  Create new task.
*
*  Call:    D0.W  -  Task size (1K byte increments).
*           D1.W  -  Time slice/priority
*           D2.W  -  I/O port
*           A0    -  Low memory pointer if D0.W is zero.
*           A1    -  High memory pointer if D0.W is zero.
*           A2    -  Start address if D0.W is zero.  Otherwise, command
*                    line pointer.  If zero, start VMEPROM.
*
*  Return:  D0.L  -  New task number
*
SPAWN_TASK      MACRO
		DC.W    $A026           ;XCTB  page 1-22
		ENDM
