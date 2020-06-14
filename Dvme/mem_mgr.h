/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/mem_mgr.h
*
*    Description:  Parameters for the memory management task in the 
*                  CPU-40/A.  Structures defined herein are also needed
*                  for host routines which drive the VME memory manager.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/22/92    MCSQ         Original
*
*   10/29/92    MCSQ         Added a parameter for the default memory
*                            allocation.
*
*    5/ 5/93    MCSQ         Add command to return interface device available
*                            status and Ethernet addresses to host.
*
*    2/15/95    MCSQ         Add a zero memory request for the memory
*                            allocate function call.  If the request flag
*                            is non-zero, the allocated block of memory
*                            is zet to zero.  Otherwise, memory is unaltered.
*
*    9/16/96    MCSQ         Change default memory allocation from 256 to
*                            384 Kbytes.  DEF_MEM_ALLOC  384
*
*    3/29/03    MCSQ         Reduce the size of struct Load by 2 bytes.
*                            The size of union Cmd will then be less than
*                            MAX_ORPH_DATA. (NOTE: the compiler makes the
*                            size of union Cmd multiple of 4).
*****************************************************************************/
#ifndef  MEM_MGR_H_
#define  MEM_MGR_H_

#ifndef  DEVICES_H_
#include  "devices.h"
#endif

/*
*   Loader information block.  This contains assigned memory limits
*   and data used in loading and starting a task in the VME processor
*/
struct Loader {
     int   low;        /* Low address limit                      */
     int   high;       /* High address limit                     */
     int   load;       /* Highest loaded address + 1             */
     int   start;      /* Start address                          */
     int   heap;       /* Heap size                              */
     int   stack;      /* Stack size                             */
     } ;

/*
*   Memory allocation control blocks.  Allocated memory may be just a
*   block of memory alloacted for any use or it may be a task under VMEPROM.
*   Each allocated block must have a name consisting of at least a nonzero
*   character (Currently there is no requirement for ASCII text names).
*   If the first character of the name is zero, the block is free.
*   If the task variable is nonzero, this block is a task under VMEPROM.
*   The memory limits are in the structure Loader.  Values in this structure
*   have meaning only if the block is allocated.
*/
struct MCB  {
     char Name[16];             /* Memory segement name (ASCII)           */
     int  Task;                 /* Task number if this block is a task    */
     struct Loader mem;         /* Memory allocation and task data        */
     };
/*
*  Number of memory allocation segements.  The curent maximum value is
*  33.  This is the maximum number that will fit in one packet as returned
*  by a DISPLAY_MEM command.  If more than 33 should be required code
*  WILL need modification.
*/
#define  MEM_SEGS  20

/*
*    Parameters for resident tasks in the CPU40
*/
#define  Task0_tcb   USERRAM
#define  Task0_size  8
#define  Lan_tcb     (Task0_tcb+Task0_size*1024)
#define  Lan_size    16
#define  Mem_tcb     (Lan_tcb+Lan_size*1024)
#define  Mem_size    76
/*
*    Default parameters for CPU40 task creation.
*/
#define  DEF_MEM_ALLOC  384
#define  DEF_TIME       1
#define  DEF_PRIORITY   63
#define  DEF_PORT       1
#define  DEF_HEAP       (5*2048)
#define  DEF_STACK      (1*2048)

/*
*    Function codes.  Codes for functions provided by the memory manager.
*/
#define  KILL_TASK     0x0101
#define  LOAD_MEM      0x0202
#define  START_TASK    0x0303
#define  ALLOC_MEM     0x0404
#define  FREE_MEM      0x0505
#define  DISPLAY_MEM   0x0606
#define  SET_TIME      0x0707
#define  DEVICES       0x0808

struct t_list {
     char  parent;
     char  slice;
     int   tcb;
     short dum;
     char  evt2;
     char  evt1;
     char  dum2[6];
    };

struct t_que {
     char  priority;
     char  task;
    };

struct task_dat {
     unsigned char  task;
     unsigned char  slice;
     unsigned char  priority;
     char  evt1;
     char  evt2;
     char  parent;
     short dum;
     int   start;
     int   end;
     int   pc;
     char  name[16];
    };

/*
*    The following are structures for the command/reply data for the
*    memory manager.  There is a unique struct for each command.  However,
*    all structures share a common command/reply format.  The first
*    short of each structure is the command code.  The second short
*    is in most cases an index to the MCB data structure.  On reply,
*    the status replaces these two fields.  The remainder is particular
*    to the command.
*/
struct Start {          /* Start task execution                           */
     short  func;       /* Function code START_TASK                       */
     short  mid;        /* Memory control block index                     */
     int    start;      /* Start address, if nonzero                      */
     int    heap;       /* Heap size in bytes                             */
     int    stack;      /* Stack size                                     */
     int    priority;   /* Task priority                                  */
     int    time_slice; /* Task time_slice in ticks (10 millisecond)      */
     int    port;       /* Port number  [1 , 4]                           */
     } ;

struct Kill {           /* Stop task and release memory                   */
     short  func;       /* Function code KILL_TASK                        */
     short  mid;        /* Memory control block index                     */
     };

struct Alloc {          /* Allocate a memory block                        */
     short  func;       /* Function code ALLOC_MEM                        */
     short  dum;        /* unused                                         */
     int    size;       /* size to allocate in 1 Kbyte blocks             */
     int    mid;        /* Returned memory control block index            */
     int    begin;      /* First byte address of this block - returned    */
     int    end;        /* End byte address + 1  - returned               */
     char   zerof;      /* 1 = zero allocated memory block                */
     char   name[16];   /* Name assigned to memory block                  */
     };

struct Free {           /* Release memory block                           */
     short  func;       /* Function code FREE_MEM                         */
     short  mid;        /* Memory control block index                     */
     int    begin;      /* ALternate first byte address                   */
     int    end;        /* end address                                    */
     };

struct Display {        /* Display memory allocation                      */
     short  func;       /* Function code DISPLAY_MEM                      */
     short  dum;        /* Not used                                       */
     int    num;        /* Number of memory segements  - returned         */
     struct task_dat tsk[MEM_SEGS];
     };

struct Load {           /* Load S record data                             */
     short  func;       /* Function code LOAD_MEM                         */
     short  mid;        /* Memory control block index                     */
     char   data[MAX_ORPH_DATA-4-2];    /* S record data                  */
     };

struct Time {           /* Set VME processor's clock time                 */
     short  func;       /* Function code SET_TIME                         */
     short  dum ;       /* Not used                                       */
     char   mon;        /* Month                                          */
     char   day;        /* Day of month                                   */
     char   yrs;        /* Year - 1900                                    */
     char   hrs;        /* Hours                                          */
     char   min;        /* Minutes                                        */
     char   sec;        /* Seconds                                        */
     char   dst;        /* Daylight savings time if nonzero               */
     char   tzone;      /* Hours west of the Greenwich Meridian           */
     };

struct Devices {        /* Return interface status and Ether addresses    */
     short  func;       /* Function code DEVICES                          */
     short  dum;        /* Not used                                       */
unsigned char enet_vme[6];  /* Ethernet address of VME processor          */
unsigned char enet_host[6]; /* Ethernet address of default host           */
unsigned char enet_broad[6]; /* Multicast address for boot requests       */
struct devices devtbl;  /* Device available flags                         */
     };

struct Reply {          /* Status reply                                   */
     int   status;      /* status code                                    */
     };
/*
*   The union below includes all command/reply structures
*/
union Cmd {
     struct Start start;
     struct Kill  kill;
     struct Alloc alloc;
     struct Free  free;
     struct Load  load;
     struct Display display;
     struct Time  time;
     struct Devices devices;
     struct Reply reply;
     };

/*
*    Status codes
*/
#define  OK           0         /* successful operation                   */
#define  SRADRERR     1         /* S record address out of range          */
#define  SRCHKSUM     2         /* S record checksum error                */
#define  SRILLHEX     3         /* S record illegal hex char              */
#define  SRCNTERR     4         /* S record byte count error              */
#define  ILLINDEX     5         /* Illegal memory control block index     */
#define  KILLERR      6         /* Request to kill nonexistant task       */
#define  TSKMEM       7         /* Insufficent task memory                */
#define  ALLOCERR     8         /* Memory control block table full        */
#define  ILLFUNC      9         /* Unrecognized function                  */
#define  PERMTSK     10         /* Permamently resident task              */
#define  NOMEMAVAIL  11         /* No memory available                    */
#define  MEMDUPC     12         /* Duplicate memory section name          */
#define  ILLFREE     13         /* Illegal free memory request            */

#endif            /* end MEM_MGR_H_      */
