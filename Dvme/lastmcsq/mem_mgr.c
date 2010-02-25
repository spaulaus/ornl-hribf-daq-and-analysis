/*****************************************************************************
*
*                            HHIRF COMPUTER GROUP
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1998
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
*    File:         /usr/users/mcsq/Dvme3/mem_mgr.c
*
*    Description:  Memory management task for the VME processor.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/10/91    MCSQ         Original
*
*    5/8/92     MCSQ         Set task priority just below the Ethernet
*                            driver so the host has a better chance of
*                            talking to the VME.  Also make all terminal
*                            output conditional so it can be disabled when
*                            debug info is not needed.
*
*    5/ 5/93    MCSQ         Add a function - DEVICES - to return the
*                            the Ethernet addresses used by the VME processor
*                            and status of hardware interfaces as tested
*                            at boot time.
*
*    2/12/95    MCSQ         In the function alloc, add code to zero memory
*                            allocated when start of memory block is above
*                            the Acq_Params table.
*
*    4/ 8/96    MCSQ         Set the real-time clock chip to the time
*                            of the workstation in function set_time().
*
*    3/ 4/97    MCSQ         For the CPU-60, the task Lan_Driver is changed
*                            to Lan_Driver60.
*
*    2/24/98    MCSQ         For the CPU-60, the memory used by the ethernet
*                            controller should be allocated so it cannot be
*                            used by other processes.  This is done in routine
*                            mgr_init().
*
*    4/17/98    MCSQ         Y2K patch for RTC chip.  If years since 1900 is
*                            greater than 99, load RTC chip with (years since
*                            1900) -100.  For example, in year 2001 we load
*                            the RTC chip with 01 for the two digit year.
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "vme_sys.h"
#include  "vmeprom.h"
#include  "syram.h"
#include  "tcb.h"
#include  "lan.h"
#include  "orph.h"
#include  "lan_lib.h"
#include  "mem_mgr.h"
#include  "devices.h"

/**********
***   If VMEBUG is defined certain diagnositic data is output to
***   terminal attached to port 1 of the VME processor.
#define VMEBUG
**********/

/*
*    Storage for the initial exception vector table
*/
char *init_vec[256];

/*
*    Memory control allocation table.
*/
struct MCB  mcb[MEM_SEGS];

/*
*   Function Prototypes
*/
void mgr_init(void);
int  kill_tsk(union Cmd *);
int  start_tsk(union Cmd *);
int  alloc(union Cmd *);
int  free_mem(union Cmd *);
int  exit_task(int task);
int  set_time(union Cmd *);
int  srloader(char *, int,struct Loader *);
static int  get_record(int ,char **,char *,struct Loader *);
static int  get_byte(char *,int *);
void dsp_mem(union Cmd *);
int  devices_task(union Cmd *);

/****************************************************************************
****************************************************************************/
main(void)
{
   struct Ether_Packet *out_hdr;
   union Cmd *cmd;
   int   cnt;
   char  *inbuf,*outbuf;

   mgr_init();

   lan_open((char)PROTO_CODE,&outbuf,&out_hdr);

   task_priority_(-1,1,69);

   while(1)
    {
      int  task,size;
      char *cptr;

      cnt = lan_read(0,&inbuf);         /* get a command packet from host  */

      if (receive_ptr_(EXIT_MSG_SLOT,&task,&cptr)) exit_task(task);

      byte_swap_(inbuf,4);
      size = sizeof(struct Reply);      /* size of most common reply message */
      cmd = (union Cmd *)inbuf;
      switch (cmd->start.func)          /* dispatch on function code       */
       {
         case  KILL_TASK:
          cmd->reply.status = kill_tsk(cmd);
          break;
         case  LOAD_MEM:
          if (cmd->load.mid < 0 || cmd->load.mid > MEM_SEGS) 
             {
               cmd->reply.status = -ILLINDEX;
               break;
             }
          cmd->reply.status = srloader(cmd->load.data,cnt - 4,
                                                      &mcb[cmd->load.mid].mem);
          if(cmd->reply.status != OK)printf("s record error = %x\n",
                                                            cmd->reply.status);
          break;
         case  START_TASK:
          cmd->reply.status = start_tsk(cmd);
          break;
         case  ALLOC_MEM:
          cmd->reply.status = alloc(cmd);
          size = sizeof(struct Alloc);
          break;
         case  FREE_MEM:
          cmd->reply.status = free_mem(cmd);
          break;
         case  DISPLAY_MEM:
          cmd->reply.status = OK;
          dsp_mem(cmd);
          size = sizeof(struct Display);
          break;
         case  SET_TIME:
          cmd->reply.status = set_time(cmd);
          break;
         case  DEVICES:
          cmd->reply.status = devices_task(cmd);
          size = sizeof(struct Devices);
          break;
         default:
          cmd->reply.status = -ILLFUNC;
          break;
       }
     word_swap_(&cmd->reply.status,2);          /* prepare reply packet    */
     byte_swap_(&cmd->reply.status,4);
     memcpy(outbuf,inbuf,size);
     lan_reply(size,(char)ACK);                 /* Send command response   */
    }
}
/****************************************************************************
*   Initialize the memory manager.  The purpose is to enter data for the
*   resident tasks int the memory manager's tables.  These tasks are:
*     1) Task 0 - This task is required by VMEPROM and is the only task
*                 which can do input from the console device attached to
*                 the VME processor.
*     2) Lan_Driver - Ethernet driver task.
*     3) Mem_Mgr  -  Memory manager task.
*
*  NOTE:  Memory requirements for these three tasks are fixed in this code.
*         ANYTHING which changes these allocations WILL require changes in
*         this code !!
*
*   On entry, the exception vector table is saved.  This is done so that
*   when a task dies any vectors it has used can be restored.
****************************************************************************/
void mgr_init(void)
{
   struct MCB *mem_ptr = &mcb[0];
   char **vectors;
   int  i,size;

/*
*   Free all memory above the base system
*/
   free_mem_((MAXMEM-USERRAM)/1024,(char *)USERRAM);
/*
*   Allocate space for Task 0 and enter data in memory manager tables
*/
   mem_ptr->Task = 0x80;
   strcpy(mem_ptr->Name,"Task0");
   mem_ptr->mem.low = (int)mem_alloc_low_(Task0_size);
   mem_ptr->mem.high = mem_ptr->mem.low + Task0_size * 1024;
   mem_ptr++;
/*
*   Ethernet driver task.
*/
   mem_ptr->Task = 0x81;

#ifndef  CPU60
   strcpy(mem_ptr->Name,"Lan_Driver");
#else
   strcpy(mem_ptr->Name,"Lan_Driver60");
#endif

   mem_ptr->mem.low = (int)mem_alloc_low_(Lan_size);
   mem_ptr->mem.high = mem_ptr->mem.low + Lan_size * 1024;
   mem_ptr++;
/*
*   Memory manager task
*/
   mem_ptr->Task = 0x82;
   strcpy(mem_ptr->Name,"Mem_mgr");
   mem_ptr->mem.low = (int)mem_alloc_low_(Mem_size);
   mem_ptr->mem.high = mem_ptr->mem.low + Mem_size * 1024;

#ifdef  CPU60
/*
*   For the CPU-60, we need to protect the memory used the the ethernet
*   controller.
*/
   size = (LAN_RAM - mem_ptr->mem.high) >> 10;
   size += 128;               /* need 128K bytes for ethernet */
   mem_alloc_low_(size);
   size -= 128;
   free_mem_(size,(char *)(mem_ptr->mem.high));
#endif

/*
*   Save the exception vector table.
*/
   vectors = (char **)0;
   for (i=0; i < 256; i++) 
     {
       init_vec[i] = *vectors;
       vectors++;
     }
}
/****************************************************************************
*  Kill a task and delete it's memory allocation.  Only nonresident tasks
*  can be killed.  Attempts to kill tasks 0, 1 or 2 will return the error
*  code PERMTSK.  NOTE that a task is referenced by it's memory allocation
*  index (mid).  This is NOT the same as the task number assigned by
*  VMEPROM when the task is created.
****************************************************************************/
int kill_tsk(union Cmd *cmd)
{
   struct LAN_CALL  lan_call;
   struct MCB *mem_ptr;
   char **vectors = (char **)8;
   int  i;

   if (cmd->kill.mid < 0 || cmd->kill.mid > MEM_SEGS) return (-ILLINDEX);
   if (cmd->kill.mid < 3) return (-PERMTSK);
/*
*  Use the memory allocation index to find the control block
*/
   mem_ptr = &mcb[cmd->kill.mid];
/*
*  If this is a task, the element Task will be nonzero.  If this is not
*  a task return an error code.  A memory block which is not a task can
*  be released by the command FREE_MEM.
*/
   if (mem_ptr->Task == 0) return (-KILLERR);
   for (i=2; i < 256; i++)
     {
       if (*vectors >= (char *)mem_ptr->mem.low
           && *vectors <= (char *)mem_ptr->mem.high) *vectors = init_vec[i];
       vectors++;
     }
   mem_ptr->mem.low = NULL;
   mem_ptr->mem.high = NULL;
   mem_ptr->mem.load = NULL;
   mem_ptr->mem.start = NULL;
   mem_ptr->mem.heap = 0;
   mem_ptr->mem.stack = 0;
/*
*   Close and release any connection to the Ethernet driver.
*/
   lan_call.eno = mem_ptr->Task - 0x80;
   lan_call.func = E_SYS_CLOSE;
   lan_call.flag = 0;
   while (!send_ptr_(LAN_MSG_SLOT,(char *)&lan_call))
                          wait_evts_((unsigned char)-LAN_MSG,(unsigned char)0);
   wait_phys_(0x87,(char *)&lan_call,NULL);
/*
*   Call VMEPROM to delete this task.
*/
   task_kill_((int)lan_call.eno);

#ifdef VMEBUG
   printf("\nKill task %s, Task No. %x\n",mem_ptr->Name,lan_call.eno);
#endif

   mem_ptr->Task = 0;
   mem_ptr->Name[0] = '\0';
   return OK;
}
/****************************************************************************
*   Start a task.  Prior to this memory must have been allocated by a 
*   ALLOC_MEM command.  Optionally code can be loaded by a LOAD_MEM command.
*   By default, the task will start in VMEPROM.  An alternate start address
*   can be supplied with this command or by an S record which specifies
*   the starting address.  By default the heap size is 10240 bytes and the
*   stack is 4096 bytes.  These parameters can be specified in this call.
****************************************************************************/
int start_tsk(union Cmd *cmd)
{
   struct SYRAM *sys = (struct SYRAM *)SYSRAM;
   struct MCB  *mem_ptr;
   struct TCB  *tcb_ptr;        /* pointer to task control block           */
   int  new_task;
   int  start_adr,high;
   char abuf[16];
   char *cptr;

   if (cmd->start.mid < 0 || cmd->start.mid > MEM_SEGS) return (-ILLINDEX);
   mem_ptr = &mcb[cmd->start.mid];
   byte_swap_(&cmd->start.start,24);
   word_swap_(&cmd->start.start,12);
   start_adr = cmd->start.start;
/*
*  Determine the starting address.  The start address can be supplied in this
*  call or by an S record.  If both of these are zero, the task will by
*  default start a the first address of the task.  VMEPROM puts an exit
*  to VMEPROM here.
*/
   if (start_adr == NULL && mem_ptr->mem.start == NULL)
                                      start_adr = mem_ptr->mem.low + CODE_OFF;
   else if (start_adr == NULL && mem_ptr->mem.start != NULL)
                                      start_adr = mem_ptr->mem.start;
/*
*  Determine the heap and stack sizes.
*/
   if (cmd->start.heap <= 0) cmd->start.heap = DEF_HEAP;
   if (cmd->start.stack < DEF_STACK) cmd->start.stack = DEF_STACK;
/*
*  Find the highest address required by the code, heap and stack.  If
*  this is greater than the high memory allocation, return an error code.
*/
   high = (((mem_ptr->mem.load + cmd->start.heap
                                + cmd->start.stack)/2048)*2048 + 2048);
   if (high > mem_ptr->mem.high) return (-TSKMEM);
   free_mem_((mem_ptr->mem.high - high)/1024,(char *)high);
/*
*  Set new memory allocation parameters in the memory control block.
*/
   mem_ptr->mem.high = high;
   mem_ptr->mem.heap = cmd->start.heap;
   mem_ptr->mem.stack = cmd->start.stack;
/*
*  Call VMEPROM to create the new task and then release any excess memory.
*/
   if (cmd->start.port < 1 || cmd->start.port > 4) cmd->start.port = DEF_PORT;
   new_task = spawn_task_(-1,1*256+63,cmd->start.port,
                          (char *)(mem_ptr->mem.low),
                          (char *)mem_ptr->mem.high,
                          (char *)0);
   mem_ptr->Task = new_task + 0x80;
/*
*   Must let new task run to setup it's TCB
*/
   task_swap_();
/*
*  Store heap start address and size in the new task's control block.
*  NOTE:  These are spare locations in the current version of VMEPROM.
*         BEWARE! (VMEPROM Version 2.74 9-Apr-91)
*/
   tcb_ptr = (struct TCB *)(mem_ptr->mem.low);
   tcb_ptr->_brk = (char *)mem_ptr->mem.load;
   tcb_ptr->_brk_size = cmd->start.heap;

#ifdef VMEBUG
   printf("\nStart %s",mem_ptr->Name);
   printf("  at Address %x ,",start_adr);
   printf("Task No. = %x   ",new_task);
   printf("High memory = %x\n",mem_ptr->mem.load);
#endif

/*
*   Prepare a start command for the new task and send it via
*   an intertask message to the task we just created.
*/
   sprintf(abuf,"go %lx",start_adr);
   if (cmd->start.port != DEF_PORT)
     {
       cptr = &sys->_port[(cmd->start.port - 1)*NCP];
       *cptr = (char)strlen(abuf) + 1;
       abuf[*cptr-1] = '\r';
       abuf[*cptr++] = '\0';
       *cptr++ = '\0';
       strcpy(cptr,abuf);
       set_evt_((unsigned char)(96 + cmd->start.port));
     }
   else  send_msg_(new_task,abuf);
/*
*   Let the new task startup before we change the time slice and
*   priority.
*/
   task_swap_();
   if(cmd->start.priority < 1 || cmd->start.priority > 127)
                                            cmd->start.priority = DEF_PRIORITY;
   if(cmd->start.time_slice < 1 || cmd->start.time_slice > 100)
                                              cmd->start.time_slice = DEF_TIME;
   task_priority_(new_task,cmd->start.time_slice,cmd->start.priority);
   return OK;
}
/****************************************************************************
*  Allocate a block of memory.  Return the start and end addresses to
*  the host.
****************************************************************************/
int alloc(union Cmd *cmd)
{
   struct MCB *mem_ptr, *mem_ptr_end;
   int  i,j;
   int  begin;

   byte_swap_(&cmd->alloc.size,4);
   word_swap_(&cmd->alloc.size,2);
/*
*   Check for duplicate memory section name
*/
   mem_ptr = &mcb[0];
   mem_ptr_end = &mcb[MEM_SEGS];
   while (mem_ptr < mem_ptr_end)
     {
       if (!strcmp(mem_ptr->Name,cmd->alloc.name)) return (-MEMDUPC);
       mem_ptr++;
     }
/*
*   Find a free slot in the memory control table.  If none is available,
*   return an error code.
*/
   mem_ptr = &mcb[3];
   for(i=3; i < MEM_SEGS; i++)
     {
       if (mem_ptr->Name[0] == '\0') break;
       mem_ptr++;
     }
   if (i >= MEM_SEGS) return(-ALLOCERR);
   cmd->alloc.mid = i;
/*
*   Try to allocate the requested memory.  If none available, return
*   an error code.
*/
   begin = (int)mem_alloc_low_(cmd->alloc.size);
   if (begin == 0xffffffff) return (-NOMEMAVAIL);
/*
*   Return start and end address of the block and setup the new entry
*   in the memory control block.
*/
   cmd->alloc.begin = begin;
   cmd->alloc.end = cmd->alloc.begin + cmd->alloc.size * 1024;
   mem_ptr->mem.low = cmd->alloc.begin;
   mem_ptr->mem.high = cmd->alloc.end;
   mem_ptr->mem.heap = 0;
   mem_ptr->mem.stack = 0;
   mem_ptr->mem.start = NULL;
/*
*   Put the name of the memory block in the memory control table.
*/
   cmd->alloc.name[sizeof(cmd->alloc.name)-1] = '\0';
   j = (sizeof(cmd->alloc.name) > sizeof(mem_ptr->Name)) ?
                             sizeof(mem_ptr->Name) : sizeof(cmd->alloc.name);
   strncpy(mem_ptr->Name,cmd->alloc.name,j);

#ifdef VMEBUG
   printf("\nAllocate memory for %s, Memory ID = %x, %d Kbytes at %x\n",
         mem_ptr->Name,i,cmd->alloc.size,mem_ptr->mem.low);
#endif
/*
*   Zero all of the allocated memory block if requested.
*/
   if (cmd->alloc.zerof)
     {
        memset((void *)mem_ptr->mem.low,0,
                   (size_t)(mem_ptr->mem.high-mem_ptr->mem.low+1));
#ifdef VMEBUG
        printf("Zero %i bytes of memory starting at address %x (hex)\n",
                       mem_ptr->mem.high-mem_ptr->mem.low+1,mem_ptr->mem.low);
#endif
     }

   word_swap_(&cmd->alloc.size,8);
   byte_swap_(&cmd->alloc.size,16);
   return OK;
}
/****************************************************************************
*   Release a block of memory.  Call with an index to the memory control
*   block.  Note that memory assigned to permanent tasks CAN NOT be released.
****************************************************************************/
int free_mem(union Cmd *cmd)
{
   int  size;
   struct MCB *mem_ptr;

#ifdef VMEBUG
   printf("free_mem - memory ID = %x\n",cmd->free.mid);
#endif

   if (cmd->free.mid == -1)
     {
       byte_swap_(&cmd->free.begin,8);
       word_swap_(&cmd->free.end,4);
       size = cmd->free.end - cmd->free.begin;
       if(size <= 0 || size > MAXMEM) return(-ILLFREE);

#ifdef VMEBUG
       printf("free_mem - free block of %i Kbytes at address %x\n",size,
                                                             cmd->free.begin);
#endif

       free_mem_(size/1024,(char *)cmd->free.begin);
       return(OK);
     }
   if (cmd->free.mid < 0 || cmd->free.mid > MEM_SEGS) return (-ILLINDEX);
   if (cmd->free.mid < 3) return (-PERMTSK);
   mem_ptr = &mcb[cmd->free.mid];
   if (mem_ptr->Name[0] == '\0') return(-KILLERR);
   size = (mem_ptr->mem.high - mem_ptr->mem.low)/1024;
   if (size == 0)  return OK;
   free_mem_(size,(char *)mem_ptr->mem.low);
   mem_ptr->Name[0] = '\0';
   mem_ptr->Task = 0;
   mem_ptr->mem.low = NULL;
   mem_ptr->mem.high = NULL;
   mem_ptr->mem.load = NULL;
   mem_ptr->mem.start = NULL;
   mem_ptr->mem.heap = 0;
   mem_ptr->mem.stack = 0;
   return  OK;
}
/****************************************************************************
*   Release memory for a task which called exit.
****************************************************************************/
int exit_task(int task)
{
  int  i;
  struct Free xfree;
  struct MCB *mem_ptr = &mcb[3];

  task += 0x80;
  for(i=3; i < MEM_SEGS; i++)
    {
      if (mem_ptr->Task == task)
        {
          xfree.mid = i;

#ifdef VMEBUG
          printf("exit_task - delete task no. %x, MID = %x\n",task,xfree.mid);
#endif

          free_mem((union Cmd *)&xfree);
          return(OK);
        }
      mem_ptr++;
    }

#ifdef VMEBUG
   printf("exit_task - task not found! - Task No. = %x\n",task);
#endif

  return(-1);
}
/****************************************************************************
*   Set clock time in the VME processor.
****************************************************************************/
int set_time(union Cmd *cmd)
{
   int i;
   struct SYRAM *syram = (struct SYRAM *)SYSRAM;
   struct rtc7242 {
         unsigned char sec1reg;
         unsigned char sec10reg;
         unsigned char min1reg;
         unsigned char min10reg;
         unsigned char hou1reg;
         unsigned char hou10reg;
         unsigned char day1reg;
         unsigned char day10reg;
         unsigned char mon1reg;
         unsigned char mon10reg;
         unsigned char yr1reg;
         unsigned char yr10reg;
         unsigned char weekdayreg;
volatile unsigned char dcontrol;
volatile unsigned char econtrol;
volatile unsigned char fcontrol;
}  *rtc;

   syram->_smon = (unsigned char)cmd->time.mon;
   syram->_sday = (unsigned char)cmd->time.day;
   syram->_syrs[0] = (unsigned char)cmd->time.yrs;
   syram->_syrs[1] = (unsigned char)cmd->time.dst;
   syram->_shrs = (unsigned char)cmd->time.hrs;
   syram->_smin = (unsigned char)cmd->time.min;
   syram->_ssec[0] = (unsigned char)cmd->time.sec;
   syram->_ssec[1] = (unsigned char)cmd->time.tzone;

/*
*   Set workstation time in the RTC chip
*/
   rtc = (struct rtc7242 *)0xFF803000;
   rtc->dcontrol = 1;
   i = 10;
   while (i)
     {
       if (!(rtc->dcontrol & 2)) break;
       delay_(1);
       i--;
     }
   if (i)
     {
       rtc->fcontrol = 5;
       rtc->fcontrol = 4;
       rtc->sec10reg = syram->_ssec[0]/10;
       rtc->sec1reg = syram->_ssec[0]%10;
       rtc->min10reg = syram->_smin/10;
       rtc->min1reg = syram->_smin%10;
       rtc->hou10reg = syram->_shrs/10;
       rtc->hou1reg = syram->_shrs%10;
       rtc->day10reg = syram->_sday/10;
       rtc->day1reg = syram->_sday%10;
       rtc->mon10reg = syram->_smon/10;
       rtc->mon1reg = syram->_smon%10;
       rtc->yr10reg = (syram->_syrs[0]/10)%10;
       rtc->yr1reg = syram->_syrs[0]%10;
     }
   rtc->dcontrol = 0;
#ifdef VMEBUG
   printf("\nVME processor clock time has been set\n");
#endif

   return(OK);
}
/****************************************************************************
*    S Record loader
*  Call:   buf  - Pointer to buffer containing S records
*          size - Buffer size in bytes
*          ldr  - Pointer the struct Loader which contains address limits
*                 for valid S records and returns the start address if any.
*
*  Return:  0   -  buffer OK
*       nonzero - error code from get_byte or get_record
*
*        load->load = highest loaded address + 1
*        load->start = start address from S7, S8 or S9 record.
****************************************************************************/
int srloader(char *buf, int size,struct Loader *ldr)
{
  char     *buf_end = buf+size;
  char     ch;
  int      err,adrlen;
  int      addr;

  while(buf < buf_end)
   {
     ch = *buf++;
     if (ch == 'S' || ch == 's')
       {
         addr = ldr->load;
         adrlen = 2;
         ch = *buf++;
         switch (ch - '0')
           {
             case     3:        /* S3 records - 32 bit address             */
               adrlen++;        
             case     2:        /* S2 records - 24 bit address             */
               adrlen++;
             case     1:        /* S1 records - 16 bit address             */
               if (err = get_record(adrlen,&buf,buf_end,ldr)) return (err);
               ldr->load = (ldr->load > addr) ? ldr->load : addr;
               break;
             case     7:        /* S7 record - 32 bit start address        */
               adrlen++;
             case     8:        /* S8 record - 24 bit start address        */
               adrlen++;
             case     9:        /* S9 record - 16 bit start address        */
               err = get_record(adrlen,&buf,buf_end,ldr);
               if (err == -SRADRERR) ldr->load = 0;
               else if (err != OK) return(err);
               ldr->start = ldr->load;
               ldr->load = addr;

#ifdef VMEBUG
               printf("Start address = %x\n",ldr->start);
#endif

               return (OK);
             case     0:
             case     4:
             case     5:        /* S0, S4 or S5 records - ignore then      */
             default:           
               while (*buf++ != '\n' && buf < buf_end);
               break;
           }       /* end switch */
       }
    }              /* end while  */
   return (OK);
}
/****************************************************************************
*    Process one S Record
*  Call:   adrlen  - number of address bytes
*          buf     - Pointer to buffer containing S records
*          buf_end - Buffer size in bytes
*          ldr     - Pointer the struct Loader which contains address limits
*                    for valid S records and returns the start address if any.
*
*  Return:  OK        - no error
*           SRILLHEX  - Non Hex char - returned from get_byte routine
*           SRCHKSUM  - Checksum error
*           SRCNTERR  - Illegal byte count
*           SRADRERR  - Address limits error
****************************************************************************/
static int get_record(int adrlen,char **cptr,char *buf_end,struct Loader *ldr)
{
  char *buf = *cptr;
  char *buf1;
  char *store;
  int  addr = 0,cnt,value;
  int  checksum,err,i;

/*
*   Get record byte count.  This is a count of data bytes following the
*   byte count including the address and checksum.
*/
  err = get_byte(buf,&cnt);
  if (err != OK) return (err);                  /* Hex conversion error    */
  if (buf + cnt * 2 >= buf_end) return (-SRCNTERR);  /* Invalid byte count */
  buf += 2;
  checksum = cnt;
  buf1 = buf;
/*
*   Check record for validity.
*/
  for (i = 0; i < cnt; i++, buf1 += 2)
    {
      if ((err = get_byte(buf1,&value)) != OK) return(err);
      checksum += value;
    }
  checksum++;
  if (checksum & 0xff) return(-SRCHKSUM);      /* Checksum error          */
/*
*   Get record start address.  adrlen is the number of address bytes.
*/
  cnt -= adrlen + 1;
  while (adrlen > 0)
    {
      addr <<= 8;
      get_byte(buf,&value);
      buf += 2;
      addr += value;
      adrlen--;
    }
/*
*   Verify that data is within the address limits specified in struct Loader.
*/
  if (addr < (ldr->low + CODE_OFF + 2) 
                        || addr + cnt > ldr->high) return (-SRADRERR);
/*
*   Load the data record to memory
*/
  store = (char *)addr;
  while (cnt > 0)
    {
      get_byte(buf,&value);
      buf += 2;
      *store++ = value;
      cnt--;
    }
  buf += 2;
  *cptr = buf;
  ldr->load = (int)store;
  return (OK);
}
/****************************************************************************
*    ASCII Hex to binary conversion - one byte
*  Call:   buf     - Pointer to buffer containing S records
*          value  - returned value(byte)
*
*  Return: OK       - no error
*          SRILLHEX - Non Hex character found
****************************************************************************/
static int get_byte(char *buf,int *value)
{
  int   val,val1;

  val = *buf++ - '0';
  if (val > 9) val -= 7;
  if (val > 15) val -= 32;
  if (val < 0 || val > 15) return (-SRILLHEX);
  val1 = *buf++ - '0';
  if (val1 > 9) val1 -= 7;
  if (val1 > 15) val1 -= 32;
  if (val1 < 0 || val1 > 15) return (-SRILLHEX);
  *value = (val << 4) + val1;
  return (OK);
}
/****************************************************************************
*
*   DISPLAY_MEM  command


****************************************************************************/
void  dsp_mem(union Cmd *cmd)
{
  struct SYRAM *syram = (struct SYRAM *)SYSRAM;
  struct MCB *mcb_ptr;
  struct TCB   *tcb_ptr;
  struct t_list *tl_ptr;
  struct t_que  *tq_ptr;
  struct task_dat *tsk_ptr;

  int  i,j,task;

  cmd->display.num = MEM_SEGS;
  tsk_ptr = &cmd->display.tsk[0];

  for (i=0; i < MEM_SEGS; i++)
    {
      mcb_ptr = &mcb[i];
      tsk_ptr->start = mcb_ptr->mem.low;
      tsk_ptr->end = mcb_ptr->mem.high;
      strncpy(tsk_ptr->name,mcb_ptr->Name,16);
      tsk_ptr->task = mcb_ptr->Task;
      task = mcb_ptr->Task - 0x80;
      if (mcb_ptr->Task < 0x80)
       {
         tsk_ptr->slice = 0;
         tsk_ptr->priority = 0;
         tsk_ptr->evt1 = 0;
         tsk_ptr->evt2 = 0;
         tsk_ptr->parent = 0;
         tsk_ptr->pc = NULL;
       }
      else
       {
         tl_ptr = (struct t_list *)syram->_tlst + task;
         tsk_ptr->slice = tl_ptr->slice;
         tsk_ptr->evt1 = tl_ptr->evt1;
         tsk_ptr->evt2 = tl_ptr->evt2;
         tsk_ptr->parent = tl_ptr->parent;
         tcb_ptr = (struct TCB *)tl_ptr->tcb;
         tsk_ptr->pc = (int)*(char **)(tcb_ptr->_tsp + 0x42);
         tq_ptr = (struct t_que *)syram->_tque;
         for (j=0; j < NT; j++)
          {
            if (task == tq_ptr->task)
              {
                tsk_ptr->priority = tq_ptr->priority;
                break;
              }
            tq_ptr++;
          }
       }
      byte_swap_(&tsk_ptr->start,12);
      word_swap_(&tsk_ptr->start,6);
      tsk_ptr++;
    }
}
/****************************************************************************
*
*  DEVICES - Send Ethernet addresses and interface device status to host.
****************************************************************************/
int  devices_task(union Cmd *cmd)
{
   struct devices *devtbl = DEVTBL;
   unsigned char *ucptr;

   ucptr = *our_ether_address;
   memcpy(cmd->devices.enet_vme,ucptr,6);
   ucptr  = *host_ether_address;
   memcpy(cmd->devices.enet_host,ucptr,6);
   ucptr += 6;
   memcpy(cmd->devices.enet_broad,ucptr,6);
   cmd->devices.devtbl = *devtbl;
   return OK;
}
