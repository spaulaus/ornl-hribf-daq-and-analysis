/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    File:         /usr/users/mcsq/Dvme3/lan_str.c
*
*    Description:  This process starts the basic system which must be
*                  downloaded by the host after power-up or a forced
*                  reboot.  Both the Ethernet driver and the memory
*                  manager are started.  Once these two tasks are
*                  running in the CPU40,  memory manager handles
*                  the loading and running of other tasks.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/22/92    MCSQ         
*
*    4/10/93    MCSQ        Disable hardware handshake on serial ports
*                           2,3 and 4.  NOTE:  The boot loader disables
*                           handshake on port 1.
*
*    3/ 2/97    MCSQ        Fix for use with CPU-60.  NOTE: still need to
*                           find a way to disable RTS/CTS handshake for
*                           CPU-60 serial ports.
*
*    3/ 7/97    MCSQ        Both CPU-60 serial ports have CTS handshake
*                           disabled by the boot loader - vme_voot60.s
*****************************************************************************/
#include  "vme_sys.h"
#include  "vmeprom.h"
#include  "tcb.h"
#include  "orph.h"
#include  "mem_mgr.h"

void main()
{
   char *low_mem;
   char *high_mem;
   char *syram;
   struct TCB *tcb;
   char *loaded;
   unsigned char *ucptr;
   int  task1,task2;

   char *build_cmd(unsigned int addr);

/*
*    Get memory limits for this task (i.e. Task 0).  The only useful
*    variable returned is high_mem which tells us where the user stack
*    is.
*/
   mem_lim_(&low_mem,&high_mem,&loaded,&syram,(char **)&tcb);
   loaded = (char *)(Task0_tcb+Task0_size*1024);
   tcb->_eum = loaded;
   high_mem += 0x100;
/*
*    Move the stack down to the top of our reallocation for this
*    task and free the excess memory.
*/
   move_stack_(loaded,high_mem);
   free_mem_((int)((high_mem-loaded)/1024),loaded);
/*
*    Allocate space for the Ethernet driver and the memory manager
*    tasks.
*/
   mem_alloc_low_(Lan_size);
   mem_alloc_low_(Mem_size);
/*
*    Create the Ethernet driver task.
*/
   task1 = spawn_task_(-1,1*256+63,1,(char *)Lan_tcb,
                                     (char *)(Lan_tcb+Lan_size*1024),
                                     (char *)0);
   task_swap_();
/*
*    Create the memory manger task.
*/
   task2 = spawn_task_(-1,1*256+63,1,(char *)Mem_tcb,
                                     (char *)(Mem_tcb+Mem_size*1024),
                                     (char *)0);
   task_swap_();

#ifndef  CPU60        /* use following for CPU-40                          */
/*
*   Disable hardware handshake on CPU-40 serial ports 2,3 and 4.
*/
   ucptr = (unsigned char *)0xff802024;    /* port 2 */
   *ucptr = *ucptr & 0x73;
   ucptr = (unsigned char *)0xff802204;    /* port 3 */
   *ucptr = *ucptr & 0x73;
   ucptr = (unsigned char *)0xff802224;    /* port 4 */
   *ucptr = *ucptr & 0x73;

#endif
/*
*    Now start both new tasks and exit to VMEPROM in task 0.
*/

   send_msg_(task1,build_cmd(Lan_tcb + CODE_OFF +2));
   task_swap_();
   send_msg_(task2,build_cmd(Mem_tcb + CODE_OFF +2));
   task_exit_();
}
/****************************************************************************
*   Build to start command for a process we created.
*
*   The string is "go XXXXXXXX" where XXXXXXXX is the start address.
*
*   NOTE: Regular C library routines could be used but I didn't want
*         to include the C library in this process.
******************************************************************************/
char *build_cmd(unsigned int addr)
{
  static char cmd_str[16];

  char adr_str[9];
  char *cptr = adr_str;
  int  i;

  for (i=0; i < sizeof(adr_str) -2; i++, cptr++) *cptr = ' ';
  *cptr-- = '\0';
  while (addr != 0)
   {
     *cptr = addr % 16;
     *cptr += '0';
     if (*cptr > '9') *cptr += 7;
     addr >>= 4;
     cptr--;
   }
  strcpy(cmd_str,"go ");
  strcat(cmd_str,++cptr);
  return (cmd_str);
}
