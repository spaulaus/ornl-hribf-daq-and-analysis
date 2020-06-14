/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-2000
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
*    File:         /usr/users/mcsq/Dvme3/vmebug.c
*
*    Description:  Debug routine for the VME acquisition system.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 5/92    MCSQ         
*
*    1/ 1/93    MCSQ      Added handling for Access Faults.  This needs an
*                         external module: bus_fault.s.  The routine bus_error
*                         is called from bus_fault to do output of fault
*                         address.
*
*    1/ 7/93    MCSQ      Added CES 8170 debug routines.
*
*    7/10/93    MCSQ      Added ORNL trigger module routines.
*
*    5/10/94    MCSQ      Has stack overflow problems in memory modify due
*                         to changing user to supervisor modes.  Change
*                         to set supervisor mode at the start of the code
*                         in main.  Hence, the entire code runs in supervisor
*                         mode.
*
*    7/22/94    MCSQ      Add LeCroy 1190 Dual Port Memory module.
*
*    2/19/96    MCSQ      Added routines to disassemble PAC codes.
*                         Routines are pac_dmp_XXXX.
*
*    9/16/96    MCSQ      List delay time for conditional module readout.
*
*    9/23/96    MCSQ      Include conditional CAMAC optimizer routine.
*
*    9/18/98    MCSQ      Add new FERA module types
*
*    2/18/99    MCSQ      Handle multiple LRS-1190 modules in routine
*                         fera_dmp.
*
*    2/28/00    MCSQ      Add CAMAC module type XIA_TIME and XIA module
*                         readout list.
*****************************************************************************/
#include "Acq_Params.h"
#include "vme_sys.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan_lib.h"
#include "lrs.h"
#include "ksc.h"
#include "acromag.h"
#include "trigger.h"
#include "syram.h"
#include "tcb.h"
#include "mem_mgr.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OPT             /* Optimize conditional CAMAC readout  */

#define  DEF_LC   21    /* Default line count for output  */
/*
*    Function Prototypes
*/
void  mainloop(void);
void  vmebug_help(void);
void  hex_display(int ,int);
void  memory_modify(int ,int);
void  disassemble(int, int);
int   get_cmd(char *);
int   get_next_cmd(char *);
char *gfield(char *,char *,int);
void  lrs_dmp(void);
unsigned short lrs_get_dat(char, unsigned long);
void  lrs_help(void);
void  ksc_dmp(void);
unsigned short ksc_get_dat(unsigned long);
void  ksc_help(void);
void  ces_dmp(void);
void  ces_help(void);
void  fera_dmp(void);
void  fera_help(void);
void  acro_dmp(void);
void  trig_dmp(void);
void  pac_dmp(void);
void  pac_dmp_startvme(void);
void  pac_dmp_crates(void);
void  pac_dmp_init(void);
void  pac_dmp_uncond(void);
void  pac_dmp_windup(void);
void  pac_dmp_latch(void);
void  pac_dmp_cammod(void);
void  pac_dmp_xiamod(void);
void  pac_dmp_feramod(void);
void  pac_dmp_fastmod(void);
void  pac_dmp_cond(void);
void  pac_dmp_fast_cond(void);
void  pac_dmp_fera_cond(void);
void  pac_dmp_kill(void);
void  pac_dmp_camac_cond(void);
void  pac_dmp_gates(void);

#ifdef OPT
int   cam_cond_opt(int *);
int   prior_ref(int);
#endif

extern int  lan_printf(char *, ...);
extern unsigned char *vmedis(int,unsigned char *,unsigned char *,int,int);
extern void bus_fault_init_(void);

/*
*     Global variables
*/
int   tmo;
int   line_count = DEF_LC;
unsigned long larg1,larg2;
int   which_1190;
char command[10],sarg1[10],sarg2[10];
char *cmd[] = {"dm", "di", "de", "mm", "fm", "dr", "as", "he", "lc", "pa",NULL};
enum func     {DM = 0,DI,DE,MM,FM,DR,AS,HE,LC,PA} ;
char *devices[] = {"lrs","ksc","ces","acro","trig","fera1","fera2","fera3",
                    "fera4",NULL};
struct SYRAM *syram = (struct SYRAM *)SYSRAM;
struct TCB  *tcb_ptr;
struct cnafdat_list *tmp_uncond;
int uncond_cnt;
/*
*   Register storage on task stack
*/
struct TRegs {
               unsigned long usp;
               unsigned long d0;
               unsigned long d1;
               unsigned long d2;
               unsigned long d3;
               unsigned long d4;
               unsigned long d5;
               unsigned long d6;
               unsigned long d7;
               unsigned long a0;
               unsigned long a1;
               unsigned long a2;
               unsigned long a3;
               unsigned long a4;
               unsigned long a5;
               unsigned long a6;
               unsigned short sr;
               unsigned long pc;
  };
struct TRegs *reg_ptr;
struct t_list *task_ptr;
char *cam_modules[] = {"unknown","generic","PHIL_7164","PHIL_7186","LRS_2277",
                       "SILENA_4418C","MCSQ_CAM","LRS_4300C","LRS_3377C",
                       "XIA_TIME"};
char *fera_modules[] = {"unknown","LRS_4300","GAN_812F","SILENA_4418",
                        "MCSQ_FER","LRS_3377","BAKLASH","BAKLASH2","BAKLASH3"};
char *fast_modules[] = {"unknown","LRS_1885","PHIL_10C6","LRS_1875",
                        "LRS_1881M","LRS_1877","MCSQ_FAS"};

#ifdef OPT
struct cnaf_list CNAFnew[2000];
             int IDnew[2000];
struct cond_cam_pgm Cpgm[500];
#endif

/*****************************************************************************
*  Dummy main routine.  All it does is change to supervisor mode and then
*  call the rela main - mainloop.
*****************************************************************************/
void main()
{
  super_mode_();
  mainloop();
}
/*****************************************************************************
*****************************************************************************/
void mainloop(void)
{
  int  i,j;
  int  args, size, type;
  char  str[4];
  struct Ether_Packet *out_hdr;
  char  *outbuf;

  lan_open(PROTO_SOFT,&outbuf,&out_hdr);

  task_priority_(-1,1,69);
  bus_fault_init_();

  while(1)
   {
/*
*   Get command from host
*/
     if((args = get_cmd("VMEbug>  \b")) < 0) continue;
     gfield(str,command,3);
     i = 0;
/*
  lan_printf("USP = %x, SSP = %x, MSP = %x, ISP = %x\n",usp,ssp,msp,isp);
*   Search for valid command
*/
     while (cmd[i] != NULL)
      {
        if(!strcmp(str,cmd[i])) break;
        ++i;
      }
/*
*   Dispatch command from host
*/
     switch  ((enum func)i)
      {
        case  DM:        /* Display memory - hex and ascii  */
          size = 2;
          if (command[3] == 'b') size = 1;
          else if (command[3] == 'l') size = 4;
          hex_display(size,args);
          break;
        case  DI:        /* Disassemble                     */
          type = 3;
          if (command[3] == 'b') type = 0;
          else if (command[3] == 'w') type = 1;
          else if (command[3] == 'l') type = 2;
          disassemble(type,args);
          break;
        case  DE:        /* Select a VME device interface   */
          j = 0;
          while (devices[j] != NULL)
           {
             if(!strcmp(sarg1,devices[j])) break;
             ++j;
            }
          switch (j)
           {
             case  0:
               lrs_dmp();    /* LeCroy 1131 VME Interface to SIB      */
               break;
             case  1:
               ksc_dmp();    /* Kinetics Systems 2917 CAMAC Interface */
               break;
             case  2:
               ces_dmp();    /* CES 8170 High Speed Memory            */
               break;
             case  3:
               acro_dmp();
               break;
             case  4:
               trig_dmp();
               break;
             case  5:
               which_1190 = 1;
               fera_dmp();  /* LRS 1190 dual port memory             */
               break;
             case  6:
               which_1190 = 2;
               fera_dmp();  /* LRS 1190 dual port memory             */
               break;
             case  7:
               which_1190 = 3;
               fera_dmp();  /* LRS 1190 dual port memory             */
               break;
             case  8:
               which_1190 = 4;
               fera_dmp();  /* LRS 1190 dual port memory             */
               break;
             default:
               lan_printf("Unknown VME Device!\n");
               break;
           }
          break;
        case  MM:        /* modify memory in CPU-40        */
          size = 2;
          if (command[3] == 'b') size = 1;
          else if (command[3] == 'l') size = 4;
          memory_modify(size,args);
          break;
        case  DR:        /*  Display task registers       */
          task_ptr = (struct t_list *)syram->_tlst;
          task_ptr += (larg1 & 0x3f);
          tcb_ptr = (struct TCB *)task_ptr->tcb;
          reg_ptr = (struct TRegs *)tcb_ptr->_tsp;
          lan_printf("\n Task No. %i - CPU Registers\n",larg1 & 0x3f);
          lan_printf("       0        1        2        3        4\
        5        6        7\n");
          lan_printf("D  %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x\n",
       reg_ptr->d0,reg_ptr->d1,reg_ptr->d2,reg_ptr->d3,reg_ptr->d4,reg_ptr->d5,
                                                      reg_ptr->d6,reg_ptr->d7);
          lan_printf("A  %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x %8.8x\n",
       reg_ptr->a0,reg_ptr->a1,reg_ptr->a2,reg_ptr->a3,reg_ptr->a4,reg_ptr->a5,
                                                      reg_ptr->a6,reg_ptr);
          lan_printf("USP:  %8.8x  SSP:  %8.8x  SR:  %4.4x\n",reg_ptr->usp,
                                                          reg_ptr,reg_ptr->sr);
          lan_printf("PC:   %8.8x\n",reg_ptr->pc);
          break;
        case  HE:          /* Help message             */
          vmebug_help();
          break;
        case  LC:          /*  Set output line count   */
          if (args == 1) sscanf(sarg1,"%i",&larg1);
          if (args == 0) line_count = DEF_LC;
          else if (larg1 > 100) line_count = 100;
          else  line_count = larg1;
          break;
        case  PA:
          pac_dmp();
          break;
        default:
          lan_printf("Unknown VMEbug command!!\n");
          break;
      }
   }
}
/*****************************************************************************
*****************************************************************************/
void vmebug_help(void)
{
  int  i = 0;
  static char *mess[] = {
  "\n                       VMEbug  Commands\n",
  "\n",
  "Command Arguments:\n",
  "       saddr - start address(hex)\n",
  "       eaddr - end address(hex)\n",
  "       x     - Format code: B W L or C\n",
  "       n     - number(decimal)\n",
  "\n",
  "Command   Args                    Function\n",
  "\n",
  "  dm.x  saddr eaddr  Display CPU-40 memory in hex and ASCII format.\n",
  "  di.x  saddr eaddr  Disassemble CPU-40 code.\n",
  "  mm.x  saddr eaddr  Modify memory in CPU-40. Format codes - x -\n",
  "                     are: B W L or C.\n",
  "  dev  device        Debug a hardware device. Device codes are:\n",
  "                     LRS KSC ACRO TRIG CES FERA1 FERA2 FERA3 FERA4.\n",
  "  dr   [n]           Display 68040 registers for task 'n' in CPU-40.\n",
  "  pac                Disassemble a PAC code\n",
  "  lc   [n]           Set output line counter.\n",
  "  lt                 Display tasks loaded in the CPU-40.\n",
  "  help               Display this message.\n",
  NULL };

  while(mess[i] != NULL) lan_printf("%s",mess[i]), i++;
}
/*****************************************************************************
*
*   Display Force CPU-40 memory in hexadecimal format.
*
*   Access and display memory as bytes, words(default) or long words.
*
*   Call:   size  - display size in bytes
*           args  - number of command line arguments
*           larg1 - start address
*           larg2 - end address(if present)
*****************************************************************************/
void  hex_display(int size,int args)
{
   int   i,j,l,lc;
   unsigned char *bptr, *eptr;
   char  *cptr,str[40];
   unsigned short wtemp;
   unsigned long  ltemp;

/*
*   Check call arguments
*/
   bptr = (unsigned char *)larg1;
   if (args == 0)
     {
       lan_printf("\aNeed start address!\n");
       return;
     }
   else if (args == 1) eptr = (unsigned char *)0x7fffffff; 
   else   eptr = (unsigned char *)larg2;
   
   lan_printf("CPU-40 memory - hex/ASCII display\n");

   while(1)
    {
      l = j = 0;
      lc = line_count;
      while (bptr < eptr)
       {
         if(l%16 == 0)
          {
            str[j] = '\0';
            if (!lc--) break;
            lan_printf(" %16s\n",str);
            lan_printf("%4x: ",bptr);
            j = 0;
          }
         switch  (size)
          {
/*
*   Access and display as bytes.
*/
            case  1:
              lan_printf("%2.2x ",*bptr);
              cptr = (char *)bptr;
              break;
/*
*   Access and display as 16-bit words.
*/
            case  2:
              wtemp = *((short *)bptr);
              lan_printf("%4.4x ",wtemp);
              cptr = (char *)&wtemp;
              break;
/*
*   Access and display as 32-bit long words.
*/
            case  4:
              ltemp = *((long *)bptr);
              lan_printf("%8.8x ",ltemp);
              cptr = (char *)&ltemp;
              break;
            default:
              lan_printf("\a\aIllegal display size requested\n");
              break;
          }
/*
*   Also display bytes as ASCII characters
*/
         for (i=size; i > 0; --i)
          {
            if (*cptr < ' ' || *cptr > 'z') str[j] = '.';
            else  str[j] = *cptr;
            cptr++;
            j++;
          }
         l += size;
         bptr += size;
       }
     lan_printf(" %s\n",str);
/*
*   Get host command.  You can continue, specify new start address and
*   size or return to previous command level.
*/
     if ((args = get_next_cmd("--More--?  \b")) < 0) return;
     if (args != 0) bptr = (unsigned char *)larg1;
     if (args == 2)
       {
         if (*sarg2 == 'b') size = 1;
         else if (*sarg2 == 'w') size = 2;
         else if (*sarg2 == 'l') size = 4;
       }
    }
}
/*****************************************************************************
*
*   Modify Force CPU-40 memory.  NOTE: This is restricted to "data".
*   Modification of "code" may or may not work since the 68040 has separate
*   data and instruction caches.
*
* Call:  size  -  access/modify size in bytes
*        args  - number of arguments on command line
*        larg1 - address to modify
*****************************************************************************/
void  memory_modify(int size,int args)
{
   unsigned char *bptr;
   unsigned short wtemp;
   unsigned long  ltemp;

   bptr = (unsigned char *)larg1;
   if (args == 0)
    {
      lan_printf("\a\aNeed modify address\n");
      return;
    }
   
   lan_printf("CPU-40 memory modify\n");

/*
*  Read and display specified address
*/
   while(1)
    {
      lan_printf("%4x:  ",bptr);
/*
*   Access and display the contents of the current location.
*/
      switch  (size)
       {
         case  1:
           lan_printf("%2.2x  ",*bptr);
           break;
         case  2:
           wtemp = *((short *)bptr);
           lan_printf("%4.4x  ",wtemp);
           break;
         case  4:
           ltemp = *((long *)bptr);
           lan_printf("%8.8x  ",ltemp);
           break;
         default:
           break;
       }
/*
*   Get the change(if any) from the host.
*/
     if ((args = get_next_cmd(" \b")) < 0) return;
     if (args == 1)
      {
/*
*   If user modified the location, make the change in CPU-40 memory.
*/
       switch  (size)
        {
          case  1:
            *bptr = larg1;
            break;
          case  2:
            *((short *)bptr) = larg1;
            break;
          case  4:
            *((long *)bptr) = larg1;
            break;
          default:
            break;
        }
      }
     bptr += size;
    }
}
/***************************************************************************
*
*   Dissassemble CPU-40 code.
*
* Call:  type - type of address space to disassemble: Types are:
*               Byte, Word, Long or Code
*        args - number of arguments on command line.
*
*  If both start and end addresses are specified, first run the disassembler
*  as pass 1 to build the reference table.  Then run pass 2, to list
*  the disassembler output.
*
***************************************************************************/
void  disassemble(int type,int args)
{
  unsigned char *bptr, *eptr;
  
/*
*  If both a start and end address supplied, run  disassembler Pass 1.
*/
  if (args == 2) vmedis(1,(unsigned char *)larg1,(unsigned char *)larg2,
                                                              type,line_count);
  bptr = (unsigned char *)larg1;
  eptr = bptr + 0x80;
  while(1)
   {
     bptr = vmedis(2,bptr,eptr,type,line_count);
     if ((args = get_next_cmd("--More--?  \b")) < 0) return;
     if (args != 0) bptr = (unsigned char *)larg1;
     eptr = bptr + 0x80;
     if (args == 2)
       {
         if (*sarg2 == 'b') type = 0;
         else if (*sarg2 == 'w') type = 1;
         else if (*sarg2 == 'l') type = 2;
         else if (*sarg2 == 'c') type = 3;
       }
   }
}
/***************************************************************************
*
*  Get a command from the host.  This routine used at top level command
*  input.  String is expected to consist of a command and upto two
*  arguments.
*
*  Call:  prompt  - pointer to prompt string
*
*  Return:  args - number of arguments found (1 or 2)
*           larg1 - value of argument 1
*           larg2 - value of argument 2
*
***************************************************************************/
int get_cmd(char *prompt)
{
  char  *inbuf, *cptr;
  int   args, i;
  static int ird = 0;

  while(1)
   {
     if (ird != 0) lan_printf("%s",prompt);
     lan_read(0,&inbuf);
     ird = 1;
     cptr = (char *)inbuf;
/*
*  Convert buffer to lower case
*/
     for (i=0; i < 81; i++)
      {
        if (isupper(inbuf[i])) inbuf[i] = (char)tolower(inbuf[i]);
      }
     larg1 = larg2 = 0;
     args = 0;
     strcpy(command,"     ");
/*
*   Parse the command line into a command and upto two arguments.
*/
     if ((cptr = gfield(command,inbuf,5)) == NULL) continue;
     if (*command == '.') return(-1);
     if ((cptr = gfield(sarg1,cptr,9)) != NULL)
      {
        ++args;
        sscanf(sarg1,"%x",&larg1);
        if ((cptr = gfield(sarg2,cptr,9)) != NULL)
         {
           ++args;
           sscanf(sarg2,"%x",&larg2);
         }
      }
     return(args);
   }
}
/***************************************************************************
*
*   Get next string from host.  This string is expected to be one or two
*   arguments.  Function is used when current command can continue or
*   expects data.
*
*  Call:  prompt - pointer to prompt string
*
* Return:  args - number of arguments found.
*                 -1 means first argument is not hex.  Usually means
*                 exit current command.
*          larg1 - value of first argument
*          larg2 - value of second argument
*          sarg1 - string for first argument
*          sarg2 - string for second argument
*
***************************************************************************/
int get_next_cmd(char * prompt)
{
  char  *inbuf, *cptr;
  int   args, i;

  lan_printf("%s",prompt);
  lan_read(0,&inbuf);
  cptr = (char *)inbuf;
/*
*  Convert string to lower case
*/
  for (i=0; i < 81; i++)
   {
     if (isupper(inbuf[i])) inbuf[i] = (char)tolower(inbuf[i]);
   }
  larg1 = larg2 = 0;
  args = 0;
  if ((cptr = gfield(sarg1,inbuf,9)) == NULL) return(0);
  for (i=0; i < sizeof(sarg1); i++)
   {
     if (sarg1[i] == '\0') break;
     if (!isxdigit((int)sarg1[i])) return(-1);
   }
  ++args;
  sscanf(sarg1,"%x",&larg1);
  if ((cptr = gfield(sarg2,cptr,9)) != NULL)
    {
      ++args;
      sscanf(sarg2,"%x",&larg2);
    }
  return(args);
}
/***************************************************************************
*
*  Extract one field from the source string.  Fields are delimited by
*  space(s), tab(s) and new_line characters.
*
* Call:  s1  -  pointer to destination string
*        s2  -  pointer to source string
*        n   -  number of characters(including the NULL at end of string)
*               to store in destination.  If the field in the source
*               is greater than n-1, the left most n-1 characters are
*               returned.
*
* Return:  Pointer to next character in source sting following this
*          field.
***************************************************************************/
char *gfield(char *s1,char *s2,int n)
{
   char *cptr;

   *s1 = '\0';
   for (;*s2 == ' ' || *s2 == '\t'; s2++);
   for (cptr = s2; *cptr != ' ' && *cptr != '\0' && *cptr != '\n'; cptr++);
   if (cptr - s2 != 0)
     {
       if (cptr - s2 < n) n = cptr - s2;
       else   n = n -1;
       strncpy(s1,s2,(size_t)n);
       *(s1 + n) = '\0';
       return(cptr);
     }
   return ((char *)NULL);
}
/****************************************************************************
*
*   Debug routine for the LeCroy 1131 VME Interface to SIB bus.
*   Routine allows display of hardware registers and module memory.
*
****************************************************************************/
void lrs_dmp(void)
{
  struct LRS_VME *lrs = (struct LRS_VME *)LRS1;
            int  args,i,lc,tmp;
  unsigned short csr,nta,wc,sib;
           int   dat,iwd,mod,reg;
           char  str[4],type;

  lan_printf("LeCroy 1131 \n\n");

  while(1)
   {
/*
*   Get command line from host
*/
     args = get_cmd("LRS cmd>  \b");
     if (command[0] == '.') return;
     gfield(str,command,3);
     i = 0;
     while (cmd[i] != NULL)
      {
        if(!strcmp(str,cmd[i])) break;
        ++i;
      }
/*
*   Module has two types of memory space.  The first 8K 16-bit words are
*   for command lists.  The second 8K 16-bit words are data.
*   dm.l selects list memory and dm.d selects data memory.
*/
     type = 'L';
     if (command[3] == 'd') type = 'D';
     switch  ((enum func)i)
      {
/*
*    Display module memory in hex format
*/
        case  DM:
          while(1)
           {
             tmo = 0;
             if (larg1 > 8191)
               {
                 lan_printf("LRS dm argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 256;
             if (larg2 > 8192) larg2 = 8192;
             i = 0;
             lc = line_count;
             while(larg1 < larg2)
              {
                if (i%8 == 0)
                  {
                    if (!lc--) break;
                    lan_printf("\n%c%4.4x:  ",type,larg1);
                  }
                lan_printf("%4x ",lrs_get_dat(type,larg1++));
                i++;
                if(tmo) break;
              }
             lan_printf("\n");
             tmp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = tmp;
             if (args == 2)
               {
                 if (*sarg2 == 'l') type = 'L';
                 else if (*sarg2 == 'd') type = 'D';
                 larg2 = 0;
               }
           }
          break;
/*
*   Disassemble a command list in the module memory
*/
        case  DI:
          while(1)
           {
             tmo = 0;
             if (larg1 > 8191)
               {
                 lan_printf("LRS di argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 256;
             if (larg2 > 8192) larg2 = 8192;
             lc = line_count;
             while(larg1 < larg2)
              {
                iwd = lrs_get_dat('L',larg1);
                if (tmo) break;
                dat = lrs_get_dat('D',larg1);
                lan_printf("\n%4x:  ",larg1);
                larg1++;
                if ((iwd & 0xc801) != 0)
                  {
                    lan_printf("%4x  Illegal list instruction word\n",iwd);
                    break;
                  }
                reg = (iwd >> 1) & 0xf;
                mod = (iwd >> 5) & 0xf;
                if ((iwd & 0x1000) == 0) lan_printf("Write ");
                else  lan_printf(" Read ");
                lan_printf("Register %2i, Data = %4x",reg,dat);
                if ((iwd & 0x600) == 0x200) lan_printf(", Module %x",mod);
                if ((iwd & 0x2000) != 0) lan_printf(", End-of-List");
                if (!--lc) break;
              }
             lan_printf("\n");
             tmp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = tmp;
           }
          break;
/*
*   Display hardware registers in the LRS 1131 module
*/
        case  DR:
          tmo = 0;
          if ((lrs->csr & LRS_CSR_RUN) != 0)
            {
              delay_(1);
              if (tmo++ > 3)
                {
                  lan_printf("\a\a*** Can't access LRS 1131!\n");
                  break;
                }
            }
          csr = lrs->csr;
          nta = lrs->nta;
          wc = lrs->wc;
          sib = lrs->sib;
          lan_printf("\nLRS 1131 registers\n");
          lan_printf("csr:   %4x  nta:   %4x  wc:  %4x  sib:   %4x\n",
                                                               csr,nta,wc,sib);
          break;
        case  LC:          /*  Set output line count   */
          if (args == 1) sscanf(sarg1,"%i",&larg1);
          if (args == 0) line_count = DEF_LC;
          else if (larg1 > 100) line_count = 100;
          else  line_count = larg1;
          break;
/*
*    LRS 1131 help
*/
        case  HE:
          lrs_help();
          break;
        default:
          lan_printf("Unknown LRS command!\n");
          break;
      }
   }
}
/****************************************************************************
*
*  Get one 16-bit word from modules memory.  Routine reads 16 words for
*  every hardware access.  Subsequent calls just return the data word
*  until requested address is not in memory.
*
* Call:    type  -  L = list memory, D = data memory
*          addr  -  address(module internal address) of data requested.
*
* Return:  value stored in the module memory at specified address
*
****************************************************************************/
unsigned short lrs_get_dat(char type,unsigned long addr)
{
  struct mod_mem {
             int  saddr;
             int  eaddr;
  unsigned short  buf[16];
 } ;
  static struct mod_mem  lbuf,dbuf;
  struct mod_mem *buffer;
  struct LRS_VME *lrs = (struct LRS_VME *)LRS1;
  unsigned short *lrs_mem;
  int  i,j,level;

  if ((lrs->csr & LRS_CSR_RUN) != 0)
    {
      delay_(1);
      if (tmo++ > 3)
        {
          lan_printf("\a\a*** Can't access LRS 1131!\n");
          return(0);
        }
    }
  if (type == 'L') buffer = &lbuf;
  else
    {
      buffer = &dbuf;
      addr += 8192;
    }
  if (buffer->saddr == buffer->eaddr || addr < buffer->saddr ||
                                                         addr > buffer->eaddr)
   {
     level = set_intr_level_(0x500);
     lrs_mem = (unsigned short *)(LRS1 + LIST);
     lrs_mem += addr;
     if ((j = 16384 - addr) > 16) j = 16;
     for (i = 0; i < j; i++) buffer->buf[i] = *lrs_mem++;
     set_intr_level_(level);
     buffer->saddr = addr;
     buffer->eaddr = buffer->saddr + 15;
   }
  return (buffer->buf[addr-buffer->saddr]);
}
/*****************************************************************************
*
*   LeCroy 1131 help routine
*****************************************************************************/
void lrs_help(void)
{
  int  i = 0;
  static char *mess[] = {
  "\n                       LRS 1131 Commands\n",
  "\n",
  "Command Arguments:\n",
  "       saddr - start address(hex)\n",
  "       eaddr - end address(hex)\n",
  "       x     - L or D for List or Data memory\n",
  "       n     - number(decimal)\n",
  "\n",
  "Command   Args                    Function\n",
  "\n",
  "  dm.x  saddr eaddr  Display LRS 1131 memory in hex format\n",
  "                     x = L or D for List or Data memory.\n",
  "  di    saddr eaddr  Disassemble LRS 1131 command list sequences.\n",
  "  dr                 Display LRS 1131 registers.\n",
  "  lc   [n]           Set output line counter.\n",
  "  lt                 Display tasks loaded in the CPU-40.\n",
  "  help               Display this message.\n",
  NULL };

  while(mess[i] != NULL) lan_printf("%s",mess[i]), i++;
}
/****************************************************************************
*
*  Debug routine Kinetic Systems 2917 VME Interface to CAMAC.
*
****************************************************************************/
void ksc_dmp(void)
{
  register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
  int  args,i,lc,temp;
  int  cdata,err,ad,qm,ws;
  char  str[4];
  unsigned short wd1,wd2;
  short  wc;
  static char *qmode[] = {"Q-Stop","Q-Ignore","Q-Repeat","Q-Scan"};

  lan_printf("Kinetic Systems 2917 \n\n");

  while(1)
   {
     args = get_cmd("KSC cmd>  \b");
     if (command[0] == '.') return;
     gfield(str,command,3);
     i = 0;
     while (cmd[i] != NULL)
      {
        if(!strcmp(str,cmd[i])) break;
        ++i;
      }
     switch  ((enum func)i)
      {
/*
*   Display module memory in hex format
*/
        case   DM:
          while(1)
           {
             tmo = 0;
             if (larg1 > 8192)
               {
                 lan_printf("KSC dm argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 256;
             if (larg2 > 8192) larg2 = 8192;
             i = 0;
             lc = line_count;
             while(larg1 < larg2)
              {
               if (i%8 == 0)
                {
                  if (!lc--) break;
                  lan_printf("\n%4x:  ",larg1);
                }
               lan_printf("%4x ",ksc_get_dat(larg1));
               larg1++;
               i++;
               if (tmo) break;
              }
             lan_printf("\n");
             temp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = temp;
             larg2 = 0;
           }
          break;
/*
*    Disassemble module command list in module memory
*/
        case   DI:
          while(1)
           {
             tmo = 0;
             if (larg1 > 8192)
               {
                 lan_printf("KSC di argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 100;
             if (larg2 > 8192) larg2 = 8192;
             lc = line_count;
             while(larg1 < larg2)
              {
                lan_printf("\n%4x:  ",larg1);
                wd1 = ksc_get_dat(larg1++);
                if (tmo) break;
                if ((wd1 & 0xf800) != 0)
                  {
                    lan_printf("%4x  Illegal instruction",wd1);
                    break;
                  }
                qm = (wd1 & 0x18) >> 3;
                ws = wd1 & 6;
                if ((ws & 0x4) != 0)
                  {
                    lan_printf("%4x  Illegal transfer size",wd1);
                    break;
                  }
                ad = wd1 & 1;
                err = 0;
                if ((wd1 & 0x80) != 0)
                  {
                    if ((wd1 & 0xff3f) != 0)
                      {
                        lan_printf("%4x  Illegal instruction",wd1);
                        break;
                      }
                    switch (wd1 & 0xc0)
                     {
                      case  0x80:          /* Halt Instruction */
                        lan_printf("HALT");
                        break;
                      case  0xc0:          /* Jump instruction */
                        wd2 = ksc_get_dat(larg1++);
                        lan_printf("JUMP %x\n",wd2);
                        if ((wd2 & 0xe000) != 0)
                          {
                            lan_printf("  Illegal JUMP address");
                            err = 1;
                          }
                        break;
                      default:
                        break;
                     }
                  }
                else
                  {
                    wd2 = ksc_get_dat(larg1++);;
                    lan_printf("c = %d, n = %d, a = %d, f = %d",
                (wd1 >> 8),((wd2 >> 9) & 0x1f),((wd2 >> 5) &0xf),(wd2 & 0x1f));
                    switch (wd1 & 0xe0)
                     {
                       case  0:             /* Single CAMAC    */
                         if (ws == 2) lan_printf(", 16-bit");
                         else  lan_printf(", 24-bit");
                         if (ad == 0) lan_printf(", Abort on Err");
                         if (qm != 0)
                          {
                            lan_printf(", Illegal Q mode");
                            err = 1;
                          }
                         break;
                       case  0x20:          /* Block CAMAC     */
                         wc = -ksc_get_dat(larg1++);
                         lan_printf(" BLOCK, wc = %i, ",wc);
                         lan_printf(", %s",qmode[qm]);
                         ksc_get_dat(larg1++);
                         break;
                       case  0x60:          /* In-line write   */
                         wd2 = ksc_get_dat(larg1++);
                         cdata = (ksc_get_dat(larg1++) & 0xff);
                         if (ws == 2) cdata = 0;
                         cdata = (cdata << 16) | wd2;
                         lan_printf(", INLINE-WRITE, data = %i",cdata);
                         if (qm != 0)
                          {
                            lan_printf(", Illegal Q mode");
                            err = 1;
                          }
                         break;
                       default:
                         break;
                     }
                  }
                if (!--lc || err != 0 || tmo) break;
              }
             lan_printf("\n");
             temp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = temp;
             larg2 = 0;
           }
          break;
/*
*    Display hardware registers in the KSC 2917 module
*/
        case  DR:               /* Display hardware registers    */
          tmo = 0;
          if ((ksc->csr & KSC_CSR_GO) != 0)
            {
              delay_(1);
              if (tmo++ > 3)
                {
                  lan_printf("\a\a*** Can't access KSC 2917!\n");
                  break;
                }
            }
          lan_printf("\nKSC 2917 registers\n");
          lan_printf("cse:   %4x  doc:   %4x  scc: %4x\n",ksc->cse,
                                                            ksc->doc,ksc->scc);
          lan_printf("machi: %4x  maclo: %4x  mtc: %4x\n",ksc->machi,
                                                          ksc->maclo,ksc->mtc);
          lan_printf("LAM_icr:   %4x   LAM_ivr:   %4x\n",ksc->LAM_icr,
                                                                 ksc->LAM_ivr);
          lan_printf("DMA_icr:   %4x   DMA_ivr:   %4x\n",ksc->DMA_icr,
                                                                 ksc->DMA_ivr);
          lan_printf("done_icr:  %4x   done_ivr:  %4x\n",ksc->done_icr,
                                                                ksc->done_ivr);
          lan_printf("abort_icr: %4x   abort_ivr: %4x\n",ksc->abort_icr,
                                                               ksc->abort_ivr);
          temp = ksc->cma;
          lan_printf("amr:  %4x  cmr:  %4x  cma:  %4x  cwc:  %4x\n",
                                          ksc->amr,ksc->cmr,temp,ksc->cwc);
          ksc->cma = temp;
          lan_printf("ssr:  %4x  csr:  %4x\n", ksc->ssr,ksc->csr);
          break;
        case  LC:          /*  Set output line count   */
          if (args == 1) sscanf(sarg1,"%i",&larg1);
          if (args == 0) line_count = DEF_LC;
          else if (larg1 > 100) line_count = 100;
          else  line_count = larg1;
          break;
/*
*     KSC 2917 help
*/
        case  HE:          /*  KSC 2917 help              */
          ksc_help();
          break;
        default:
          lan_printf("Unknown KSC command!\n");
          break;
      }          /* end switch    */
   }             /* end while(1)  */
}
/****************************************************************************
*
*   Read memory in the module.  Reads 16 words for each hardware access.
*   Returns the word specified.  Subsequent calls get data from the
*   internal buffer if present.
*
* Call:  addr  -  address(internal module address)
*
* Return: 16-bit word stored at the specified address in the module memory.
*
****************************************************************************/
unsigned short ksc_get_dat(unsigned long addr)
{
  struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
  static int saddr = 0xfffff, eaddr;
  static unsigned short dat[16];
  unsigned short temp;
  int  i,j,level;

  if ((ksc->csr & KSC_CSR_GO) != 0)
    {
      delay_(1);
      if (tmo++ > 3)
        {
          lan_printf("\a\a*** Can't access KSC 2917!\n");
          return(0);
        }
    }
  tmo = 0;
  if (addr < saddr || addr > eaddr)
   {
     level = set_intr_level_(0x500);
     temp = ksc->cma;
     ksc->cma = addr;
     if ((j = 8192 - addr) > 16) j = 16;
     for (i = 0; i < j; i++) dat[i] = ksc->cmr;
     ksc->cma = temp;
     set_intr_level_(level);
     saddr = addr;
     eaddr = saddr + 15;
   }
  return (dat[addr-saddr]);
}
/*****************************************************************************
*
*  KSC 2917 help routine
*
*****************************************************************************/
void ksc_help(void)
{
  int  i = 0;
  static char *mess[] = {
  "\n                       KSC 2917 Commands\n",
  "\n",
  "Command Arguments:\n",
  "       saddr - start address(hex)\n",
  "       eaddr - end address(hex)\n",
  "       n     - number(decimal)\n",
  "\n",
  "Command   Args                    Function\n",
  "\n",
  "  dm    saddr eaddr  Display KSC 2917 memory in hex format.\n",
  "  di    saddr eaddr  Disassemble command lists in KSC 2917 memory.\n",
  "  dr                 Display KSC 2917 registers.\n",
  "  lc   [n]           Set output line counter.\n",
  "  lt                 Display tasks loaded in the CPU-40.\n",
  "  help               Display this message.\n",
  NULL };

  while(mess[i] != NULL) lan_printf("%s",mess[i]), i++;
}
/****************************************************************************
*
*   Debug routine for the CES 8170 High Speed Memory module.
*   Routine allows display of hardware registers and module memory.
*
****************************************************************************/
void ces_dmp(void)
{
   struct hsm {
     union 
      {
        unsigned long l[0x40000];
        unsigned short w[0x80000];
      } mem;
     long intr;
     long ctrl;
     long mem_ptr;
     long wrd_cnt;
      };

  struct hsm *hsm_ptr = (struct hsm *)CES1;
  int  args,i,j,lc,size,tmp;
  char str[4];

  lan_printf("CES 8170 \n\n");

  while(1)
   {
/*
*   Get command line from host
*/
     args = get_cmd("CES cmd>  \b");
     if (command[0] == '.') return;
     gfield(str,command,3);
     i = 0;
     while (cmd[i] != NULL)
      {
        if(!strcmp(str,cmd[i])) break;
        ++i;
      }
     sarg2[0] = command[3];
     switch  ((enum func)i)
      {
/*
*    Display module memory in hex format
*/
        case  DM:
          while(1)
           {
             if (*sarg2 == 'l') size = 4;
             else  size = 2;
             if (larg1 > 0x20000)
               {
                 lan_printf("CES dm argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 256;
             if (larg2 > 0x20000) larg2 = 0x20000;
             j = larg1/size;
             i = 0;
             lc = line_count;
             while(larg1 < larg2)
              {
                if ((size == 2 && i%8 == 0) || (size == 4 && i%4 == 0))
                  {
                    if (!lc--) break;
                    lan_printf("\n%5.5x:  ",larg1);
                  }
                if (size == 2) lan_printf("%4.4x ",hsm_ptr->mem.w[j]);
                else  lan_printf("%8.8x ",hsm_ptr->mem.l[j]);
                i++;
                j++;
                larg1 += size;
              }
             lan_printf("\n");
             tmp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = tmp;
             if (args == 2)
               {
                 if (*sarg2 == 'l') size = 4;
                 else  size = 2;
                 larg2 = 0;
               }
           }
          break;
/*
*    Fill memory with user specified value.
*/
        case  FM:
          for (i = 0; i < 0x10000; i++) hsm_ptr->mem.w[i] = larg1;
          break;
/*
*   Display hardware registers in the CES 8170 module
*/
        case  DR:
          lan_printf("Interrupt  = %8x\n",hsm_ptr->intr);
          lan_printf("Control    = %8x\n",hsm_ptr->ctrl);
          lan_printf("Address    = %8x\n",hsm_ptr->mem_ptr);
          lan_printf("Word count = %8x\n",hsm_ptr->wrd_cnt);
          break;
        case  LC:          /*  Set output line count   */
          if (args == 1) sscanf(sarg1,"%i",&larg1);
          if (args == 0) line_count = DEF_LC;
          else if (larg1 > 100) line_count = 100;
          else  line_count = larg1;
          break;
/*
*    CES 8170 help
*/
        case  HE:
          ces_help();
          break;
        default:
          lan_printf("Unknown CES command!\n");
          break;
      }
   }
}
/*****************************************************************************
*
*   CES 8170 help routine
*****************************************************************************/
void ces_help(void)
{
  int  i = 0;
  static char *mess[] = {
  "\n                       CES 8170 Commands\n",
  "\n",
  "Command Arguments:\n",
  "       saddr - start address(hex)\n",
  "       eaddr - end address(hex)\n",
  "       x     - W or L for word or long word display\n",
  "       n     - number(decimal)\n",
  "\n",
  "Command   Args                    Function\n",
  "\n",
  "  dm.x  saddr eaddr  Display CES 8170 memory in hex format\n",
  "                     x = W or L for word or long word display.\n",
  "  fm   value         Fill memory with 'value'\n",
  "  dr                 Display CES 8170 registers.\n",
  "  lc   [n]           Set output line counter.\n",
  "  lt                 Display tasks loaded in the CPU-40.\n",
  "  help               Display this message.\n",
  NULL };

  while(mess[i] != NULL) lan_printf("%s",mess[i]), i++;
}
/****************************************************************************
*
*   Debug routine for the LeCroy 1190 Dual Port Memory module.
*   Routine allows display of address register and module memory.
*
****************************************************************************/
void fera_dmp(void)
{
   struct Lrs1190 {
     union 
      {
        unsigned long l[32768];
        unsigned short w[65536];
      } mem;
     unsigned short addr;
     unsigned short mode;
      };
  int  args,i,j,lc,size,tmp;
  char str[4],prompt[16];
  struct Lrs1190 *lrs_ptr;


  lan_printf("LRS 1190 (FERA%i)\n\n",which_1190);

  switch (which_1190)
   {
       case 1:
          lrs_ptr = (struct Lrs1190 *)LRS1190A;
          break;
       case 2:
          lrs_ptr = (struct Lrs1190 *)LRS1190B;
          break;
       case 3:
          lrs_ptr = (struct Lrs1190 *)LRS1190C;
          break;
       case 4:
          lrs_ptr = (struct Lrs1190 *)LRS1190D;
          break;
       default:
          lrs_ptr = (struct Lrs1190 *)0;
          break;
   }

  lrs_ptr->mode = 0;       /* Disable front panel input  */
  sprintf(prompt,"FERA%i cmd>  \b",which_1190);

  while(1)
   {
/*
 *   Get command line from host
 */
     args = get_cmd(prompt);
     if (command[0] == '.') return;
     gfield(str,command,3);
     i = 0;
     while (cmd[i] != NULL)
      {
        if(!strcmp(str,cmd[i])) break;
        ++i;
      }
     sarg2[0] = command[3];
     switch  ((enum func)i)
      {
/*
 *    Display module memory in hex format
 */
        case  DM:
          while(1)
           {
             if (*sarg2 == 'l') size = 4;
             else  size = 2;
             if (larg1 > 0x20000)
               {
                 lan_printf("FERA dm argument error!\n");
                 break;
               }
             if (larg2 == 0) larg2 = larg1 + 256;
             if (larg2 > 0x20000) larg2 = 0x20000;
             j = larg1/size;
             i = 0;
             lc = line_count;
             while(larg1 < larg2)
              {
                if ((size == 2 && i%8 == 0) || (size == 4 && i%4 == 0))
                  {
                    if (!lc--) break;
                    lan_printf("\n%5.5x:  ",larg1);
                  }
                if (size == 2) lan_printf("%4.4x ",lrs_ptr->mem.w[j]);
                else  lan_printf("%8.8x ",lrs_ptr->mem.l[j]);
                i++;
                j++;
                larg1 += size;
              }
             lan_printf("\n");
             tmp = larg1;
             if ((args = get_next_cmd("--More--?  \b")) < 0) break;
             if (args == 0) larg1 = tmp;
             if (args == 2)
               {
                 if (*sarg2 == 'l') size = 4;
                 else  size = 2;
                 larg2 = 0;
               }
           }
          break;
/*
 *    Fill memory with user specified value
 */
        case  FM:
          for (i = 0; i < 0x10000; i++) lrs_ptr->mem.w[i] = larg1;
          break;
/*
 *   Display address register in the LRS 1190 module
 */
        case  DR:
          lan_printf("Address    = %8x\n",lrs_ptr->addr);
          break;
        case  LC:          /*  Set output line count   */
          if (args == 1) sscanf(sarg1,"%i",&larg1);
          if (args == 0) line_count = DEF_LC;
          else if (larg1 > 100) line_count = 100;
          else  line_count = larg1;
          break;
/*
 *    FERA help
 */
        case  HE:
          fera_help();
          break;
        default:
          lan_printf("Unknown FERA command!\n");
          break;
      }
   }
}
/*****************************************************************************
*
*   LRS 1190 help routine
*****************************************************************************/
void fera_help(void)
{
  int  i = 0;
  static char *mess[] = {
  "\n                         FERA   Commands\n",
  "\n",
  "Command Arguments:\n",
  "       saddr - start address(hex)\n",
  "       eaddr - end address(hex)\n",
  "       x     - W or L for word or long word display\n",
  "       n     - number(decimal)\n",
  "\n",
  "Command   Args                    Function\n",
  "\n",
  "  dm.x  saddr eaddr  Display LRS 1190 memory in hex format\n",
  "                     x = W or L for word or long word display.\n",
  "  fm   value         Fill memory with 'value'\n",
  "  dr                 Display LRS 1190 address register.\n",
  "  lc   [n]           Set output line counter.\n",
  "  lt                 Display tasks loaded in the CPU-40.\n",
  "  help               Display this message.\n",
  NULL };

  while(mess[i] != NULL) lan_printf("%s",mess[i]), i++;
}
/****************************************************************************
****************************************************************************/
void acro_dmp(void)
{
  struct acromag *avme = (struct acromag *)ACROMAG;

  lan_printf("\nACROmag registers\n");
  lan_printf("   csr   = %2x\n",avme->csr);
  lan_printf("ip_clr   = %2x\n",avme->ip_clr);
  lan_printf("inputs   = %2x\n",avme->intr_in);
  lan_printf("enables  = %2x\n",avme->intr_ena);
  lan_printf("level    = %2x\n",avme->intr_level);
  lan_printf("polarity = %2x\n",avme->intr_pol);
  lan_printf("Vector 0 = %2x  Vector 1 = %2x  Vector 2 = %2x  \
Vector 3 = %2x\n",avme->intr_vec0,avme->intr_vec1,avme->intr_vec2,
avme->intr_vec3);
  lan_printf("Vector 4 = %2x  Vector 5 = %2x  Vector 6 = %2x  \
Vector 7 = %2x\n",avme->intr_vec4,avme->intr_vec5,avme->intr_vec6,
avme->intr_vec7);
  lan_printf("\n");
  lan_printf("Port 0 = %2x  Port 1 = %2x  Port 2 = %2x  \
Port 3 = %2x\n",avme->port0,avme->port1,avme->port2,avme->port3);
  lan_printf("Port 4 = %2x  Port 5 = %2x  Port 6 = %2x  \
Port 7 = %2x\n",avme->port4,avme->port5,avme->port6,avme->port7);
}
/****************************************************************************
****************************************************************************/
void trig_dmp(void)
{
  static struct trig intrig;
         struct trig *trigger = (struct trig *)TRIGGER;

  intrig.gpip = trigger->gpip;
  intrig.aer = trigger->aer;
  intrig.ddr = trigger->ddr;
  intrig.iera = trigger->iera;
  intrig.ierb = trigger->ierb;
  intrig.ipra = trigger->ipra;
  intrig.iprb = trigger->iprb;
  intrig.isra = trigger->isra;
  intrig.isrb = trigger->isrb;
  intrig.imra = trigger->imra;
  intrig.imrb = trigger->imrb;
  intrig.vr = trigger->vr;
  intrig.tacr = trigger->tacr;
  intrig.tbcr = trigger->tbcr;
  intrig.tcdcr = trigger->tcdcr;
  intrig.tadr = trigger->tadr;
  intrig.tbdr = trigger->tbdr;
  intrig.tcdr = trigger->tcdr;
  intrig.tddr = trigger->tddr;
  intrig.scr = trigger->scr;
  intrig.ucr = trigger->ucr;
  intrig.rsr = trigger->rsr;
  intrig.udr = trigger->udr;
  intrig.delay_time = trigger->delay_time;

  lan_printf("\nORNL Trigger module registers\n");
  lan_printf("Interrupt Vector = %2x\n",intrig.vr);
  lan_printf("iera = %2x ipra = %2x isra = %2x imra = %2x\n",intrig.iera,
                                         intrig.ipra,intrig.isra,intrig.imra);
  lan_printf("ierb = %2x iprb = %2x isrb = %2x imrb = %2x\n\n",intrig.ierb,
                                         intrig.iprb,intrig.isrb,intrig.imrb);
  lan_printf("gpip = %2x aer  = %2x ddr  = %2x\n\n",intrig.gpip,intrig.aer,
                                                                 intrig.ddr);
  lan_printf(" tacr = %2x tadr = %2x\n",intrig.tacr,intrig.tadr);
  lan_printf(" tbcr = %2x tbdr = %2x\n",intrig.tbcr,intrig.tbdr);
  lan_printf("tcdcr = %2x tcdr = %2x tddr = %2x\n",intrig.tcdcr,intrig.tcdr,
                                                                  intrig.tddr);
  lan_printf("delay_time = %2x\n",intrig.delay_time);
}
/****************************************************************************
****************************************************************************/
void pac_dmp(void)
{
                int i,j;
		int status;
		int *iptr;
   struct tbl_index *tblptr;
      unsigned char *cptr;
struct cnafdat_list *naf1,*naf2;
	static char *table_names[] = {"FILENAME","CRATES","CNAF_INIT",
                                      "CAMAC_MODULES","FASTBUS_MODULES",
				      "FERA_MODULES","LATCHES","GATE_READ",
                                      "RAW_GATE_SPEC","CAL_GATE_SPEC",
                                      "COUNT_DOWN","KILL_LIST",
				      "UNCOND_RO","COND_RO","CNAF_LIST",
				      "ID_LIST","CAMAC_RO","FASTBUS_RO",
                                      "FERA_RO","CAMAC_ID", "FASTBUS_ID",
				      "FERA_ID","WINDUP_CNAF","RUN_CNAF_LIST",
                                      "XIA_LIST"};
         static char pacfile[128];
		int invalid = 0;
                int sum_len = 0;

/*
*    Check all tables.  Offsets, lengths and delays must be within
*    reasonable ranges.
*/
   lan_printf("\nTable Index:\n");
   tblptr = (struct tbl_index *)ACQ_RAM;
   for(i=0; i < 25; ++i)
    {
      status = 0;
      if ((tblptr->offset < 0) || (tblptr->length < 0)) status = 1;
      else if (tblptr->offset + tblptr->length > ACQ_MAX_RAM) status = 1;      
      else if (tblptr->delay > ACQ_MAX_DELAY) status = 1;
      if (status)
	{
	  lan_printf("Invalid %s parameter table\n",table_names[i]);
	  invalid = 1;
	}
      sum_len += tblptr->length;

      lan_printf("off = %xh, len = %d, dly = %d, Address = %xh  %s\n",
                   tblptr->offset,tblptr->length,tblptr->delay,
                   ACQ_RAM + tblptr->offset * 4,table_names[i]);
      tblptr++;
    }
   if (!sum_len)  return;
   if (invalid)  return;
/*
*  Fix file name storage, if needed.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FILENAME;
   cptr = (unsigned char *)( ACQ_RAM + tblptr->offset * 4);
   strncpy(pacfile,(char *)cptr,tblptr->length*4);
   if (*(cptr+tblptr->length*4-1) != 0xff)
     {
       byte_swap_(pacfile,tblptr->length*4);
       word_swap_((short *)pacfile,tblptr->length*2);
       cptr =  (unsigned char *)pacfile+tblptr->length*4 -2;
       *cptr++ = 0;
       *cptr = 0xff;
     }
   cptr = (unsigned char *)pacfile; 
   cptr = cptr + tblptr->length * 4 -3;
   while (cptr >= (unsigned char *)(ACQ_RAM + tblptr->offset * 4))
     {
       if (*cptr != ' ')
         {
           cptr++;
           *cptr = 0;
           break;
         }
       cptr--;
     }
   lan_printf("\nPacFile:  %s\n",pacfile);
/*
*   Copy the camac unconditional readout list to the heap.  The latch dump
*   routine looks for a second read of the latch in the unconditional list.
*   If found, the entry in the unconditional list is marked invalid
*   by setting f=255.  The pac_dmp_uncond routine then ignores entries
*   which have f=255.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_UNCOND_RO;
   naf1 = (struct cnafdat_list *)(ACQ_RAM + tblptr->offset * 4);
   uncond_cnt = tblptr->length;
   tmp_uncond = calloc(tblptr->length,sizeof(struct cnafdat_list));
   naf2 = tmp_uncond;
   for (i=0; i < tblptr->length; i++) *naf2++ = *naf1++;
   pac_dmp_startvme();
   pac_dmp_crates();
   pac_dmp_init();
   pac_dmp_cammod();
   pac_dmp_feramod();
   pac_dmp_fastmod();
   pac_dmp_xiamod();
   pac_dmp_latch();
   pac_dmp_kill();
   pac_dmp_fera_cond();
   pac_dmp_fast_cond();
   pac_dmp_camac_cond();
   pac_dmp_gates();
   pac_dmp_uncond();
   pac_dmp_cond();
   pac_dmp_windup();
   free(tmp_uncond);
   return;
}
/****************************************************************************
****************************************************************************/
void pac_dmp_startvme(void)
{
   int  i;
   struct tbl_index *tblptr;
   struct cnafdat_list *camptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RUN_CNAF_LIST;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC `startvme' list:   %d NAFs\n",tblptr->length);
   camptr = (struct cnafdat_list *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      if (camptr->f >= 16 && camptr->f < 24)
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                           camptr->c,camptr->n,camptr->a,camptr->f,camptr->d);
       }
      else
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d\n",
                                     camptr->c,camptr->n,camptr->a,camptr->f);
       }
      camptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_crates(void)
{
   int  i,*iptr;
   struct tbl_index *tblptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CRATES;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Crate list:   %d Crates - ",tblptr->length);
   i = 0;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   while(i < tblptr->length)
     {
       lan_printf("%2i ",*iptr);
       iptr++;
       i++;
     }
   lan_printf("\n");
}
/****************************************************************************
****************************************************************************/
void pac_dmp_init(void)
{
   int  i;
   struct tbl_index *tblptr;
   struct cnafdat_list *camptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_INIT;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Init list:   %d NAFs\n",tblptr->length);
   camptr = (struct cnafdat_list *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      if (camptr->f >= 16 && camptr->f < 24)
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                           camptr->c,camptr->n,camptr->a,camptr->f,camptr->d);
       }
      else
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d\n",
                                     camptr->c,camptr->n,camptr->a,camptr->f);
       }
      camptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_uncond(void)
{
   int  i,count;
   struct tbl_index *tblptr;
   struct cnafdat_list *camptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_UNCOND_RO;
   count = tblptr->length;
   camptr = tmp_uncond;
   for (i=0; i < tblptr->length; i++) if ((camptr++)->f == 255) count--;
   if (count <= 0) return;
   lan_printf("\nCAMAC Unconditional Read list:   %d NAFs.  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   camptr = tmp_uncond;
   i = 1;
   while(i <= tblptr->length)
    {
      if (camptr->f == 255) ;
      else if (camptr->f >= 16 && camptr->f < 24)
        {
          lan_printf("-- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                             camptr->c,camptr->n,camptr->a,camptr->f,camptr->d);
        }
      else if (camptr->f < 8)
        {
          lan_printf("-- C%1d  N%2d  A%2d  F%2d  ID %4xh  %dd\n",
            camptr->c,camptr->n,camptr->a,camptr->f,camptr->d,camptr->d&0x7fff);
        }
      else
        {
          lan_printf("-- C%1d  N%2d  A%2d  F%2d\n",
                                       camptr->c,camptr->n,camptr->a,camptr->f);
        }
      camptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_windup(void)
{
   int  i;
   struct tbl_index *tblptr;
   struct cnafdat_list *camptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_WINDUP_CNAF;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Windup list:   %d NAFs\n",tblptr->length);
   camptr = (struct cnafdat_list *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      if (camptr->f >= 16 && camptr->f < 24)
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                           camptr->c,camptr->n,camptr->a,camptr->f,camptr->d);
       }
      else
       {
         lan_printf("-- C%1d  N%2d  A%2d  F%2d\n",
                                     camptr->c,camptr->n,camptr->a,camptr->f);
       }
      camptr++;
      i++;
    }
}
/****************************************************************************
*
*   CAUTION:  This routine should be called before pac_dmp_uncond!!
****************************************************************************/
void pac_dmp_latch(void)
{
   int  i,id,j,k;
   struct tbl_index *tblptr;
   struct cnaf_list *camptr,*uncon;
   struct cnafdat_list *uncond;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_LATCHES;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Latch list:   %d User Latches\n",tblptr->length);
   camptr = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   j = 2;
   while(i <= tblptr->length)
    {
      uncond = tmp_uncond;
      id = 0;
      for (k=0; k < uncond_cnt; k++)
         {
           uncon = (struct cnaf_list *)uncond;
           if (uncon->c == camptr->c && uncon->n == camptr->n &&
                                uncon->a == camptr->a && uncon->f == camptr->f)
             {
               uncond->f = 255;
               id = uncond->d;
               break;
             }
           uncond++;
         }
      if (id == 0) lan_printf("-- C%1d  N%2d  A%2d  F%2d  Latch #%d\n",
                                    camptr->c,camptr->n,camptr->a,camptr->f,j);
      else lan_printf("-- C%1d  N%2d  A%2d  F%2d  ID %4xh %dd  Latch #%d\n",
                     camptr->c,camptr->n,camptr->a,camptr->f,id,id & 0x7fff,j);
      camptr++;
      i++;
      j++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_cammod(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct mod_type *mptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_MODULES;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Module list:   %d Modules\n",tblptr->length);
   mptr = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = mptr->type_code;
      if (j == 0) j=1;
      else
        {
          j = j - PHIL_7164 + 2;
          if (j > sizeof(cam_modules)/sizeof(char *)) j = 0;
        }
      lan_printf("   C%1d  N%2d  Module = %s\n",mptr->c,mptr->n,
                                                              cam_modules[j]);
      mptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_xiamod(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct xia_list *mptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_XIA_LIST;
   if (tblptr->length <= 0) return;
   lan_printf("\nXIA Module list:   %d Modules\n",tblptr->length);
   mptr = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      lan_printf("   C%1d  N%2d  Vsn = %3d  Group = %2d\n",mptr->c,mptr->n,
                                                      mptr->vsn,mptr->group);
      mptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_feramod(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct mod_type *mptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_MODULES;
   if (tblptr->length <= 0) return;
   lan_printf("\nFERA Module list:   %d Modules\n",tblptr->length);
   mptr = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = mptr->type_code - LRS_4300 + 1;
      if (j < 0 || j > sizeof(fera_modules)/sizeof(char *)) j = 0;
      lan_printf("   C%1d  N%2d  Module = %s\n",mptr->c,mptr->n,
                                                             fera_modules[j]);
      mptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_fastmod(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct mod_type *mptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FASTBUS_MODULES;
   if (tblptr->length <= 0) return;
   lan_printf("\nFASTBUS Module list:   %d Modules\n",tblptr->length);
   mptr = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = mptr->type_code - LRS_1885 + 1;
      if (j < 0 || j > sizeof(fast_modules)/sizeof(char *)) j = 0;
      lan_printf("   C%1d  N%2d  Module = %s\n",mptr->c,mptr->n,
                                                            fast_modules[j]);
      mptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_cond(void)
{
   int  count,i,*ids,*id_list,naf_ctr;
   struct tbl_index *tblptr;
   struct cnaf_list *cnaf,*naf;
   struct cond_cam_pgm *cam;

#ifdef  OPT
   cam_cond_opt(&count);
   if (count <= 0) return;
   cnaf = CNAFnew;
   ids = IDnew;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   lan_printf("\nCAMAC Conditional Read list:   %d Tests.  Delay = %d\n",
                                                           count,tblptr->delay);
   cam = Cpgm;
#else
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_LIST;
   cnaf = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_ID_LIST;
   ids = (int *)( ACQ_RAM + tblptr->offset * 4);
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Conditional Read list:   %d Tests.  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   cam = (struct cond_cam_pgm *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
#endif
   i = 1;
   while(count)
     {
       lan_printf("Test #%i: Test Latch #%d, Mask %4xh\n",i,cam->lat,cam->mask);
       if (cam->tr_next == 0)
         {
           lan_printf("=== True.  %d NAFs.  End Test Pgm\n",cam->tr_num);
         }
       else
         {
           lan_printf("=== True.  %d NAFs.  Next Test #%i\n",
                                                   cam->tr_num,cam->tr_next);
         }
       naf_ctr = cam->tr_num;
       naf = cnaf + cam->tr_index - 1;
       id_list = ids + cam->tr_index -1;
       while(naf_ctr--)
         {
           if (naf->f < 8)
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d  ID %4xh  %dd\n",
                         naf->c,naf->n,naf->a,naf->f,*id_list,*id_list&0x7fff);
             }
            else if (naf->f >= 16 && naf->f < 24)
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                                         naf->c,naf->n,naf->a,naf->f,*id_list);
             }
            else
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d\n",
                                                  naf->c,naf->n,naf->a,naf->f);
             }
           naf++;
           id_list++;
         }
       if (cam->fa_next == 0)
         {
           lan_printf("=== False.  %d NAFs.  End Test Pgm\n",cam->fa_num);
         }
       else
         {
           lan_printf("=== False.  %d NAFs.  Next Test #%i\n",
                                                   cam->fa_num,cam->fa_next);
         }
       naf_ctr = cam->fa_num;
       naf = cnaf + cam->fa_index - 1;
       id_list = ids + cam->fa_index -1;
       while(naf_ctr--)
         {
           if (naf->f < 8)
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d  ID %4xh  %dd\n",
                         naf->c,naf->n,naf->a,naf->f,*id_list,*id_list&0x7fff);
             }
            else if (naf->f >= 16 && naf->f < 24)
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d  Data %4xh\n",
                                         naf->c,naf->n,naf->a,naf->f,*id_list);
             }
            else
             {
               lan_printf("   -- C%1d  N%2d  A%2d  F%2d\n",
                                                  naf->c,naf->n,naf->a,naf->f);
             }
           naf++;
           id_list++;
         }
       cam++;
       --count;
       i++;
     }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_fast_cond(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct cond_fast_pgm *fptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FASTBUS_RO;
   if (tblptr->length <= 0) return;
   lan_printf("\nFASTBUS Conditional Readout:   %d Tests  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   fptr = (struct cond_fast_pgm *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = fptr->type_code - LRS_1885 + 1;
      if (j < 0 || j > sizeof(fast_modules)/sizeof(char *)) j = 0;
      lan_printf("Latch #%i  Mask = %4xh  Module = %s\n",
                                         fptr->lat,fptr->mask,fast_modules[j]);
      fptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_fera_cond(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct cond_fera_pgm *fptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_RO;
   if (tblptr->length <= 0) return;
   lan_printf("\nFERA Conditional Readout:   %d Tests  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   fptr = (struct cond_fera_pgm *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = fptr->type_code - LRS_4300 + 1;
      if (j < 0 || j > sizeof(fera_modules)/sizeof(char *)) j = 0;
      lan_printf("Latch #%i  Mask = %4xh  Module = %s\n",
                                         fptr->lat,fptr->mask,fera_modules[j]);
      fptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_kill(void)
{
   int  i;
   struct tbl_index *tblptr;
   struct cond_kill *kptr;
   static char *ktype[] = {"NONE set","ANY set"};

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_KILL_LIST;
   if (tblptr->length <= 0) return;
   lan_printf("\nConditional Kill List:   %d Tests\n",tblptr->length);
   kptr = (struct cond_kill *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      lan_printf("Latch #%i  Mask = %4xh  Kill Type = %s\n",
                             kptr->lat,kptr->mask,ktype[kptr->type]);
      kptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_camac_cond(void)
{
   int  i,j;
   struct tbl_index *tblptr;
   struct cond_cam_special *fptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_RO;
   if (tblptr->length <= 0) return;
   lan_printf("\nCAMAC Special Readout:   %d Tests  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   fptr = (struct cond_cam_special *)( ACQ_RAM + tblptr->offset * 4);
   i = 1;
   while(i <= tblptr->length)
    {
      j = fptr->type_code;
      if (j == 0) j=1;
      else
        {
          j = j - PHIL_7164 + 2;
          if (j > sizeof(cam_modules)/sizeof(char *)) j = 0;
        }
      if (j < 0 || j > sizeof(cam_modules)/sizeof(char *)) j = 0;
      lan_printf("Latch #%i  Mask = %4xh  Module = %s\n",
                                         fptr->lat,fptr->mask,cam_modules[j]);
      fptr++;
      i++;
    }
}
/****************************************************************************
****************************************************************************/
void pac_dmp_gates(void)
{
   int  count,i,j,*cal_spec,*last,rdcnt;
   struct tbl_index *tblptr;
   struct gate_read_table *cnaf,*naf;
   struct raw_gate_spec *gate,*gspec1,*gspec2,*tmp_gate;
   static char *cmp[] = {""," .not. "};
   static char *opr[] = {" .and. "," .or. "};

/*
*   Sort raw gate spec in order of the gate read table
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_GATE_READ;
   cnaf = (struct gate_read_table *)( ACQ_RAM + tblptr->offset * 4);
   rdcnt = tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RAW_GATE_SPEC;
   tmp_gate = calloc(tblptr->length,sizeof(struct raw_gate_spec));
   gspec2 = tmp_gate;
   for (i=1; i <= rdcnt; i++)
     {
       gspec1 = (struct raw_gate_spec *)(ACQ_RAM + tblptr->offset * 4);
       for (j=0; j < tblptr->length; j++)
         {
           if (gspec1->rdindx == i) *gspec2++ = *gspec1;
           gspec1++;
         }
     }
   gate = tmp_gate;
   if (tblptr->length <= 0) return;
   lan_printf("\nRaw gate list:   %d Gates.  Delay = %d\n",
                                                 tblptr->length,tblptr->delay);
   count = tblptr->length;
   i = 1;
   while(count--)
     {
       naf = cnaf + gate->rdindx - 1;
       j = naf->type_code;
       if (j == 0) j=1;
       else
        {
          j = j - PHIL_7164 + 2;
          if (j > sizeof(cam_modules)/sizeof(char *)) j = 0;
        }
       if (j < 0 || j > sizeof(cam_modules)/sizeof(char *)) j = 0;
       lan_printf("Gate #%d:  Low = %d, High = %d, Pat #%d, Mask %xh\n",
                         i,gate->low,gate->high,gate->pat,gate->mask & 0xffff);
       lan_printf("            Read: C%1d  N%2d  A%2d  F%2d  Module = %s\n",
                                   naf->c,naf->n,naf->a,naf->f,cam_modules[j]);
       gate++;
       i++;
     }
   free(tmp_gate);
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_GATE_READ;
   cnaf = (struct gate_read_table *)( ACQ_RAM + tblptr->offset * 4);
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAL_GATE_SPEC;
   cal_spec = (int *)( ACQ_RAM + tblptr->offset * 4);
   if (tblptr->length == 0) return;
   last = cal_spec + tblptr->length;
   lan_printf("\nCalculated gate list:\n");
   while(cal_spec < last)
     {
       lan_printf("Gate #%d:  Pat #%d, Mask %xh\n",
                                        i,*cal_spec,*(cal_spec + 1) & 0xffff);
       cal_spec += 2;
       lan_printf("            %sGate #%d",cmp[*cal_spec],*(cal_spec+1));
       cal_spec += 2;
       if (*cal_spec != -1)
         {
           lan_printf("%s%sGate #%d",opr[*cal_spec],cmp[*(cal_spec+1)],
                                                                *(cal_spec+2));
           cal_spec += 3;
         }
       lan_printf("\n");
       cal_spec++;
       i++;
     }
}
/****************************************************************************
****************************************************************************/
void bus_error(int sr,int pc,int ea,int ssw)
{
   static void (*funcs[])(void) = {
             (void (*)(void))hex_display,
                             lrs_dmp,
                             ksc_dmp,
                             ces_dmp,
                             fera_dmp,
                             acro_dmp,
                             trig_dmp,
                             pac_dmp,
                             pac_dmp_crates,
                             pac_dmp_init,
                             pac_dmp_uncond,
                             pac_dmp_windup,
                             pac_dmp_latch,
                             pac_dmp_cammod,
                             pac_dmp_feramod,
                             pac_dmp_fastmod,
                             pac_dmp_cond,
                             pac_dmp_fast_cond,
                             pac_dmp_fera_cond,
                             pac_dmp_kill,
                             pac_dmp_camac_cond,
                             pac_dmp_gates,
             (void (*)(void))bus_error,
             (void (*)(void))((int)bus_error + 38912),
                             NULL };
   static char *mes[] = {
                          "???",
                          "CPU-40 memory examine/modify",
                          "LRS 1131 examine/modify",
                          "KSC 2917 examine/modify",
                          "CES 8170 examine/modify",
                          "LRS 1190 examine/modify",
                          "ACROMAG examine",
                          "ORNL Trigger examine",
                          "PAC dump",
                          "PAC dump Crates",
                          "PAC dump Init list",
                          "PAC dump Uncond RO",
                          "PAC dump Windup",
                          "PAC dump Latch",
                          "PAC dump CAMAC modules",
                          "PAC dump FERA modules",
                          "PAC dump FAST modules",
                          "PAC dump Cond RO",
                          "PAC dump FAST Cond",
                          "PAC dump FERA Cond",
                          "PAC dump Kill List",
                          "PAC dump CAMAC Cond",
                          "PAC dump Gates",
                          "C Library",
                          "?" };
   int  i = 0;

   while (funcs[i] != NULL)
     {
       if (pc < (int)funcs[i]) break;
/*
lan_printf("funcs[%i] = %x\n",i,(int)funcs[i]);
*/
       i++;
     }
   lan_printf("\nBus Error in %s\n",mes[i]);
   lan_printf("PC = %x, Fault Addr = %x, SR = %4.4x, Special Status = %4.4x\n",
                                                                 pc,ea,sr,ssw);
}
#ifdef  OPT
/****************************************************************************
*
*   Optimize the conditional CAMAC readout for our hardware
****************************************************************************/
int cam_cond_opt(int *cntr)
{
	  struct tbl_index *tblptr;
       struct cond_cam_pgm *cam,tmp_pgm;
	  struct cnaf_list *cnaf;
		      int  *ids, count;
                      int  camnew,pgmnew,cur,next;
                      int  i,index,num;
               static int testindex[500];
/*
*   Compute pointers to the the tables we use - COND_RO, CNAF_LIST
*   and ID_LIST.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_LIST;
   cnaf = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_ID_LIST;
   ids = (int *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   cam = (struct cond_cam_pgm *)( ACQ_RAM + tblptr->offset * 4);
/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   *cntr = 0;
   count = tblptr->length;
   if (count == 0) return(0);
   for (i=0; i < count; i++) Cpgm[i] = *cam++;

   camnew = pgmnew = 0;
   while(count)
    {
/*
*   Check for prior reference to this test
*/
      if (!prior_ref(pgmnew))
        {
/*
*    This test now has no prior references (NOTE that PAC does not
*    allow backward references).
*/
          Cpgm[pgmnew].tr_num = 0;
          Cpgm[pgmnew].fa_num = 0;
          Cpgm[pgmnew].tr_next = 0;
          Cpgm[pgmnew].fa_next = 0;
        }
      else
        {
/*
*   Follow the TRUE for this test until we find a "real" test or we
*   reach the end.  There are two dummy test cases:
*     1) test of latch #1 with mask = 0xffffffff (Always TRUE)
*     2) test of any latch with mask = 0  (Always FALSE)
*/
          if (Cpgm[pgmnew].lat == 1 && Cpgm[pgmnew].mask == -1)
            {
              Cpgm[pgmnew].fa_num = 0;
              Cpgm[pgmnew].fa_next = 0;
            }
          index = camnew + 1;
          for (i=0; i < Cpgm[pgmnew].tr_num; i++)
            {
              CNAFnew[camnew] = cnaf[Cpgm[pgmnew].tr_index + i -1];
              IDnew[camnew] = ids[Cpgm[pgmnew].tr_index + i -1];
              camnew++;
            } 
          Cpgm[pgmnew].tr_index = index;
          next = Cpgm[pgmnew].tr_next;
          while (next)
            {
              cur = next - 1;
              if (Cpgm[cur].mask > 0) break;
              if (Cpgm[cur].mask == -1)
                {
                  num = Cpgm[cur].tr_num;
                  index = Cpgm[cur].tr_index;
                  next = Cpgm[cur].tr_next;
                }
              else if (Cpgm[cur].mask == 0)
                {
                  num = Cpgm[cur].fa_num;
                  index = Cpgm[cur].fa_index;
                  next = Cpgm[cur].fa_next;
                }
              for (i=0; i < num; i++)
                {
                  CNAFnew[camnew] = cnaf[index + i -1];
                  IDnew[camnew] = ids[index + i -1];
                  camnew++;
                  Cpgm[pgmnew].tr_num++;
                }
              if (Cpgm[cur].tr_next == 0 && Cpgm[cur].fa_next == 0)
                {
                  next = 0;
                  break;
                }
            }
          Cpgm[pgmnew].tr_next = next;
/*
*   Follow the FALSE for this test until we find a "real" test or we
*   reach the end.
*/
          index = camnew + 1;
          for (i=0; i < Cpgm[pgmnew].fa_num; i++)
            {
              CNAFnew[camnew] = cnaf[Cpgm[pgmnew].fa_index + i -1];
              IDnew[camnew] = ids[Cpgm[pgmnew].fa_index + i -1];
              camnew++;
            } 
          Cpgm[pgmnew].fa_index = index;
          next = Cpgm[pgmnew].fa_next;
          while (next)
            {
              cur = next - 1;
              if (Cpgm[cur].mask > 0) break;
              if (Cpgm[cur].mask == -1)
                {
                  num = Cpgm[cur].tr_num;
                  index = Cpgm[cur].tr_index;
                  next = Cpgm[cur].tr_next;
                }
              else if (Cpgm[cur].mask == 0)
                {
                  num = Cpgm[cur].fa_num;
                  index = Cpgm[cur].fa_index;
                  next = Cpgm[cur].fa_next;
                }
              for (i=0; i < num; i++)
                {
                  CNAFnew[camnew] = cnaf[index + i -1];
                  IDnew[camnew] = ids[index + i -1];
                  camnew++;
                  Cpgm[pgmnew].fa_num++;
                }
              if (Cpgm[cur].tr_next == 0 && Cpgm[cur].fa_next == 0)
                {
                  next = 0;
                  break;
                }
            }
          Cpgm[pgmnew].fa_next = next;
        }
      count--;
      pgmnew++;
    }     
/*
*   Now rebuild the test array to eliminate dummy tests.
*/
   count = tblptr->length;
   index = 0;
   for (i=0; i < count; i++)
     {
       if (Cpgm[i].mask > 0 || (Cpgm[i].mask == -1 && Cpgm[i].tr_num > 0))
        {
          tmp_pgm = Cpgm[i];
          Cpgm[index] = tmp_pgm;
          testindex[i] = index + 1; 
          index++;
        }
     }
/*
*   Fix next test indices to match the new test array
*/
   count = index;
   for (i=0; i < count; i++)
     {
       Cpgm[i].tr_next = testindex[Cpgm[i].tr_next - 1];
       Cpgm[i].fa_next = testindex[Cpgm[i].fa_next - 1];
     }
/*
*  Return the number of tests in the new test array.
*/
   *cntr = index;
  return(1);
}
/****************************************************************************
*
****************************************************************************/
int prior_ref(int cur)
{
   int  i;

   if (cur == 0) return(1);
   for (i=0; i < cur; i++)
     {
       if (Cpgm[i].tr_next == (cur + 1)) return(1);
       if (Cpgm[i].fa_next == (cur + 1)) return(1);
     }
   return(0);
}
#endif
