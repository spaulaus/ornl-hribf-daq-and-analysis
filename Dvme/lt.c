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
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/lt.c
*
*    Description:  List tasks/memory in the VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/ 1/92    MCSQ        Original
*
*   12/13/92    MCSQ       Change for new Ethernet library.
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    7/24/98    MCSQ        Eliminate the default VME processor.
*
*    7/26/02    MCSQ        Ported to Linux
*
*    3/19/03    MCSQ        Changed as required by pkt_io.c
*
*****************************************************************************/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "mem_mgr.h"

/*
*    Function Prototypes
*/
static void mgr_error(int );

/*
*    Global Variables
*/
struct Vmed xbuf;
char *prog;
static char server[12] = "vme";

/****************************************************************************
****************************************************************************/
main(int argc, char *argv[])
{
     char *cptr;
     int i,size,status;
     struct Display *cmd = (struct Display *)xbuf.buf;
     struct Reply *rpy = (struct Reply *)xbuf.buf;

     prog = argv[0];
     if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);

     cmd->func = DISPLAY_MEM;
     xbuf.len = sizeof(struct Display);
     status = pkt_open(&xbuf,&xbuf,CODE,5);
     status = pkt_send(&xbuf);
     status = pkt_recv(&xbuf);
     if (status != 0)
       {
         mgr_error(status);
         exit(50);
       }
     if (rpy->status != OK)
       {
         mgr_error(rpy->status);
         exit(50);
       }
     printf("\nVME Processor: %s\n",server);
     printf(" ID          Memory/Task    Size       Address");
     printf("      pc    evt1/evt2  pri/time\n");
     for(i=0; i < MEM_SEGS; i++)
       {
        if(cmd->tsk[i].name[0] == '\0') continue;
        printf("%3d",i);
        if (cmd->tsk[i].task >= 0x80)
                               printf("  T%2.2i  ",cmd->tsk[i].task - 0x80);
        else  printf("  M    ");
        printf("%16s",cmd->tsk[i].name);
        printf(" %4d",(cmd->tsk[i].end-cmd->tsk[i].start)/1024);
        printf("%8X%8X",cmd->tsk[i].start,cmd->tsk[i].end);
        if (cmd->tsk[i].task < 0x80) printf("\n");
        else
          {
            printf("%10X",cmd->tsk[i].pc);
            printf("%5d%5d",cmd->tsk[i].evt1,cmd->tsk[i].evt2);
            printf(" %4d %4d",cmd->tsk[i].priority,cmd->tsk[i].slice);
            if (cmd->tsk[i].evt1 || cmd->tsk[i].evt2) printf(" W\n");
            else  printf(" R\n");
          }
       }
     return(0);
}
/****************************************************************************
*
*  Report error code from the VME processor.
****************************************************************************/
void  mgr_error(int error)
{
   fprintf(stderr," %s error \7 - ",prog);
   switch (-error)
    {
      case  SRADRERR:
        fprintf(stderr,"S record address outside allocated memory\n");
        break;
      case  SRCHKSUM:
        fprintf(stderr,"S record checksum error\n");
        break;
      case  SRILLHEX:
        fprintf(stderr,"S record has nonhex character\n");
        break;
      case  SRCNTERR:
        fprintf(stderr,"S record byte count error\n");
        break;
      case  ILLINDEX:
        fprintf(stderr,"Illegal memory ID - must be in range 3 <= ID <= %d\n",MEM_SEGS);
        break;
      case  KILLERR:
        fprintf(stderr,"Request to kill nonexistent task\n");
        break;
      case  TSKMEM:
        fprintf(stderr,"Insufficient task memory allocated\n");
        break;
      case  ALLOCERR:
        fprintf(stderr,"Memory allocation table is full\n");
        break;
      case  ILLFUNC:
        fprintf(stderr,"Unrecognized command function code\n");
        break;
      case  PERMTSK:
        fprintf(stderr,"Request to kill a permamently resident task\n");
        break;
      case  NOMEMAVAIL:
        fprintf(stderr,"Not enough memory available\n");
        break;
      case  MEMDUPC:
        fprintf(stderr,"Duplicate memory/task name\n");
        break;
      case  ILLFREE:
        fprintf(stderr,"Free memory request error - size <= 0\n");
        break;
      case  -ETHER_RECEIVE:
        printf("Ethernet receive timeout.\n");
        break;
      case  -ETHER_TRANSMIT:
        printf("Ethernet transmit error.\n");
        break;
      case  -ETHER_OPEN:
        printf("Ethernet open failure.\n");
        break;
      default:
        fprintf(stderr,"Unknown error code: %i\n",error);
        break;
    }
}
