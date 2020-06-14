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
*    File:         /usr/users/mcsq/Dlinux/Dvme/kt.c
*
*    Description:  Kill a task or delete memory segment in the VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/ 8/92    MCSQ        Original
*
*    7/26/02    MCSQ        Linux version
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "mem_mgr.h"

/*
*    Function Prototypes
*/
int  tsk_find(char *);
void mgr_error(int );

/*
*     Global variables
*/
char server[12] = "vme";
char *prog;
int mem_id;
struct Vmed xbuf,rbuf;
union Cmd *outbuf = (union Cmd *)xbuf.buf;
union Cmd *inbuf = (union Cmd *)rbuf.buf;

/****************************************************************************
*
*  Command syntax:
*
*     kt  memory_id
*  or
*     kt  memory_segment_name
*
*  If the command line parameter is a decimal integer, it is assumed to
*  be the memory_id of the segment or task to be deleted.  If the parameter
*  is a string, it is taken as the memory segment/task name.  Run 'lt'
*  to get the memory_id associated with a named task.
*
*  Examples:
*     kt 7
*     kt acq_sys
****************************************************************************/
main(int argc, char *argv[])
{
   char *cptr;
   int status;

   if (argc < 2) {fprintf(stderr,"Need memory ID!\n"); exit(1);}
   prog = argv[0];
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);
   xbuf.len = sizeof(struct Display);
   rbuf.len = sizeof(struct Display);

/*
*  Try converting command line parameter to an integer.  If it is an integer,
*  assume it is the memory_id.  Otherwise, assume task name.
*/
   mem_id = atoi(argv[1]);
   if (mem_id == 0)
     {
        mem_id = tsk_find(argv[1]);
        if (mem_id < 0)
          {
            fprintf(stderr,"Unknown memory/task name\n");
            return(0);
          }
     }
/*
*  First assume the memory_id points to a task and try to kill the task.
*  If this fails, the memory_id points to a memory segment.
*/
   outbuf->kill.func = KILL_TASK;
   outbuf->kill.mid = mem_id;
   status = pkt_io(&xbuf,&rbuf,CODE,5);
   if (status != 0)
     {
       mgr_error(status);
       exit(50);
     }
   if (inbuf->reply.status == OK) exit(0);
   else
    {
      outbuf->free.func = FREE_MEM;
      outbuf->free.mid = mem_id;
      status = pkt_io(&xbuf,&rbuf,CODE,5);
      if (status != 0)
        {
          mgr_error(status);
          exit(50);
        }
      if (inbuf->reply.status != 0) mgr_error(inbuf->reply.status);
    }
   return(0);
}
/****************************************************************************
*
*  If the command line parameter is a string, get the current memory
*  tables from the VME processor.  Then, search the table for a name
*  matching the one we wish to kill.  Return:
*     1) memory_id
* or  2) -1  to indicate name was not found.
****************************************************************************/
int tsk_find(char *cbuf)
{
   int  i,status;
   outbuf->display.func = DISPLAY_MEM;
   status = pkt_io(&xbuf,&rbuf,CODE,5);
   if (status != 0)
     {
       printf("Status = %i\n",status);
       exit(50);
     }
   if (inbuf->reply.status != OK) mgr_error(inbuf->reply.status);
   for(i=0; i < MEM_SEGS; i++)
     {
      if(!strcmp(inbuf->display.tsk[i].name,cbuf)) return(i);
     }
/*
*  Check for CPU-60 version of this code.
*/
   strcat(cbuf,"60");
   for(i=0; i < MEM_SEGS; i++)
     {
      if(!strcmp(inbuf->display.tsk[i].name,cbuf)) return(i);
     }
   return(-1);
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
        fprintf(stderr,"Unknown error code - %x\n",-error);
        break;
    }
}
