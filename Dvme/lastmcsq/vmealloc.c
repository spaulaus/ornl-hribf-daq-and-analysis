/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1996-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmealloc.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/16/96    MCSQ         Original
*
*    7/27/02    MCSQ         Adapted for Linux
*
*    3/19/03    MCSQ         Changed for new pkt_io.c
*
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "vme_sys.h"
#include "mem_mgr.h"


#define  MAXC  257

/*    Function Prototypes       */
void mgr_error(int);

/*
*                      Global variables
*/
char                 server[12] = "vme";
char                 in_line[MAXC];      /* line buffer for file input       */
char                *prog;               /* pointer to name of this code     */
struct Vmed xbuf;


/****************************************************************************
*
*  Command line syntax:
*
****************************************************************************/
main(int argc, char *argv[])
{
   char  *cptr;
   int   zflag,i,len,size,status;
   union Cmd *outbuf = (union Cmd *)xbuf.buf;
   union Cmd *inbuf = (union Cmd *)xbuf.buf;

   prog = argv[0];
   if (argc < 3) {fprintf(stderr,"Name and size!\n"); exit(1);}
   sscanf(argv[2],"%i",&size);
   zflag = 0;
   if (argc == 4) sscanf(argv[3],"%i",&zflag);

   outbuf->alloc.func = ALLOC_MEM;
   outbuf->alloc.size = size;
   outbuf->alloc.zerof = zflag;
   strcpy(outbuf->alloc.name,argv[1]);
   xbuf.len = sizeof(struct Alloc);
   status = pkt_io(&xbuf,&xbuf,CODE,5);
   if (status != 0)
     {
       mgr_error(status);
       exit(50);
     }
   if (inbuf->reply.status != OK)
     {
       mgr_error(inbuf->reply.status);
       exit(50);
     }
   return(0);
}
/****************************************************************************
*   Report error from remote memory manager
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
        fprintf(stderr,"Unknown error code\n");
        break;
    }
}
