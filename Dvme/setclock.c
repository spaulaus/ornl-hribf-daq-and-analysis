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
*    File:         /usr/users/mcsq/Dlinux/Dvme/setclock.c
*
*    Description:  Routine to set the real time clock in a VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/15/96    MCSQ         Original
*
*    7/28/02    MCSQ         Adapted from Alpha version
*
*    3/19/03    MCSQ         Changed for new pkt_io.c
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "mem_mgr.h"

/*
*     Function Prototypes
*/
void set_time(void);
void mgr_error(int );

/*
*    Global variables
*/
char                 *prog;
int                  buflen;
unsigned char       *buf_ptr;
struct Vmed xbuf;
union Cmd *outbuf = (union Cmd *)xbuf.buf;

/***************************************************************************
*
***************************************************************************/
main(int argc, char *argv[])
{
   char *cptr;
   int size,status;

   prog = argv[0];
/*
*   Set clock time in VME processor.
*/
   set_time();
   return(0);
}
/****************************************************************************
*   Set clock time in the VME processor.
****************************************************************************/
void set_time(void)
{
  int status;

  char chour[4];
  time_t timex;
  struct tm *tptr;
  int  dst,lt_hr,utc_hr,size,tzone;

/*
*   The VME processor must have enough time to start the ethernet driver
*   before this message is sent.
*/

  timex = time(NULL);
  tptr = localtime(&timex);
  outbuf->time.yrs = tptr->tm_year;
  outbuf->time.dst = tptr->tm_isdst;
  outbuf->time.mon = tptr->tm_mon + 1;
  outbuf->time.day = tptr->tm_mday;
  outbuf->time.hrs = tptr->tm_hour;
  outbuf->time.min = tptr->tm_min;
  outbuf->time.sec = tptr->tm_sec;
  dst = tptr->tm_isdst;
  strftime(chour,3,"%H",tptr);
  lt_hr = atoi(chour);
  tptr = gmtime(&timex);
  strftime(chour,3,"%H",tptr);
  utc_hr = atoi(chour);
  tzone = 0;
  while ((lt_hr % 24) != utc_hr) { tzone++; lt_hr++; }
  if (dst != 0) tzone++;
  outbuf->time.tzone = tzone;
  outbuf->time.func = SET_TIME;
  xbuf.len = sizeof(struct Time);
  status = pkt_io(&xbuf,&xbuf,CODE,5);
  if (status != 0)
    {
      mgr_error(-status);
      exit(30);
    }
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
      case  ETHER_RECEIVE:
        printf("Ethernet receive timeout.\n");
        break;
      case  ETHER_TRANSMIT:
        printf("Ethernet transmit error.\n");
        break;
      case  ETHER_OPEN:
        printf("Ethernet open failure.\n");
        break;
      default:
        fprintf(stderr,"Unknown error code\n");
        break;
    }
}
