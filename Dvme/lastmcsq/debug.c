/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992,2003
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
*    File:         /usr/users/mcsq/Dlinux/Dvme/debug.c
*
*    Description:  Workstation code for use with the VME debug
*                  code 'vmebug.c'
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 8/92    MCSQ        Original
*
*   12/14/92    MCSQ        Changed to new Ethernet library.
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    3/18/03    MCSQ        Port to Linux
*****************************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"

/*   Global variables       */

char prog[] = "debug";

struct Vmed xbuf;
unsigned char *lan_buf = (unsigned char *)xbuf.buf;

/****************************************************************************
****************************************************************************/
main()
{
  char *cptr;
  int  status,size,sockpfd;
  static char *args[] = {"/usr/acq/vme/lt",
                          NULL,
                          NULL };
  fd_set readfds;

   xbuf.len = 128;
   sockpfd = pkt_open(&xbuf,&xbuf,SOFT,60);
   lan_buf[0] = '\n';
   lan_buf[1] = '\0';
   xbuf.len = 2;
   pkt_send(&xbuf);
   FD_ZERO(&readfds);
   while(1)
   {
     FD_SET(sockpfd,&readfds);
     FD_SET(0,&readfds);
     if ((status=select(4,&readfds,(fd_set *)0,(fd_set *)0,NULL)) < 0) exit(99);
if (status == 0) printf("Timeout\n");
     if (FD_ISSET(0,&readfds))
      {
      if(fgets(lan_buf,80,stdin) == NULL) exit(0);
       {
         size = strlen(lan_buf) + 1;
         if (!strncmp(lan_buf,"lt",2) || !strncmp(lan_buf,"LT",2))
           {
             if (fork() == 0) 
               {
                 execv(args[0],args);
                 perror("debug ");
                 exit(1);
               }
             wait(&status);
             lan_buf[0] = '\n';
             lan_buf[1] = '\0';
             size = 2;
           }
         if (!strncmp(lan_buf,"end",3) || !strncmp(lan_buf,"END",3)) exit(0);
         xbuf.len = size;
         pkt_send(&xbuf);
/***********
printf("Sent packet\n");
***********/
        }
      }
     if (FD_ISSET(sockpfd,&readfds))
      {
         status = pkt_recv(&xbuf);
/***********
printf("status = %i len = %i error = %i\n",status,xbuf.len,xbuf.error);
***********/
         if (status < 0)
           {
             printf("Ethernet communications error = %i\n",status);
             exit(99);
           }
         if (xbuf.len >4) write(1,(char *)lan_buf,strlen((char *)lan_buf));
      }
    }
   return(0);
}
