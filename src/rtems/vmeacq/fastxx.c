/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1992-2005
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
*    File:         /usr/users/mcsq/Dvme3/fastxx.c
*
*    Description:  A VME processor task which provides host access
*                  to registers and data in the FASTBUS 1821
*                  sequencer.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/22/92    MCSQ         
*
*   12/29/92    MCSQ       The code devchk.c now builds a table of flags
*                          which indicate what hardware devices are available
*                          in the VME system.  Changed this code to use
*                          the flags for LRS1131 and LRS1821.  If either
*                          of these devices are not present,  Host I/O
*                          requests return and error.  Tis better than
*                          a bus error in the VME processor.
*
*   10/26/05    MCSQ       RTEMS version
*****************************************************************************/
#include "../include/vme_sys.h"
#include "../include/orph_udp.h"
#include "../include/lrs.h"
#include "../include/fastbus.h"
#include "../include/devices.h"

#include <bsp.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

//#define  MOD  0xe    /* 1821 device code  */
#define  MOD  0x0   /* 1821 device code  */

/*    Function Prototypes        */
static void lrs_init(void);
/*
*   External Functions
*/
extern void byte_swap(unsigned char *,int);
extern unsigned int  word_swap(unsigned short *,int);


/*      Global data              */
extern struct devices DEVTBL;

static struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
static unsigned short *lrs = (unsigned short *)LRS1;

void fastxx()
{
  static struct sockaddr_in cli_addr,serv_addr;
  int   sockfd;
  socklen_t   clilen;
  int    i,size,status;
  static struct UDP_Packet in_buf,out_buf;
  struct fast_buffer *cmds;
  struct fastd *fast;
  struct fast_return *rpy;
  rtems_interrupt_level level;

  lrs_init();

  sockfd = socket(AF_INET,SOCK_DGRAM,0);
  if (sockfd == -1) {perror("fastxx - socket error"); exit(1);}
  memset((char *) &serv_addr,0,sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(45000+PROTO_FASTBUS);
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
  if (status == -1) {perror("fastxx - bind"); exit(1);}

  clilen = sizeof(cli_addr);
  while (1)
   {
     size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                   (struct sockaddr *)&cli_addr,&clilen);
     if (size < 0) 
       { 
         printf("\nEthernet read error\n"); 
         exit(0);
       }
     cmds = (struct fast_buffer *)in_buf.Data;
     rpy = (struct fast_return *)out_buf.Data;
     byte_swap((unsigned char *)&cmds->count,2);
     i = cmds->count;
     if(i > 369)
       {
         printf("Packet data error: %x \n",i);
         i = 0; 
         cmds->count = 0;
       }
     fast = &cmds->fasts[0];
/*
*   If the LRS1131 or LRS1821 is not present, return an error code
*   to the host I/O request.
*/
     if (!DEVTBL.lrs1131 || !DEVTBL.lrs1821)
       {
         while(i)
          {
            rpy->Status = -1;
            rpy++;
            i--;
          }
       }
      while (i)
      {
        byte_swap((unsigned char *)&fast->Data,2);
        rtems_interrupt_disable(level);
        if (fast->F == 0)
          {
/*
*   Write function
*/
             *(lrs+SIB_DIRECT(MOD,fast->A)) = fast->Data;
             rpy->Data = fast->Data;
             eieio();
          }
        else
          {
/*
*    Read function
*/
             rpy->Data = *(lrs+SIB_DIRECT(MOD,fast->A));
             eieio();
           }
        rtems_interrupt_enable(level);
        rpy->Status = 0;
        rpy++;
        fast++;
        i--;
      }
     i = cmds->count * 4;
     if(i != 0)
       {
         out_buf.DataSize = i;
         word_swap((unsigned short *)&out_buf.DataSize,2);
         byte_swap((unsigned char *)&out_buf.DataSize,4);
         byte_swap((unsigned char *)&out_buf.Data,i);
         out_buf.Sequence = in_buf.Sequence;
         i = i + PKTHDRLEN;
         status=sendto(sockfd,&out_buf,i,0,(struct sockaddr *)&cli_addr,clilen);
         if (status < 0) {
            perror("fastxx - error at sendto");
            fprintf(stderr,"sockfd=%i, i=%i, clilen=%i\n",sockfd,i,(int)clilen);
         }
       }
   }
}
/****************************************************************************
*   Initialize LRS 1131 Module
*
*   Call:   No arguments
*   Return: None
*
*
****************************************************************************/
void lrs_init(void)
{
  unsigned short *lrs_mem = (unsigned short *)(LRS1 + LIST);
  int  i;
  rtems_interrupt_level level;


/*
*  If the LRS1131 is not present, just return.
*/
  if (!DEVTBL.lrs1131) return;
  rtems_interrupt_disable(level);
  lrs_cmd_ptr->csr = 0;
  lrs_cmd_ptr->nta = 0;
  lrs_cmd_ptr->wc = 0;
  lrs_cmd_ptr->sib = 0;
  eieio();
  for (i=0; i < 8192; i++) *lrs_mem++ = LIST_DONE;
  for (i=0; i < 8192; i++) *lrs_mem++ = 0xaa55;
  eieio();
  rtems_interrupt_enable(level);
  return;
}
