/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2005
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
*    File:         /tera/mcsq/Drtems/Dvmetest/VMEtest.c
*
*    Description:  Acquisition system test routine.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 1/92    MCSQ         
*
*   11/28/93    MCSQ       Moved the stop message to the process testvme.c
*
*   10/19/05    MCSQ       Version for rtems
*****************************************************************************/
#include <bsp.h>
#include <rtems.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <bsp/VME.h>
#include <libcpu/io.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "../include/vme_sys.h"
#include "../include/Acq_Params.h"
#include "../include/orph_udp.h"
#include "../include/orphmsg.h"
#include "../include/acq_ctl.h"
#include "../include/devices.h"

char ACQ_SHARED[sizeof(struct acq_share)];
char *ACQ_SHARED_RAM = ACQ_SHARED;
char ACQ_PARAMS[131072];

char *ACQ_RAM = ACQ_PARAMS;
int  ACQ_MAX_RAM = sizeof(ACQ_PARAMS);
/*
*    VME 100Hz clock
*/

       unsigned short clk100[2] = {0,0};

/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

int acq_init(void);
int acq_start(void);
int acq_stop(void);
static unsigned int word_swap(unsigned short *,int);
static void byte_swap(unsigned char *,int);
extern void host_message(int ,char *,char *);

static struct acq_share *share;
static int ACQ_initialized = 0;
static char error_msg[256];

void VMEacq(void)
{
   static struct sockaddr_in cli_addr,serv_addr;
   int   clilen,sockfd;
   static struct UDP_Packet in_buf,out_buf;
   int   len,status,size,run = 0;
   char  *inbuf,*outbuf;

   share = (struct acq_share *)ACQ_SHARED_RAM;

   sockfd = socket(AF_INET,SOCK_DGRAM,0);
   if (sockfd == -1) {perror("VMEtest - socket error"); exit(1);}
   memset((char *) &serv_addr,0,sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_port = htons(45000+PROTO_FECNTRL);
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
   if (status == -1) {perror("VMEtest - bind"); exit(1);}

   clilen = sizeof(cli_addr);

   share->acqrun = 0;
   share->FB_enabled = 0;
   share->FB_error = 0;
   share->KSC3982_enabled = 0;

   while(1)
    {
     size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                   (struct sockaddr *)&cli_addr,&clilen);
      if (size < 0)
        {
          fprintf(stderr,"\nVMEtest - Ethernet read error\n");
          exit(0);
        }
      inbuf = (char *)in_buf.Data;
      outbuf = (char *)out_buf.Data;

      *outbuf = *(outbuf+1) = 0;
      len = 2;
      switch (*inbuf)                   /* dispatch on function code       */
       {
         case  INIT_ACQ:          /* Build run-time tables from loaded PAC */
          share->Host_Ether_Adr = cli_addr;
          if (run == 0) *outbuf = acq_init();
          else  *outbuf = ACQ_INIT_RUN;
          break;
         case  START_ACQ:         /* Start data acquisition                */
          *outbuf = acq_start();
          if (*outbuf == ACQ_OK) run = 1;
          break;
         case  STOP_ACQ:          /* Stop data acquisition                 */
          *outbuf = acq_stop();
          if (*outbuf == ACQ_OK) run = 0;
          break;
         case  STATUS_ACQ:        /* Return acquisition status             */
          *outbuf = ACQ_OK;
          if (ACQ_initialized == 0)*(outbuf+1) = ACQ_UNINIT;
          else if (run) *(outbuf+1) = ACQ_RUN;
          else  *(outbuf+1) = ACQ_STOP;
          break;
         default:
          *outbuf = ACQ_UNKNOWN_COMMAND;
          break;
       }
/*
*     Send command response
*/
     out_buf.DataSize = len;
     word_swap((unsigned short *)&out_buf.DataSize,2);
     byte_swap((unsigned char *)&out_buf.DataSize,4);
     out_buf.Sequence = in_buf.Sequence;
     len = len + PKTHDRLEN;
     status=sendto(sockfd,&out_buf,len,0,(struct sockaddr *)&cli_addr,clilen);
     if (status < 0) {
       perror("VMEtest - error at sendto");
     }

    }
}
int acq_init(void)
{
  ACQ_initialized = 1;
  sprintf(error_msg,"VME test system initialized");
  host_message(INFORM,error_msg,"VMEtest ");
  return ACQ_OK;
}
int acq_start(void)
{

  if (ACQ_initialized == 0) return(ACQ_STR_NOINIT);

  share->testrun = 1;
  sprintf(error_msg,"Start VME test. Test event number = %u",
                                                         share->event_number);
  host_message(INFORM,error_msg,"VMEtest ");
  return ACQ_OK;
}
int acq_stop(void)
{

  share->testrun = 0;
  return ACQ_OK;
}

/****************************************************************************
****************************************************************************/
static unsigned int word_swap(unsigned short *buf,int count)
{
    register unsigned short tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return (*((int *)(buf - 2)));
}
/****************************************************************************
****************************************************************************/
static void byte_swap(unsigned char *buf,int count)
{
    register unsigned char tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return;
}

