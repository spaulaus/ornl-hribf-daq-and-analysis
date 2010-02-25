/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992,1993
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
*    File:         /usr/users/mcsq/Dvme3/VMEtest.c
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
*****************************************************************************/
#include "system.h"
#include "vme_sys.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan_lib.h"
#include "lan.h"
#include "Acq_Params.h"
#include "orphmsg.h"
#include "acq_ctl.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int acq_init(void);
int acq_start(void);
int acq_stop(void);
void host_message(int ,char *);
/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;
int ACQ_initialized = 0;
char error_msg[256];

main(void)
{
   struct Ether_Packet *out_hdr,*in_hdr;
   int   run = 0;
   char  *inbuf,*outbuf;

   lan_open(PROTO_FECNTRL,&outbuf,&out_hdr);

   while(1)
    {
      lan_read(0,&inbuf);         /* get a command packet from host  */

      *outbuf = *(outbuf+1) = 0;
      switch (*inbuf)                   /* dispatch on function code       */
       {
         case  INIT_ACQ:          /* Build run-time tables from loaded PAC */
          in_hdr = (struct Ether_Packet *)(inbuf - ORPH_HDR_LEN);
          memcpy(share->Host_Ether_Adr,in_hdr->Source,6);
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
     lan_reply(2,NOACK);            /* Send command response                */
    }
}
int acq_init(void)
{
  ACQ_initialized = 1;
  sprintf(error_msg,"VME test system initialized");
  host_message(INFORM,error_msg);
  return ACQ_OK;
}
int acq_start(void)
{

  if (ACQ_initialized == 0) return(ACQ_STR_NOINIT);
  share->testrun = 1;
  sprintf(error_msg,"Start VME test. Test event number = %u",
                                                         share->event_number);
  host_message(INFORM,error_msg);
  return ACQ_OK;
}
int acq_stop(void)
{

  share->testrun = 0;
  return ACQ_OK;
}
/****************************************************************************
*
*  Set a message to the Host.  If enabled, also output the message
*  to the local terminal attached to the VME processor.
****************************************************************************/
void host_message(int type,char *msg)
{
  int  status;
  static struct VMEmsg *host_msg;
  static int  eno;
  static struct Ether_Packet out_pkt;

  if(!eno)
    {
      eno = open("ln1",3);
      if (eno <= 0)
        {
          printf("Can't open device 'ln'\n");
          exit(1001);
        }
      ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in
                                                the packet header           */
      memcpy((char *)out_pkt.Destination,share->Host_Ether_Adr,6);
      out_pkt.Order = 0;
      out_pkt.Protocol[0] = PROTO_PREFIX;
      out_pkt.Protocol[1] = PROTO_FEMSG;
      out_pkt.Ack = NOACK;
      host_msg = (struct VMEmsg *)out_pkt.Data;
      strcpy(host_msg->sender,"VMEacq  ");
    }
   host_msg->type = type;
   strcpy(host_msg->text,msg);
   status = write(eno,(char *)&out_pkt,sizeof(struct VMEmsg) + ORPH_HDR_LEN);
   if (status < 0)
     {
       printf("Write failure on device 'ln'\n");
       exit(1003);
     }

#ifdef  LOCAL_MSG
   printf("%s\n",msg);
#endif
}

