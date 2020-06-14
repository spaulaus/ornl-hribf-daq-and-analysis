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
*    File:         /usr/users/mcsq/Dvme3/testvme.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 1/92    MCSQ         
*
*   10/17/93    MCSQ       Make first parameter 1 instead of 0.
*
*   11/28/93    MCSQ       Add function acq_stop which sends any partial
*                          buffer when the test is stopped.  Also sends
*                          the stop message to the host.  Previously this
*                          message was sent in VMEtest.c
*
*   12/ 4/93    MCSQ       Added compile option for ramdom data.
*
*    3/ 7/97    MCSQ       Added conditional for new CPU-60s.
*                          Measured event rates for timing loop parameters.
*****************************************************************************/
#include "system.h"
#include "vme_sys.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan_lib.h"
#include "lan.h"
#include "Acq_Params.h"
#include "acq_ctl.h"
#include "orphmsg.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

/*
*    If RAN is defined, generate random data.  Otherwise, you get
*    an incrementing pattern.
#define  RAN
*/
#define  RAN

/*
*     Data buffers and pointers
*/
unsigned short *Event;
struct data_buf *AcqBuf,*NewBuf;
struct data_buf Buf1,Buf2;

struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;
char error_msg[256];

/*************************    Function Prototypes    ************************/

void send_event(void);
void buffer_setup(void);
void acq_stop(void);
void host_message(int ,char *);
int acpran(int *);

/****************************************************************************
*
****************************************************************************/
main(void)
{
   int  j,running = 0;
   unsigned short id;
   unsigned short dat = 0;
   int seed = 1001;

   share->testrun = 0;
 while(1)
 {
   delay_(100);
   buffer_setup();
   while (share->testrun)
    {
      running = 1;
      id = 0x8001;
#ifndef RAN
      dat += 1;
#else
      dat = acpran(&seed);
#endif
      dat = dat & 0xfff;
      for(j=0; j < 64; j++)
       {
         *Event++ = id++;
         *Event++ = dat;
       }
/*
      *Event++ = 0x9000;
      *Event++ = share->event_number + AcqBuf->Bufhdr.events;
*/
      *Event++ = 0xffff;
      *Event++ = 0xffff;
      send_event();

#ifndef  CPU60

/*    CPU-40 delays
              **** 10 packets/sec
      delay_(1);
              **** 25 packets/sec  35 Kb/sec
      for(j=0; j < 20000; j++);
              **** 60 packets/sec  83 Kb/sec
      for(j=0; j < 8000; j++);
              **** 194 packets/sec 269 Kb/sec
      for(j=0; j < 2000; j++);
              **** 275 packets/sec 383 Kb/sec
      for(j=0; j < 1200; j++);
              **** 349 packets/sec 486 Kb/sec
      for(j=0; j < 800; j++);
              **** 400 packets/sec 557 Kb/sec
      for(j=0; j < 600; j++);
              **** 433 packets/sec 603 Kb/sec
      for(j=0; j < 500; j++);
              **** 473 packets/sec 658 Kb/sec
      for(j=0; j < 400; j++);
              **** 534 packets/sec 743 Kb/sec
      for(j=0; j < 200; j++);
*/
#else

/*    CPU-60 delays
              **** 10 packets/sec
      delay_(1);
              **** 95 packets/sec  132 Kb/sec
      for(j=0; j < 20000; j++);
              **** 208 packets/sec 289 Kb/sec
      for(j=0; j < 8000; j++);
              **** 259 packets/sec 360 Kb/sec
      for(j=0; j < 6000; j++);
              **** 296 packets/sec 411 Kb/sec
      for(j=0; j < 5000; j++);
              **** 344 packets/sec 479 Kb/sec
      for(j=0; j < 4000; j++);
              **** 510 packets/sec 711 Kb/sec
      for(j=0; j < 2000; j++);
*/

#endif
#ifndef  CPU60
   delay_(100);
      for(j=0; j < 2000; j++);
#else
   delay_(100);
      for(j=0; j < 8000; j++);
#endif
    }
   if (running != 0) acq_stop();
   running = 0;
 }
}
/****************************************************************************
*
*   Increment the event counters and check for event buffer full.  If the
*   buffer probably has room for another event,  just return.
*
*   If the buffer is greater than one packet or there is insufficient room
*   for another event,  send a message to the task which formats Ethernet
*   packets and sends them to the host.  Then switch to the next buffer and
*   return.
****************************************************************************/
void send_event(void)
{
   int size;

/*
*   Increment event counters and then check for space for another event.
*/
   AcqBuf->Bufhdr.events += 1;
   size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
/*
   if (size < 1390)
   if (size < MAX_PKT_DATA - AVG_PARAMS * 4)
*/
   if (size < MAX_PKT_DATA - share->avg_param_size)
     {
/*
*    There is more room in the inn
*/
       AcqBuf->Bufhdr.last_event = Event;
       return;
     }
/*
*   Send data buffer to Ethernet output task.
*/
   AcqBuf->Bufhdr.totalevents = share->event_number;
   share->event_number += AcqBuf->Bufhdr.events;
   AcqBuf->Bufhdr.end_buf = Event;
   if (Buf1.Bufhdr.ack != 0 || Buf2.Bufhdr.ack != 0)
     {
       if (Buf1.Bufhdr.ack != 0) wait_phys_(0x07,(char *)&Buf1.Bufhdr.ack,NULL);
       else  wait_phys_(0x07,(char *)&Buf2.Bufhdr.ack,NULL);
     }
   AcqBuf->Bufhdr.ack = -1;
   AcqBuf->Bufhdr.busy = -1;
   if (!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf))
     {
       printf("Send pointer failed\n");
       return;
     }
/*
*   Switch buffers for the acquisition process.
*/
   if (AcqBuf == &Buf1)
     {
/*
*   We were using Buf1, so switch to Buf2.
*/
       if (Buf2.Bufhdr.busy < 0)
         {
           wait_phys_(0x07,(char *)&Buf2.Bufhdr.busy,NULL);
         }
       Buf2.Bufhdr.events = 0;
       AcqBuf = &Buf2;
       Event = Buf2.Data;
     }
   else
     {
/*
*   We were using Buf2, so switch to Buf1.
*/
       if (Buf1.Bufhdr.busy < 0)
         {
           wait_phys_(0x07,(char *)&Buf2.Bufhdr.busy,NULL);
         }
       Buf1.Bufhdr.events = 0;
       AcqBuf = &Buf1;
       Event = Buf1.Data;
     }
}
/****************************************************************************
*
*   Initialize the event data buffers, the timer, and the interrupt
*   I/O moudle.  When finished, enable the command to start data
*   acquisition.
****************************************************************************/
void buffer_setup(void)
{
   int  task;
   char *cptr;

/*
*   Initialize data buffer headers
*/
   Buf1.Bufhdr.str_buf = Buf1.Data;
   Buf1.Bufhdr.end_buf = Buf1.Data;
   Buf1.Bufhdr.events = 0;
   Buf1.Bufhdr.busy = 0;
   Buf1.Bufhdr.ack = 0;
   Buf2.Bufhdr.str_buf = Buf2.Data;
   Buf2.Bufhdr.end_buf = Buf2.Data;
   Buf2.Bufhdr.events = 0;
   Buf2.Bufhdr.busy = 0;
   Buf2.Bufhdr.ack = 0;
  
/*
*   Initialize headers for Ethernet packets
*/
   memcpy(Buf1.Head.Source,*(our_ether_address),6);
   memcpy(Buf1.Head.Destination,share->Host_Ether_Adr,6);
   Buf1.Head.Protocol[0] = PROTO_PREFIX;
   Buf1.Head.Protocol[1] = PROTO_DATA;
   Buf1.Head.Ack = NOACK;
   memcpy(Buf2.Head.Source,*(our_ether_address),6);
   memcpy(Buf2.Head.Destination,share->Host_Ether_Adr,6);
   Buf2.Head.Protocol[0] = PROTO_PREFIX;
   Buf2.Head.Protocol[1] = PROTO_DATA;
   Buf2.Head.Ack = NOACK;
/*
*   Initialize timer
*/
   *((unsigned char *)TIM_CTL) = 0;        /* Stop timer                   */
   *((unsigned char *)TIM_PRELOAD) = 255;  /* Set preload register         */
   *((unsigned char *)TIM_ICTL) = 0;       /* Disable interrupts           */
/*
*   Initialize message pointer for data packets
*/
   receive_ptr_(DATA_MSG_SLOT,&task,&cptr);
   clr_evt_(DATA_MSG);
/*
*   Initialize buffer pointers
*/
   Event = Buf1.Data;
   AcqBuf = &Buf1;

   return;
}
/****************************************************************************
*
*   STOP Data Acquisition.
*
*   1) If acquisition is already stopped, just return error code.
*
*   2) Send remainder of the data, if any, to the data format/transmission
*      process.
*
*   3) Send a special buffer(zero length) to the data format/transmission
*      process.  This causes a special Ethernet packet to be sent to the
*      host indicating that acquisition is really stopped.
*
*   4) Send message to the host logger process showing the number of
*      next event.
****************************************************************************/
void acq_stop(void)
{
   int   size;

   size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
   if (size != 0)
     {
       AcqBuf->Bufhdr.totalevents = share->event_number;
       share->event_number += AcqBuf->Bufhdr.events;
       AcqBuf->Bufhdr.end_buf = Event;
       AcqBuf->Bufhdr.busy = -1;
       while(!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf))
                                                     wait_evts_(-DATA_MSG,0);
       wait_phys_(0x07,(char *)&(AcqBuf->Bufhdr.busy),NULL);
      }
/*
*   Send special stop acquisition buffer - zero length buffer.  This causes
*   special Ethernet packet to be sent to host.
*/
   AcqBuf->Bufhdr.totalevents = share->event_number;
   AcqBuf->Bufhdr.events = 0;
   AcqBuf->Bufhdr.busy = -1;
   AcqBuf->Bufhdr.end_buf = AcqBuf->Bufhdr.str_buf;
   while(!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf)) wait_evts_(-DATA_MSG,0);
   wait_phys_(0x07,(char *)&(AcqBuf->Bufhdr.busy),NULL);
   Event = AcqBuf->Bufhdr.str_buf;

/*
*   Send message to acquisition logger process on the host indicating the
*   event number at stop.
*/
   sprintf(error_msg,"Stop VME test. Test event number = %u",
                                                         share->event_number);
   host_message(INFORM,error_msg);
   return;
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

