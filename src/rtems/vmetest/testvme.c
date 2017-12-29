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
*    File:         /tera/mcsq/Drtems/Dvmetest/testvme.c
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
#include "../include/orph_udp.h"
#include "../include/Acq_Params.h"
#include "../include/acq_ctl.h"
#include "../include/orphmsg.h"
#include "debug.h"

extern struct devices DEVTBL;
extern rtems_id Que1id;
extern void *Que1buf;
extern char *ACQ_SHARED_RAM;

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

/*
*     Data buffers and pointers
*/
static unsigned short *Event;
static struct data_buf *AcqBuf;
static struct data_buf Buf1,Buf2;

static struct acq_share *share;
static char error_msg[256];

/*************************    Function Prototypes    ************************/

static void send_event(void);
static void buffer_setup(void);
static void acq_stop(void);
void host_message(int ,char *,char *);
static void byte_swap(unsigned char *,int);
static unsigned int  word_swap(unsigned short *,int);

/****************************************************************************
*
****************************************************************************/
void testvme(void)
{
   int  j,running = 0,count;
   unsigned short id;
   unsigned short dat = 0;
        int randat;

/*******************
#define  COUNT  0x7ffff;      634 evts/sec
#define  COUNT  0x7fff;       9908 evts/sec
#define  COUNT  0x7ff;        84403 evts/sec
*******************/

#define  COUNT  0x7fff;

   share = (struct acq_share *)&ACQ_SHARED_RAM[0];

   share->testrun = 0;
 while(1)
 {
   rtems_task_wake_after(100);
   buffer_setup();
   while (share->testrun)
    {
      count = COUNT;
      running = 1;
      id = 0x8001;
#ifndef RAN
      dat += 1;
#else
      randat = rand();
      dat = randat;
#endif
      dat = dat & 0xfff;
      for(j=0; j < 100; j++)
       {
         *Event++ = id++;
         *Event++ = dat;
       }
      *Event++ = 0xffff;
      *Event++ = 0xffff;
      send_event();
/*
*   Slowdown loop
*/
      for(j=0; j < count; j++);
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
static void send_event(void)
{
   int size;
   rtems_status_code sc;
   rtems_event_set event_rec;

/*
*   Increment event counters and then check for space for another event.
*/
   AcqBuf->Bufhdr.events += 1;
   size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
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
       if (Buf1.Bufhdr.ack != 0) while (Buf1.Bufhdr.ack != 0) rtems_task_wake_after(1);
       else while (Buf2.Bufhdr.ack != 0) rtems_task_wake_after(1);

     }
   AcqBuf->Bufhdr.ack = -1;
   AcqBuf->Bufhdr.busy = -1;
   Que1buf = AcqBuf;
   rtems_message_queue_send(Que1id, &Que1buf, 4);

   sc = rtems_event_receive(RTEMS_EVENT_0,
                            RTEMS_WAIT | RTEMS_EVENT_ALL,
                            0,
                            &event_rec);
   if (sc != RTEMS_SUCCESSFUL)
    {
      printf("testvme: receive_event failed\n");
      printf("%s\n",rtems_status_text(sc));
      exit(0);
    }

/*
*   Switch buffers for the acquisition process.
*/
   if (AcqBuf == &Buf1)
     {
/*
*   We were using Buf1, so switch to Buf2.
*/

       while (Buf2.Bufhdr.busy < 0) rtems_task_wake_after(1);
       Buf2.Bufhdr.events = 0;
       AcqBuf = &Buf2;
       Event = Buf2.Data;
     }
   else
     {
/*
*   We were using Buf2, so switch to Buf1.
*/

       while (Buf1.Bufhdr.busy < 0) rtems_task_wake_after(1);
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
static void buffer_setup(void)
{

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

   Buf1.Head.Sequence = 0;
   Buf1.Head.DataSize = 0;
   Buf2.Head.Sequence = 0;
   Buf2.Head.DataSize = 0;

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
static void acq_stop(void)
{
   int   size;
   rtems_event_set event_rec;
   rtems_status_code sc;


   size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
   if (size != 0)
     {
       AcqBuf->Bufhdr.totalevents = share->event_number;
       share->event_number += AcqBuf->Bufhdr.events;
       AcqBuf->Bufhdr.end_buf = Event;
       AcqBuf->Bufhdr.busy = -1;
       Que1buf = AcqBuf;
       while(1)
        {
          sc = rtems_message_queue_send(Que1id, &Que1buf, 4);
          if (sc == RTEMS_SUCCESSFUL) break;
          rtems_task_wake_after(1);
        }
       while(1)
        {
          rtems_task_wake_after(1);
          if (AcqBuf->Bufhdr.busy != -1) break;
        }
  
      }
/*
*   Send special stop acquisition buffer - zero length buffer.  This causes
*   special Ethernet packet to be sent to host.
*/

       AcqBuf->Bufhdr.totalevents = share->event_number;
       AcqBuf->Bufhdr.events = 0;
       AcqBuf->Bufhdr.busy = -1;
       AcqBuf->Bufhdr.end_buf = AcqBuf->Bufhdr.str_buf;

       Que1buf = AcqBuf;
       while(1)
         {
           sc = rtems_message_queue_send(Que1id, &Que1buf, 4);
           if (sc == RTEMS_SUCCESSFUL) break;
           rtems_task_wake_after(1);
         }
       while(AcqBuf->Bufhdr.busy != 0) rtems_task_wake_after(1);

       Event = AcqBuf->Bufhdr.str_buf;

/*
*   Clear RTEMS_EVENT_0
*/
       sc = rtems_event_receive(RTEMS_EVENT_0,
                                RTEMS_WAIT | RTEMS_EVENT_ALL,
                                0,
                                &event_rec);

/*
*   Send message to acquisition logger process on the host indicating the
*   event number at stop.
*/
   sprintf(error_msg,"Stop VME test. Test event number = %u",
                                                         share->event_number);
   host_message(INFORM,error_msg,"VMEtest ");
   return;
}
/****************************************************************************
*
*  Set a message to the Host.  If enabled, also output the message
*  to the local terminal attached to the VME processor.
****************************************************************************/
void host_message(int type,char *msg,char *pgm)
{
  static struct sockaddr_in cli_addr;
  static int clilen,seq = 0,sockfd = -1;
  int    i,status;
  static struct VMEmsg *host_msg;
  static struct UDP_Packet out_pkt;

  if(sockfd < 0)
    {
      sockfd = socket(AF_INET,SOCK_DGRAM,0);
      if (sockfd == -1) {perror("VMEacq - socket error"); exit(1);}
      clilen = sizeof(cli_addr);
      host_msg = (struct VMEmsg *)out_pkt.Data;
      i = sizeof(struct VMEmsg);
      out_pkt.DataSize = i;
      word_swap((unsigned short *)&out_pkt.DataSize,2);
      byte_swap((unsigned char *)&out_pkt.DataSize,4);
    }
   strcpy(host_msg->sender,pgm);
   host_msg->type = type;
   strcpy(host_msg->text,msg);
   cli_addr = share->Host_Ether_Adr;
   cli_addr.sin_port = htons(45000+PROTO_FEMSG);
   cli_addr.sin_family = AF_INET;
   out_pkt.Sequence = seq;
   word_swap((unsigned short *)&out_pkt.Sequence,2);
   byte_swap((unsigned char *)&out_pkt.Sequence,4);
   i = sizeof(struct VMEmsg);
   i = i + PKTHDRLEN;
   status=sendto(sockfd,&out_pkt,i,0,(struct sockaddr *)&cli_addr,clilen);
   if (status < 0) {
      perror("VMEacq - error at sendto");
   }

   rtems_task_wake_after(2);

#ifdef  LOCAL_MSG
   printf("%s\n",msg);
#endif
   printf("%s\n",msg);
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


