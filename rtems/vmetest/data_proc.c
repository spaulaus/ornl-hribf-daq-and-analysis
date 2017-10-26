/*****************************************************************************
*
*                            HHIRF COMPUTER GROUP
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
*    File:         /tera/mcsq/Drtems/Dvmetest/data_proc.c
*
*    Description:  Task to process data buffers from the data acquisition
*                  task and send then to the host.
*
*                  Special version for testvme.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/30/92    MCSQ         Original
*
*    9/ 3/92    MCSQ         Add word and byte swapping as conditional
*                            compilation.
*
*    3/28/93    MCSQ         This process now computes the average event
*                            size (in bytes) every 20 buffers.  This
*                            average + 12.5% is passed to the VMEacq
*                            process via the memory segment Acq_Params
*                            so that a better fit between buffer size
*                            and ethernet packet size can be maintained.
*
*    4/ 5/93    MCSQ         Remove byte swaping from this routine.  The
*                            required byte swap is now done in the ethernet
*                            driver - lance.c.   However, word swaping
*                            must still be done here.  Removed conditional
*                            compliation for SWAP.  We now accept, as a
*                            way of life, that the VME processor will forever
*                            do the required byte and word swaping.
*
*    9/16/93    MCSQ         Fix case for which there are two or more events
*                            and the event buffer is greater than one packet.
*                            First we remove the last event and send the
*                            packet.  If the last event is less or equal to
*                            one packet, just send it.  However, if the
*                            last event is larger than one packet, we have
*                            a special case.  This case is handled by moving
*                            last event to the top of the buffer so it
*                            can be handled by the code which processes one
*                            event buffers which are larger than one packet.
*
*    2/17/00    MCSQ         Change to include XIA readout.
*
*   11/03/03    MCSQ         Now include Acq_Params.h instead of XIA_Params.h
*
*   10/19/05    MCSQ         Version for vmetest.c .
*****************************************************************************/
#include <bsp.h>
#include <rtems.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>

#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include <sys/types.h>
#include <sys/socket.h> 
#include <netinet/in.h>

#include  "../include/vme_sys.h"
#include  "../include/Acq_Params.h"
#include  "debug.h"

/*  Function prototypes and global variables  */

static void send_data(struct data_buf *AcqBuf);
static unsigned int word_swap(unsigned short *,int );
static void byte_swap(unsigned char *,int );

extern char *ACQ_SHARED_RAM;
extern rtems_id Que1id;

extern rtems_id testVME_id;

static struct sockaddr_in cli_addr;
static int clilen,sockfd,seq = 0;

/****************************************************************************
****************************************************************************/
void data_proc(void)
{
                 int  status,size,*iptr;
     struct data_buf  *AcqBuf;
  struct acq_buf_hdr  HdrBuf;
 struct Packet_Header PktHdr;
     struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;
    rtems_status_code sc;
     uint32_t rtemsize = 4;

   sockfd = socket(AF_INET,SOCK_DGRAM,0);
   if (sockfd == -1) {perror("data_proc - socket error"); exit(1);}
   memset((char *)&cli_addr,0,sizeof(cli_addr));
   cli_addr.sin_family = AF_INET;
   cli_addr.sin_port = htons(45000+PROTO_DATA);
   cli_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&cli_addr,sizeof(cli_addr));
   if (status == -1) {perror("data_proc - bind"); exit(1);}

   clilen = sizeof(cli_addr);

   share->avg_param_size = AVG_PARAMS * 4;

   while(1)
    {
      sc = rtems_message_queue_receive(
         Que1id,
         &AcqBuf,
         &rtemsize,
         RTEMS_WAIT,
         RTEMS_NO_TIMEOUT);

      if (sc != RTEMS_SUCCESSFUL)
        {
          printf("message receive error\n");
          printf("%s\n",rtems_status_text(sc));
          exit(1);
        }

      else
        {
          AcqBuf->Bufhdr.ack = 0;
          HdrBuf = AcqBuf->Bufhdr;
          size = ((char *)AcqBuf->Bufhdr.end_buf
                                              - (char *)AcqBuf->Bufhdr.str_buf);
          if (size <= MAX_PKT_DATA)
            {
              if (size != 0)
                {
                  send_data(AcqBuf);
                  AcqBuf->Bufhdr.totalevents += AcqBuf->Bufhdr.events;
                  AcqBuf->Bufhdr.end_buf = AcqBuf->Bufhdr.str_buf;
                }
              else  send_data(AcqBuf);
              AcqBuf->Bufhdr.str_buf = HdrBuf.str_buf;
              AcqBuf->Bufhdr.busy = 0;
          sc = rtems_event_send(testVME_id,RTEMS_EVENT_0);
          if (sc != RTEMS_SUCCESSFUL)
           {
             printf("data_proc: send event failed\n");
             printf("%s\n",rtems_status_text(sc));
             exit(2000);
           }
              continue;
            }
          PktHdr = AcqBuf->Head;
          AcqBuf->Bufhdr.events = 0;
          iptr = (int *)AcqBuf->Bufhdr.str_buf;
          while (iptr < (int *)HdrBuf.end_buf)
            {
              if (*iptr++ == -1)
                {
                  AcqBuf->Bufhdr.events++;
                  size = ((char *)iptr - (char *)AcqBuf->Bufhdr.str_buf);
                  if (size < MAX_PKT_DATA)
                    {
                      AcqBuf->Bufhdr.last_event = (unsigned short *)iptr;
                      continue;
                    }
                  if (AcqBuf->Bufhdr.events > 1)
                    {
                      iptr = (int *)AcqBuf->Bufhdr.last_event;
                      AcqBuf->Bufhdr.events--;
                    }
                  AcqBuf->Bufhdr.end_buf = (unsigned short *)iptr;
                  send_data(AcqBuf);
                  AcqBuf->Head = PktHdr;
                  AcqBuf->Bufhdr.str_buf = AcqBuf->Bufhdr.end_buf;
                  AcqBuf->Bufhdr.end_buf = HdrBuf.end_buf;
                  AcqBuf->Bufhdr.totalevents += AcqBuf->Bufhdr.events;
                  AcqBuf->Bufhdr.events = 0;
                }
            }
          size = ((char *)AcqBuf->Bufhdr.end_buf
                                              - (char *)AcqBuf->Bufhdr.str_buf);
          if (size) send_data(AcqBuf);
          AcqBuf->Bufhdr.totalevents += AcqBuf->Bufhdr.events;
          AcqBuf->Bufhdr.str_buf = HdrBuf.str_buf;
          AcqBuf->Bufhdr.end_buf = HdrBuf.str_buf;
          AcqBuf->Bufhdr.busy = 0;

          sc = rtems_event_send(testVME_id,RTEMS_EVENT_0);
          if (sc != RTEMS_SUCCESSFUL)
           {
             printf("data_proc: send event failed\n");
             printf("%s\n",rtems_status_text(sc));
             exit(2000);
           }
        }
    }
}

/****************************************************************************
****************************************************************************/
static void send_data(struct data_buf *AcqBuf)
{
      int  size,status,bytes;
      int  cont_pkt;
    struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;
static int  bytesum = 0, evtcntr = 0, count = 0;
     char  *bufptr;

/*  process  data buffer from acquisition task    */

    size = ((char *)AcqBuf->Bufhdr.end_buf - (char *)AcqBuf->Bufhdr.str_buf);
    if (size == 0)
      {
/*
*   This special case is for the special packet which indicates that
*   data acquisition has stopped.
*/
        AcqBuf->TotalEvents = AcqBuf->Bufhdr.totalevents;
        AcqBuf->Events = 0;
        *(AcqBuf->Bufhdr.str_buf) = 0xffff;
        *(AcqBuf->Bufhdr.str_buf+1) = 0xffff;
        AcqBuf->Head.Sequence = seq++;
        AcqBuf->Head.DataSize = 4 + 8;
        word_swap((unsigned short*)&(AcqBuf->Head),6);
        byte_swap((unsigned char*)&(AcqBuf->Head),4 + PKTHDRLEN + 8);
        cli_addr = share->Host_Ether_Adr;
        cli_addr.sin_port = htons(45000+PROTO_DATA);
        status = sendto(sockfd,(char *)&AcqBuf->Head,4 + PKTHDRLEN + 8,0,
                        (struct sockaddr *)&cli_addr,clilen);
        if (status < 0)
          {
            perror("data_proc - error at sendto");
            exit(2003);
          }
        return;
      }
    bytesum += size;
    evtcntr += AcqBuf->Bufhdr.events;
    count++;
    if (count > 20)
      {

/*
*   Compute average event size in bytes.  1.125 times this average is
*   stored in shared RAM for the front-end acquisition code which uses
*   this to determine when to switch buffers.
*/
        count = 0;
        bytesum = bytesum/evtcntr;
        bytesum = bytesum + (bytesum >> 3);
        if (bytesum >= MAX_ORPH_DATA) share->avg_param_size = MAX_ORPH_DATA;
        else  share->avg_param_size = bytesum;
        bytesum = 0;
        evtcntr = 0;
      }
    if (size > MAX_PKT_DATA && AcqBuf->Bufhdr.events > 1)
      {
/*
*    Buffer is larger than one packet but there are multiple events.
*    Send as two or more packets.  The data acquisition task ensures that
*    there are no more than one extra event but the extra event can
*    be larger than a packet.
*/
        bytes = (AcqBuf->Bufhdr.last_event - AcqBuf->Bufhdr.str_buf)
                                                               * sizeof(short);
        /* Setup the buffer header for record keeping and transmission */
        AcqBuf->Events = AcqBuf->Bufhdr.events - 1;
        AcqBuf->TotalEvents = AcqBuf->Bufhdr.totalevents;
        AcqBuf->Bufhdr.totalevents += AcqBuf->Events;
        AcqBuf->Bufhdr.events = 1;
        /* Setup the packet header for copy */
        AcqBuf->Head.Sequence = seq++;
        AcqBuf->Head.DataSize = bytes + 8;
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - PKTHDRLEN - 8;
        memcpy(bufptr,&AcqBuf->Head,PKTHDRLEN+8);
        /* RLV 19 Jun2011 fix the following to use bufptr, not AcqBuf->Head */
        word_swap((unsigned short*)bufptr,6);
        byte_swap((unsigned char*)bufptr,bytes + PKTHDRLEN + 8);
        cli_addr = share->Host_Ether_Adr;
        cli_addr.sin_port = htons(45000+PROTO_DATA);
        status = sendto(sockfd,(char *)bufptr,bytes + PKTHDRLEN + 8,0,
                        (struct sockaddr *)&cli_addr,clilen);
        if (status < 0)
          {
            perror("data_proc - error at sendto");
            exit(2003);
          }

        AcqBuf->Bufhdr.str_buf = AcqBuf->Bufhdr.last_event;
        size = (AcqBuf->Bufhdr.end_buf - AcqBuf->Bufhdr.last_event)
                                                               * sizeof(short);
      }
    if (size <= MAX_PKT_DATA)
      {
/*
*    Buffer fits in one packet. Just send it.
*/
        AcqBuf->Events = AcqBuf->Bufhdr.events;
        AcqBuf->TotalEvents = AcqBuf->Bufhdr.totalevents;
        /* RLV moved the next two lines from just before wordswap to here */
        AcqBuf->Head.Sequence = seq++;
        AcqBuf->Head.DataSize = size + 8;
        /* Point to a place Header bytes before the data */
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - PKTHDRLEN - 8;
        /* Stuff the packet header into the reserved space before the data */
        memcpy(bufptr,&AcqBuf->Head,PKTHDRLEN+8);
        word_swap((unsigned short*)bufptr,6); /* the header is int */
        byte_swap((unsigned char*)bufptr,size + PKTHDRLEN + 8); /* data byte*/
        cli_addr = share->Host_Ether_Adr;
        cli_addr.sin_port = htons(45000+PROTO_DATA);
        /* Send the data from the correct pointer */
        status = sendto(sockfd,(char *)bufptr,size + PKTHDRLEN + 8,0,
                        (struct sockaddr *)&cli_addr,clilen);
/* Discovered by RLV 19 June 2011
*  This is wrong.  The argument for the swap and the sendto needs to be bufptr.

        word_swap((unsigned short*)&(AcqBuf->Head),6);
        byte_swap((unsigned char*)&(AcqBuf->Head),size + PKTHDRLEN + 8);
        cli_addr = share->Host_Ether_Adr;
        cli_addr.sin_port = htons(45000+PROTO_DATA);
        status = sendto(sockfd,(char *)&AcqBuf->Head,size + PKTHDRLEN + 8,0,
                        (struct sockaddr *)&cli_addr,clilen);
*/
        if (status < 0)
          {
            perror("data_proc - error at sendto");
            exit(2003);
          }

        return;
      }
    if (AcqBuf->Bufhdr.events == 1)
      {
        int sav_TotalEvents;
        unsigned short sav_Events = 0;
/*
*   This buffer is one large event. Break it into multiple packets
*   for transmission to host.
*/
        AcqBuf->Events = 0;
        AcqBuf->TotalEvents = AcqBuf->Bufhdr.totalevents;
        word_swap((unsigned short *)&(AcqBuf->TotalEvents),1);
        cont_pkt = 1;
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - PKTHDRLEN - 8;
        sav_TotalEvents = AcqBuf->TotalEvents;
        do
          {
            if (size > MAX_PKT_DATA) bytes = MAX_PKT_DATA;
            else  bytes = size;
            if (size == bytes) sav_Events = 1;
            memcpy(bufptr,&AcqBuf->Head,PKTHDRLEN);
            *((int *)(bufptr+PKTHDRLEN)) = sav_TotalEvents;
            *((unsigned short *)(bufptr+PKTHDRLEN+4)) = sav_Events;
            *((unsigned short *)(bufptr+PKTHDRLEN+6)) = cont_pkt;
            *((unsigned int *)(bufptr)) = seq++;
            *((int *)(bufptr+4)) = bytes+8;
            word_swap((unsigned short*)bufptr,4);
            byte_swap((unsigned char*)bufptr,bytes + PKTHDRLEN + 8);

            cli_addr = share->Host_Ether_Adr;
            cli_addr.sin_port = htons(45000+PROTO_DATA);
            status = sendto(sockfd,(char *)bufptr,bytes + PKTHDRLEN + 8,0,
                        (struct sockaddr *)&cli_addr,clilen);
            if (status < 0)
              {
                perror("data_proc - error at sendto");
                exit(2003);
              }

            ++cont_pkt;
            bufptr += bytes;
            size = size - bytes;
          }
        while(size > 0);
        AcqBuf->Cont = 0;
        return;
      }
/*
*    If we get here, something is bad Wrong!!
*/
    exit(0);
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
