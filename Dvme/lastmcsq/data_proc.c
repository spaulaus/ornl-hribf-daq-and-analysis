/*****************************************************************************
*
*                            HHIRF COMPUTER GROUP
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2000
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
*    File:         /usr/users/mcsq/Dvme3/data_proc.c
*
*    Description:  Task to process data buffers from the data acquisition
*                  task and send then to the host.
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
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "system.h"
#include  "vme_sys.h"
#include  "vmeprom.h"
#include  "lan.h"
#include  "orph.h"
#include  "Acq_Params.h"

/*  Function prototypes and global variables  */

void send_data(struct data_buf *AcqBuf);

int  eno;

/****************************************************************************
****************************************************************************/
main(void)
{
                 int  task;
                 int  size,*iptr;
     struct data_buf  *AcqBuf;
  struct acq_buf_hdr  HdrBuf;
 struct Packet_Header PktHdr;
     struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;


   eno = open("ln1",3);

   task_priority_(-1,1,69);

   share->avg_param_size = AVG_PARAMS * 4;

   while(1)
    {
      wait_evts_(DATA_MSG,0);
      if (receive_ptr_(DATA_MSG_SLOT,&task,(char **)&AcqBuf))
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
        }
    }
}

/****************************************************************************
****************************************************************************/
void send_data(struct data_buf *AcqBuf)
{
    int  size,status,bytes;
    int  cont_pkt;
    struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;
static  int  bytesum = 0, evtcntr = 0, count = 0;


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
        word_swap_(&(AcqBuf->TotalEvents),1);
        AcqBuf->Events = 0;
        *(AcqBuf->Bufhdr.str_buf) = 0xffff;
        *(AcqBuf->Bufhdr.str_buf+1) = 0xffff;
        status = write(eno,(char *)&AcqBuf->Head,4 + ORPH_HDR_LEN + 8);
        if (status < 0)
          {
            printf("Write failure on device 'ln1'\n");
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
        AcqBuf->Events = AcqBuf->Bufhdr.events - 1;
        AcqBuf->TotalEvents = AcqBuf->Bufhdr.totalevents;
        AcqBuf->Bufhdr.totalevents += AcqBuf->Events;
        AcqBuf->Bufhdr.events = 1;
        word_swap_(&(AcqBuf->TotalEvents),1);
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - ORPH_HDR_LEN - 8;
        memcpy(bufptr,&AcqBuf->Head,ORPH_HDR_LEN+8);
        status = write(eno,bufptr,bytes + ORPH_HDR_LEN + 8);
        if (status < 0)
          {
            printf("Write failure on device 'ln1'\n");
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
        word_swap_(&(AcqBuf->TotalEvents),1);
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - ORPH_HDR_LEN - 8;
        memcpy(bufptr,&AcqBuf->Head,ORPH_HDR_LEN+8);
        status = write(eno,bufptr,size + ORPH_HDR_LEN + 8);
        if (status < 0)
          {
            printf("Write failure on device 'ln1'\n");
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
        word_swap_(&(AcqBuf->TotalEvents),1);
        cont_pkt = 1;
        bufptr = (char *)AcqBuf->Bufhdr.str_buf - ORPH_HDR_LEN - 8;
        sav_TotalEvents = AcqBuf->TotalEvents;
        do
          {
            if (size > MAX_PKT_DATA) bytes = MAX_PKT_DATA;
            else  bytes = size;
            if (size == bytes) sav_Events = 1;
            memcpy(bufptr,&AcqBuf->Head,ORPH_HDR_LEN);
            *((int *)(bufptr+ORPH_HDR_LEN)) = sav_TotalEvents;
            *((unsigned short *)(bufptr+ORPH_HDR_LEN+4)) = sav_Events;
            *((unsigned short *)(bufptr+ORPH_HDR_LEN+6)) = cont_pkt;
            status = write(eno,bufptr,bytes + ORPH_HDR_LEN + 8);
            if (status < 0)
              {
                printf("Write failure on device 'ln1'\n");
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
