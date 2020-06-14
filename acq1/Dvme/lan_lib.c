/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-1996
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
*    File:         /usr/users/mcsq/Dvme3/lan_lib.c
*
*    Description:  Basic user routines for Ethernet.  They hide most
*                  of the bloody details and provide packet buffers
*                  for receive and transmit.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/21/92    MCSQ         
*
*    5/ 7/93    MCSQ       Included herein what was in lan_io.c.  Namely
*                          the lan_printf and lan_scanf routines.  This
*                          makes mixing calls to lan_printf with other
*                          lan_* routines.  NOTE: This increases the
*                          size of task by about 6 Kbytes.  Also changed
*                          how the packet order byte is generated.  Previously,
*                          the packet order was taken from the received
*                          host packet.  However, this means only one
*                          packet may be sent for every packet received.
*                          Now we keep our own order counter which is
*                          incremented after each packet which requests an
*                          ACK.
*
*    2/18/95    MCSQ       Complete revision of functions lan_write and
*                          lan_reply.  Latest version of ULTRIX (V4.4 Rev 69)
*                          appears to occasionally lose packets from the VME
*                          system.  Guess it is time that I make the use
*                          Ack requests really work!
*
*    9/23/95    MCSQ       Second attempt at error recovery when the
*                          workstations loses packets.  At the  same time there
*                          are major changes to the workstation packet filter
*                          ethernet software.  The host ID/sequence table
*                          length increased from 4 to 8.  The transmit sequence
*                          is now in the host ID/sequence table.  When a new
*                          ID is entered into the table, the transmit sequence
*                          number is set equal to the receive sequence number.
*                          Thereafter, the transmit sequence is incremented with
*                          each transmission attempt.
*
*    2/19/96    MCSQ       Fix lan_printf so that it returns the correct
*                          number of bytes in the formated string.
*
*    3/22/96    MCSQ       Resync input and output sequence numbers when
*                          packets are lost by the VME system.
*****************************************************************************/

#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "system.h"
#include  "lan.h"
#include  "orph.h"

/*    Global variables      */

#define  SEQ_LEN  8

/*
*    Host ID/sequence number table
*/
   struct packet_seq {
        unsigned long  id;        /* Host id of caller                     */
        unsigned char  in_seq;    /* receive packet sequence number        */
        unsigned char  out_seq;   /* output packet sequence number         */
       };

static struct packet_seq pkt_seq[SEQ_LEN];
static struct packet_seq *seq_beg = &pkt_seq[0];
static struct packet_seq *seq_end = &pkt_seq[0] + SEQ_LEN;
static struct packet_seq *seq_ins = &pkt_seq[0];

int retrycount = 0;
int reack = 0;

static struct Ether_Packet in_pkt,out_pkt,ack_pkt;
static int  eno = 0, tmo, wait_desc;
static char *ack_ptr, *ptr_128;

#define  TMO  300    /* Initial transmit timeout in ticks.  100 ticks/second */
/*****************************************************************************
*
*   Open Ethernet channel
*
*  Call:     proto  -  Second byte of protocol word.  We will receive only
*                      packets for which the second protocol byte matches this.
*
*  Return:   out_buf - Pointer to data region of the transmit packet.
*            out_hdr - Pointer to the transmit header.
*******************************************************************************/
void lan_open(char proto,char **out_buf,struct Ether_Packet **out_hdr)
{

   eno = open("ln1",3);
   if (eno <= 0)
     {
       printf("Can't open device 'ln'\n");
       exit(1001);
     }
   ioctl(eno,EIOSPROTO,&proto);           /* Set receive protocol byte        */
   ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in the  */
   *out_hdr = &out_pkt;                   /* transmit header and return       */
   *out_buf = (char *)out_pkt.Data;       /* pointers to xmit header and buf  */
   out_pkt.Order = 0;
   log_phys_(0,128,&ack_ptr,&ptr_128,&wait_desc);
   ioctl(eno,EIOGACKADR,&ack_ptr);        /* Get address of Ack counter       */
   *ack_ptr++ = 0;
   *ack_ptr = 0;
}
/*****************************************************************************
*
*   Ethernet read.  This routine sets the timeout, if one is requested, and
*   calls the Ethernet driver task to get a received packet.  If a timeout
*   on the read request occurs, return a zero status.  Otherwise, return
*   the number of data bytes in the packet and a pointer to the first data
*   byte.  The host software may repeat sending a packet if the VME CPU
*   does not respond within the host's timeout.  This routine keeps a table
*   of host IDs and packet sequence numbers which enable it to reject
*   repeated packets.
*
*  Call:    timeout - If this is nonzero, it is the timeout for the read
*                     operation in 10 Millisecond ticks.
*
*  Return:  zero  - Means read timeout
*            >0   - Number of data bytes in packet
*
*           buf   -  Pointer to data in received packet.
*******************************************************************************/
int lan_read(int timeout,char **buf)
{
   int  size;
   struct packet_seq *seq_ptr;

/*
*   If the timeout is different from the previous call, set the new value.
*   Note that a value of zero is no timeout.
*/
   if (timeout != tmo)
     {
       ioctl(eno,EIOSTIMEOUT,&timeout);
       tmo = timeout;
     }
   while(1)
    {
/*
*   Go get a receive packet.
*/
      size = read(eno,(char *)&in_pkt,sizeof(struct Ether_Packet));
      if (size < 0)
        {
          printf("Read failure on device 'ln'\n");
          exit(1002);
        }
      *buf = (char *)in_pkt.Data;
      if (!size) return (0);		/*Timeout                          */

if (in_pkt.Ack == NAKPKT) continue;
/*
*   We have a packet - not a timeout - so check for a repeat
*   transmission by the host (i.e. a host timeout on receive).  First 
*   search for an ID in the table which matches the current packet.  Then
*   check for a match between the sequence number of the last received
*   packet and the sequence number of the current packet.
*/
      seq_ptr = seq_beg;
      while(seq_ptr < seq_end)
       {
         if (seq_ptr->id == in_pkt.Request_Number)
          {
           if (seq_ptr->in_seq != in_pkt.Order)
            {
/*
*   Resync input and output sequence numbers if we have lost packets.
*/
              if ((seq_ptr->in_seq + 1) != in_pkt.Order)
                                               seq_ptr->out_seq = in_pkt.Order;
              seq_ptr->in_seq = in_pkt.Order;
              return(size - ORPH_HDR_LEN);   /* Have a new packet.  */
            }
           else  break;
          }
         seq_ptr++;
       }
/*
*   If we get here, there are two possibilities. If seq_ptr is not equal
*   to seq_end,  this is a repeat of the previous packet. So just discard
*   it.  If, on the other hand, seq_ptr equals seq_end,  this is a packet
*   from a new host ID.  Therefore, we must enter the new ID and sequence
*   in our table.  The oldest entry is discarded.  SEQ_LEN specifies the
*   maximum number of host processes which can be check for repeated packets
*   simultaneously.  If the number of host processes exceeds this value, only
*   the ability to check for repeated packets is compromised.  NO packets
*   will be lost.
*/
      if (seq_ptr >= seq_end)
       {
         seq_ins->id = in_pkt.Request_Number;
         seq_ins->in_seq = in_pkt.Order;
         seq_ins->out_seq = in_pkt.Order;
         seq_ins++;
         if (seq_ins >= seq_end) seq_ins = seq_beg;
         return (size - ORPH_HDR_LEN);
       }
    }
}
/*****************************************************************************
*
*   Ethernet write.  CAUTION:  User must prepare the transmit packet header.
*                    The lan_open routine returns a pointer to the transmit
*                    packet header and puts our physical address in the
*                    Source field.  However, the user must provide the
*                    remainder of the header information.
*
*   Call:    size  -  Number of data bytes in packet.
*******************************************************************************/
void lan_write(int size)
{
   int  order,status,retry = 3, tmo = TMO;
   struct packet_seq *seq_ptr = seq_beg;
   struct chk_receive check;

/*
*   Get sequence number for the output packet
*/
   while(seq_ptr < seq_end)
    {
      if (seq_ptr->id == in_pkt.Request_Number)
        {
          order = seq_ptr->out_seq++;
          break;
        }
      seq_ptr++;
    }
   do
     {
       *ack_ptr = 0;
       out_pkt.Order = order;
       status = write(eno,(char *)&out_pkt,size + ORPH_HDR_LEN);
       if (status < 0)
         {
           printf("Write failure on device 'ln'\n");
           exit(1003);
         }
       if (out_pkt.Ack != ACK) break;
       dly_set_evt_(128,tmo);
       status = wait_phys_(wait_desc,ack_ptr,ptr_128);
       if (status)
         {
/*
*   We have a timeout while waiting for an ACKPKT from the host.  We will
*   retransmit the data packet.  First we try to determine if the host
*   lost our ACKPKT.  If the host lost our ACKPKT, it should have retransmitted
*   it's request.  Use the ioctl call to check for a retransmission by the 
*   host.  If so, first send a new ACKPKT an then retransmit the data.
*/
           retry--;
           tmo -= 100;
           if (tmo < 100) tmo = 100;
           retrycount++;
           check.Request_Number = in_pkt.Request_Number;
           check.Order = in_pkt.Order;
           check.Source = out_pkt.Destination;
           ioctl(eno,EIOCHKREC,&check);
           if (check.status)
             {
               memcpy(ack_pkt.Destination,out_pkt.Destination,ORPH_HDR_LEN);
               ack_pkt.Order = in_pkt.Order;
               ack_pkt.Ack = ACKPKT;
               status = write(eno,(char *)&ack_pkt,ORPH_HDR_LEN);
               if (status < 0)
                 {
                   printf("Write faliure on device 'ln'\n");
                   exit(1004);
                 }
             }
         }
       else  break;
     } while(retry);
   *ack_ptr = 0;
}   
/*****************************************************************************
*
*   Ethernet reply.  By definition, a reply is a response to a packet received
*   from the host.  All fields of the transmit packet header, except the Ack
*   field and the Source field, are copied from the receive packet header.
*   The source field was set in lan_open.  The data field must be supplied by
*   the user (lan_open routine returns a pointer to the data field).
*
*   Call:   size  -  Number of bytes in data field.
*           ack   -  If you want the host to acknowledge receipt of your
*                    packet, this should be ACK.  Otherwise, set it to NOACK.
*                    NOTE:  The user routine never sees acknowledgment 
*                    packets.  However, if an acknowledgment is requested,
*                    the driver must receive an acknowledgment packet before
*                    further packets can be read by the user.
*******************************************************************************/
void lan_reply(int size,char ack)
{
   int  order,status,retry = 3, tmo = TMO;
   struct packet_seq *seq_ptr = seq_beg;
   struct chk_receive check;

/*
*   Get sequence number for the output packet
*/
   while(seq_ptr < seq_end)
    {
      if (seq_ptr->id == in_pkt.Request_Number)
        {
          order = seq_ptr->out_seq++;
          break;
        }
      seq_ptr++;
    }
/*
*   Build the output header from data in the receive header.
*/
   memcpy(out_pkt.Destination,in_pkt.Source,HW_ADDR_LEN);
   memcpy(out_pkt.Protocol,in_pkt.Protocol,ORPH_HDR_LEN - 2*HW_ADDR_LEN);
   out_pkt.Order = order;
   out_pkt.Ack = ack;
   do
     {
       *ack_ptr = 0;
       status = write(eno,(char *)&out_pkt,size + ORPH_HDR_LEN);
       if (status < 0)
         {
           printf("Write failure on device 'ln'\n");
           exit(1005);
         }
       if (out_pkt.Ack != ACK) break;
       dly_set_evt_(128,tmo);
       status = wait_phys_(wait_desc,ack_ptr,ptr_128);
       if (status)
         {
/*
*   We have a timeout while waiting for an ACKPKT from the host.  We will
*   retransmit the data packet.  First we try to determine if the host
*   lost our ACKPKT.  If the host lost our ACKPKT, it should have retransmitted
*   it's request.  Use the ioctl call to check for a retransmission by the 
*   host.  If so, first send a new ACKPKT an then retransmit the data.
*/
           retry--;
           tmo -= 100;
           if (tmo < 100) tmo = 100;
           check.Request_Number = in_pkt.Request_Number;
           check.Order = in_pkt.Order;

/************
printf("Req_num = %x, Order = %x\n",check.Request_Number,check.Order);
************/

           check.Source = out_pkt.Destination;
           ioctl(eno,EIOCHKREC,&check);
           if (check.status)
             {
               memcpy(ack_pkt.Destination,out_pkt.Destination,ORPH_HDR_LEN);
               ack_pkt.Order = in_pkt.Order;
               ack_pkt.Ack = ACKPKT;
               status = write(eno,(char *)&ack_pkt,ORPH_HDR_LEN);
               if (status < 0)
                 {
                   printf("Write failure on device 'ln'\n");
                   exit(1006);
                 }

/************
printf("******************* ReAck ******************\n");
************/

               reack++;
             }
           retrycount++;
         }
       else   break;
     } while(retry);
   *ack_ptr = 0;
}
/*****************************************************************************
*
*   Ethernet close.  This disconnects the user from the ethernet driver.
*   Any additional received packets for this protocol will be discarded.
*
*******************************************************************************/
void lan_close(void)
{
   close(eno);
}
/*****************************************************************************
*
*
*******************************************************************************/
void lan_ioctl(int func,void *reply)
{
   ioctl(eno,func,reply);
}
/*****************************************************************************
*
*
*******************************************************************************/
int lan_printf(const char *format, ...) {
    va_list ap;
    int count,cnt;
    static int bytes;
    static char *cptr = (char *)out_pkt.Data;
    static char *buf;
    static struct Ether_Packet *hdr;

    va_start(ap,format);
    if (eno == 0)
      {
        lan_open(PROTO_SOFT,&buf,&hdr);
        memcpy(in_pkt.Destination,out_pkt.Source,6);
        memcpy(in_pkt.Source,*host_ether_address,6);
        in_pkt.Protocol[0] = PROTO_PREFIX;
        in_pkt.Protocol[1] = PROTO_SOFT;
      }
    cnt = vsprintf(cptr,format,ap);
    count = cnt;
    bytes = bytes + cnt;
    for(cnt -= 1; cnt >= 0; --cnt, ++cptr)
      {
        if ((*cptr != '\0' && *cptr < ' ') || bytes > 1320 )
          {
            lan_reply(bytes+1,ACK);
            cptr = (char *)out_pkt.Data;
            bytes = 0;
            break;
          }
      }
    va_end(ap);
    return(count);
}
/*****************************************************************************
*
*
*******************************************************************************/
int lan_scanf(const char *format, ...) {
    va_list ap;
    int cnt;
    char *str;
    static char *buf;
    static struct Ether_Packet *hdr;

    va_start(ap,format);
    if (eno == 0) lan_open(PROTO_SOFT,&buf,&hdr);
    lan_read(0,&str);
    cnt=vsscanf(str,format,ap);
    va_end(ap);
    return(cnt);
}
