/*========================================================================
  en_write.c reliably writes a packet to the Ethernet.  By reliable, it
     waits for an ACK packet in response from the destination.  The write
     is retried upto MAX_RETRIES times before giving up.
=========================================================================*/
/*
*  revisions:
*
*   9/20/95   MCSQ    Major changes including adding the recording
*                     of all received and transmitted packets for
*                     debug.
*
*/
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "orph_pf.h"       /* Local communications header */

extern int errno;
extern struct ReadQueue en_ReadQueue;
extern struct Ether_Packet local_xmit,local_ack;
/*
*   Local function Prototype
*/
int en_get_ack(int, struct Packet_Header *);

int en_write(int pfd, struct Packet_Header *pkt_hdr, unsigned char *buffer,
                                                                    int buflen)
{
    int pktlen;
    int status;
    int i;
    static unsigned char pkt_count=0;

    en_ReadQueue.pkt = NULL;
/*
*      Insert the Sequence number and increment
*/
   pkt_hdr->Order = pkt_count++;
/*
*      Assemble the packet for sending
*/
   memcpy(&local_xmit, pkt_hdr, ORPH_HDR_LEN);
   memcpy(local_xmit.Data, buffer,(size_t)buflen);
   pktlen=ORPH_HDR_LEN + buflen;
   if (pktlen < MIN_PACKET) pktlen = MIN_PACKET;
   else if (pktlen > MAX_PACKET) pktlen = MAX_PACKET;
/*
*      Send the Packet
*/
   i = 0;
   while(i < MAX_RETRIES)
    {
      errno = 0;
      do
         {
           status=write(pfd, &local_xmit, pktlen);
         } while(status == 0 && errno == EINTR);
      if (status==FAILURE) return FAILURE;
#ifdef DEBUG
      en_AddHis(&local_xmit,XMIT_PKT);
#endif
      if (local_xmit.Ack != (char)ACK) return SUCCESS;
      status=en_get_ack(pfd, pkt_hdr);
      if (status == SUCCESS) return SUCCESS;
      i++;
    }
/*
*    If we arrive here, we received no Acknowledgement
*/
#ifdef  DEBUG
    en_dump_his_();
#endif
    return TIMEOUT;
}
/*========================================================================
  en_get_ack.c performs Acknowledgement receive and validation for
               en_write.
=========================================================================*/

int en_get_ack(int pfd, struct Packet_Header *pkt_hdr)
{
    int acklen,i,status;
    int timeout=ACK_TMO;
/*
*    Wait for successful acknowledgement
*/
    i = 0;
    while(i++ < MAX_RETRIES)
     {
       en_tmo(pfd, timeout);            /* Set a read timeout */
       acklen = en_pkt_recv(pfd, &local_ack, sizeof(struct Ether_Packet));
       if (acklen<0) return FAILURE;
       if (acklen==0) return TIMEOUT;
#ifdef DEBUG
       en_AddHis(&local_ack,RECI_PKT);
#endif
/*
*    Received an Ethernet packet - Validate it
*/
       if (local_ack.Ack == ACKPKT)
         {
           if (local_ack.Request_Number != pkt_hdr->Request_Number) continue;
           else if (memcmp(local_ack.Source, 
                            pkt_hdr->Destination, HW_ADDR_LEN)!=0) continue; 
           else if (local_ack.Order != pkt_hdr->Order) continue;
           else  return SUCCESS;
         }     
       else if (local_ack.Ack == NAKPKT) return RETRY;
       else if (local_ack.Ack == (char)ACK)
         {
           status = en_snd_acknak(pfd,(struct Packet_Header *)&local_ack,
                                                                        ACKPKT);
           if (status == FAILURE) return FAILURE;
           if (en_FindPacket(&local_ack)) continue;
         }
       en_ReadQueue.pkt = &local_ack;
       en_ReadQueue.len = acklen;
       return SUCCESS;
     }
    return TIMEOUT;
}
