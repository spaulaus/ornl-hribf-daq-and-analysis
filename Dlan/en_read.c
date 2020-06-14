/*========================================================================
  en_read.c reads a packet from the Ethernet and sends an ACK packet in 
            response if required by the setting of the packet Ack byte.
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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include "orph_pf.h"             /* Local communications header */

extern int en_timeout;
extern int errno;
extern struct ReadQueue en_ReadQueue;
extern struct Ether_Packet local_reci,local_ack;
extern unsigned char My_HW_Address[HW_ADDR_LEN];

int en_read(int pfd,struct Packet_Header *pkt_hdr,unsigned char *buffer)
{
    struct Ether_Packet *local_pkt;
    int status;                    /* Status from reads         */
    int buflen;                    /* length of returned packet */

    while (1)
     {
        if (en_ReadQueue.pkt != NULL)
          {
            local_pkt = en_ReadQueue.pkt;
            buflen = en_ReadQueue.len;
            en_ReadQueue.pkt = NULL;
            break;
          }
        local_pkt = &local_reci;
        buflen=en_pkt_recv(pfd, local_pkt, sizeof(struct Ether_Packet));
        if (buflen < 0) return FAILURE;
        else if (buflen == TIMEOUT)
          {
#ifdef DEBUG
            en_dump_his_();
#endif
            return TIMEOUT;
          }
#ifdef  DEBUG
        en_AddHis(local_pkt,RECI_PKT);
#endif
        if (local_pkt->Ack==ACKPKT || local_pkt->Ack==NAKPKT) continue;
        else if (local_pkt->Ack==(char)ACK)
          {
            status=en_snd_acknak(pfd,(struct Packet_Header *)local_pkt,ACKPKT);
            if (status==FAILURE) return FAILURE;
            if (en_FindPacket(local_pkt)) continue;
          }
        break;
     }
/*
*     Separate the data from the header
*/
    buflen = buflen - ORPH_HDR_LEN;
    memcpy(pkt_hdr, local_pkt, ORPH_HDR_LEN);        /* separate header */
    memcpy(buffer, local_pkt->Data,(size_t)buflen);  /* separate data   */
    return buflen;
}
/*========================================================================
  en_snd_acknak.c reads a packet from the Ethernet and sends an ACK packet in 
            response if required by the setting of the packet Ack byte.
=========================================================================*/

int en_snd_acknak(int pfd, struct Packet_Header *pkt_hdr, int acktype)
{
    int status;

/*
*    Prepare the acknowledgement: send back with same 
*    protocol and request number
*/
    memcpy(local_ack.Destination, pkt_hdr->Source, HW_ADDR_LEN);
    memcpy(local_ack.Source, My_HW_Address, HW_ADDR_LEN);
    memcpy(local_ack.Protocol, pkt_hdr->Protocol, PROT_SIZE);
    local_ack.Request_Number = pkt_hdr->Request_Number;
    local_ack.Ack=acktype;
    local_ack.Order=pkt_hdr->Order;
/*
*     Send the packet.  We expect no reply
*/
    errno = 0;
    do
      {
        status=write(pfd, &local_ack, MIN_PACKET);
      } while(status == 0 && errno == EINTR);
    if (status==FAILURE) return FAILURE;
#ifdef  DEBUG
    en_AddHis(&local_ack,XMIT_PKT);
#endif
    return SUCCESS;
}
/****************************************************************************
*   Receive a packet and return source address.
******************************************************************************/
int en_pkt_recv(int pfd, struct Ether_Packet *recvbuf,int recvlen)
{
    int pkt_len;			/* Length of packet */
    int status = 0;
    fd_set readfds,errfds;
    struct timeval  timeout;

    /*   Read and wait for the reply  */

    do
     {
      FD_ZERO(&errfds);
      FD_ZERO(&readfds);
      FD_SET(pfd,&readfds);
      FD_SET(pfd,&errfds);

      timeout.tv_sec = en_timeout;
      timeout.tv_usec = 0;

      if ((status = select(4,&readfds,(fd_set *)0,&errfds,&timeout)) < 0)
                                                                 return FAILURE;
      if (status == 0) return TIMEOUT;
      if (FD_ISSET(pfd,&errfds))
        {
          return FAILURE;
        }
      if (!FD_ISSET(pfd,&readfds))
        {
          return FAILURE;
        }

/*    Read a packet from the Ethernet                              */

      pkt_len = read(pfd,(unsigned char *) recvbuf,recvlen);
      if (pkt_len < 0)
        {
          perror("read err");
          printf("en_read: Error reading from Ethernet - exiting\n");
          exit (35);
         }
     } while(recvbuf->Protocol[0] != PROTO_PREFIX);
    return(pkt_len);
}
