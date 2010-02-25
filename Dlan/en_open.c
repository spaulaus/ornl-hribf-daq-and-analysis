/*==================================================================
Revisions:
  11/29/92   MCSQ   Corrected the length of the input filter.
                    It was exactly 1 too large!

   4/16/93   MCSQ   It is important to ALWAYS set the filter
                    length even when the length is zero.  Failure
                    to do this can result in open failures or 
                    unpredictable results.  Changed this code to always
                    set the filter length.

   5/ 1/94   MCSQ   Add protocols for RMS control system.

   9/20/95   MCSQ   Puts call to our list/history initialization
                    routines at the end of this routine.  ALso
                    define storage for ReadQueue here.

   3/16/03   MCSQ   Ported to Linux

  en_open.c opens the packet filter device for read and write.
   It also examines the device characteristics and sets a filter
   for packets directed to this interface with protocol Protocol[].
  =================================================================== */
#include <sys/types.h> 
#include <stdio.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <string.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <netpacket/packet.h>
#include <linux/types.h>
#include <linux/filter.h>

#include "orph_pf.h"             /* local definitions for packet filter */

/*
*    Global storage
*/
struct ReadQueue en_ReadQueue;
struct Ether_Packet local_reci,local_xmit,local_ack;
unsigned char My_HW_Address[HW_ADDR_LEN];
   int en_timeout;

int en_open(char *dest,                 /* Ethernet dest node or net     */
        struct Packet_Header *pkt_hdr,  /* packet header built here      */
	enum server_proto server_name,  /* Server name - select protocol */
	unsigned int  Requestor)        /* PID field to watch            */
{

   int pfd;                 /* File descriptor of packet filter      */
   int Ack_fld;             /* Setting for ACK field in pkt_hdr      */
   int Proto_fcn;           /* Setting for Protocol field in pkt_hdr */
   unsigned char *dest_addr; /* Address of destination               */
   unsigned char *ifname;   /* Name of interface to use for destination */
   int i,j;                 /* Counter                               */
   int ifindex,status;
   int *iptr;
   short *sptr;

   struct sockaddr_ll  sll;
   struct ifreq ifrq;
   struct sock_fprog fprog;
   struct packet_mreq mreq;
   char multiaddr[] = MULTICAST;

   static int filter1_len = 8;
   static struct sock_filter filter1[] = {
     BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 5), /* Protocol               */
     BPF_STMT(BPF_LD+BPF_W+BPF_ABS, 6),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 3), /* High 32 bits of source */
     BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 10),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1), /* Low 16 bits of source  */
     BPF_STMT(BPF_RET+BPF_K, (u_int) -1),           /* accept packet          */
     BPF_STMT(BPF_RET+BPF_K, 0)                     /* ignore packet          */
};

   static int filter2_len = 10;
   static struct sock_filter filter2[] = {
     BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 7), /* Protocol               */
     BPF_STMT(BPF_LD+BPF_W+BPF_ABS, 6),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 5), /* High 32 bits of source */
     BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 10),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 3), /* Low 16 bits of source  */
     BPF_STMT(BPF_LD+BPF_W+BPF_ABS, 16),
     BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1), /* Request_Number         */
     BPF_STMT(BPF_RET+BPF_K, (u_int) -1),           /* accept packet          */
     BPF_STMT(BPF_RET+BPF_K, 0)                     /* ignore packet          */
};

/*
*     Get the device and node address for target node
*/
   if ((ifname=en_name_to_interf(dest)) == NULL)
     {
       printf("en_open: Unknown node %s \n", dest);
       return FAILURE;
     }
   dest_addr=en_name_to_addr(dest);     /* node address */
   iptr = (int *)dest_addr;
   sptr = (short *)&dest_addr[4];
   filter1[3].k = htonl(*iptr);
   filter2[3].k = htonl(*iptr);
   filter1[5].k = htons(*sptr);
   filter2[5].k = htons(*sptr);
   memcpy(pkt_hdr->Destination, dest_addr, HW_ADDR_LEN);

   pkt_hdr->Protocol[0] = PROTO_PREFIX;
/*
*   Each protocol has assigned to it these properties.  Assignment is made in
*   ORPH.H.  This is not as flexible as one might like, but it makes for a
*   simpler user interface since the decisions about reliability and such have
*   already been made.
*/
   switch (server_name)
    {
      case DATA:
           Proto_fcn = PROTO_DATA;
           Ack_fld   = NOACK;            /* No acknowledge needed */
           break;
      case CODE:
           Proto_fcn = PROTO_CODE;
           Ack_fld   = ACK;             /* Acknowledge required */
           break;
      case FASTBUS:
           Proto_fcn = PROTO_FASTBUS;
           Ack_fld   = ACK;
           break;
      case CNAF:
           Proto_fcn = PROTO_CNAF;
           Ack_fld   = ACK;
           break;
      case TEST:
           Proto_fcn = PROTO_TEST;
           Ack_fld   = NOACK;
           break;
      case SOFT:
           Proto_fcn = PROTO_SOFT;
           Ack_fld   = ACK;
           break;
      case FECNTRL:
           Proto_fcn = PROTO_FECNTRL;
           Ack_fld   = ACK;
           break;
      case FEMSG:
           Proto_fcn = PROTO_FEMSG;
           Ack_fld   = ACK;
           break;
      case VMEIO:
           Proto_fcn = PROTO_VMEIO;
           Ack_fld   = ACK;
           break;
      case LNFILL:
           Proto_fcn = PROTO_LNFILL;
           Ack_fld   = ACK;
           break;
      case RMSSIO:
           Proto_fcn = PROTO_RMSSIO;
           Ack_fld   = ACK;
           break;
      case FORCE_BOOT:
           Proto_fcn = PROTO_FORCE_BOOT;
           Ack_fld   = NOACK;
           Requestor = 0;
           break;

      default:
           printf("Unknown Protocol - exiting\n");
           return FAILURE;
           break;
    }
   pkt_hdr->Protocol[1] = Proto_fcn;
   pkt_hdr->Ack = Ack_fld;
   pkt_hdr->Request_Number = Requestor;
   filter2[7].k = htonl(Requestor);
   if (Proto_fcn == PROTO_FORCE_BOOT) Proto_fcn = PROTO_REQUEST_BOOT;
   filter1[1].k = (PROTO_PREFIX << 8) + Proto_fcn;
   filter2[1].k = (PROTO_PREFIX << 8) + Proto_fcn;
/*
*      Define the packet filter
*/
   if (Requestor == 0)
     {
      fprog.len = filter1_len;
      fprog.filter = filter1;
     }
   else
     {
      fprog.len = filter2_len;
      fprog.filter = filter2;
     }

   pfd = socket(PF_PACKET,SOCK_RAW,htons(ETH_P_ALL));
   if (pfd == -1) {perror("socket error"); return FAILURE;}

/*
*   Must do bind for 'read' and 'write'
*/
   ifindex = if_nametoindex(ifname);
   memset(&sll, 0, sizeof(sll));
   sll.sll_family          = AF_PACKET;
   sll.sll_ifindex         = ifindex;
   sll.sll_protocol        = htons(ETH_P_ALL);

   if (bind(pfd, (struct sockaddr *) &sll, sizeof(sll)) == -1)
                                          {perror("bind err"); return FAILURE;}

   status = setsockopt(pfd,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
   if (status != 0) {perror("setsockopt"); return FAILURE;}
/*
*   Setup receive of Multicast packet
*/
  if (Proto_fcn == PROTO_REQUEST_BOOT)
    {
      memset(&mreq,0,sizeof(mreq));
      mreq.mr_ifindex = ifindex;
      mreq.mr_alen = HW_ADDR_LEN;
      memcpy(mreq.mr_address,multiaddr,HW_ADDR_LEN);
      mreq.mr_type = PACKET_MR_MULTICAST;
      setsockopt(pfd,SOL_PACKET,PACKET_ADD_MEMBERSHIP,&mreq,sizeof(mreq));
      if (status == -1){ perror("setsockopt"); return FAILURE; }
    }
/*
*   Get "ifname" hardware address
*/
   memset(&ifrq,0,sizeof(ifrq));
   strcpy(ifrq.ifr_name,ifname);
   status = ioctl(pfd,SIOCGIFHWADDR,&ifrq);
   if (status == -1) {perror("ioctl err"); return FAILURE;}

   memcpy(pkt_hdr->Source,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);
   memcpy(My_HW_Address,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);

   en_InitRecord();
   en_InitHis();
   en_ReadQueue.pkt = NULL;
   en_ReadQueue.len = 0;
   en_timeout = 60;
   return pfd;
}
