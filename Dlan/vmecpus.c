/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1996-2004
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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Data Acquisition System
*
*    File:         /usr/users/mcsq/Dlinux/Dlan/vmecpus.c
*
*    Description:
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/20/96    MCSQ         
*
*    5/31/96    MCSQ      Now report CPUs which answer to an LT command.
*
*   10/ 3/96    MCSQ      Also report CPUs which are ready for boot
*
*    3/ 8/97    MCSQ      New for CPU-60s
*
*    7/24/98    MCSQ      Eliminate default VME processor.
*
*    3/15/03    MCSQ      Ported to Linux
*
*    5/19/04    MCSQ      Add message to report host name which is being
*                         checked for attached VME processors.
*****************************************************************************/
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <netpacket/packet.h>
#include <linux/types.h>
#include <linux/filter.h>

#include "orph_pf.h"
#include "../Dvme/mem_mgr.h"
#include "../Dvme/orphmsg.h"

/*
*     Function Prototypes
*/
void vme_inquire(void);
void bootable(void);
void pkt_send(void);
int  pkt_recv(unsigned char *,char);
int  pkt_open(char *,struct Packet_Header *,char );
int  boot_open(char *,struct Packet_Header *);

/*
*   Global data
*/
char *vcpus[] = {"vme1","vme2","vme3","vme4","vme5","vme6","vme7","vme8",
                 "vme9","vme10","vme11","vme12","vme20","vme21","vme22",
                 "vme23",NULL};
int  num_vme;

#define  MAX_VME  25

struct nodes {
               char name[12];
               char inter[6];
      unsigned char addr[HW_ADDR_LEN];
      unsigned char alive;
      unsigned char boot;
} cpus[MAX_VME];

char                 server[12] = "";
int                  pfd;                /* file handle set in pkt_open  */
struct Ether_Packet  xmitbuf;
unsigned char        source_addr[HW_ADDR_LEN];
char multiaddr[HW_ADDR_LEN] = MULTICAST;

/****************************************************************************
****************************************************************************/
main()
{
   int i,j;
   char *host = NULL;
   unsigned char *uptr;

   num_vme = 0;
   i = 0;
   while (vcpus[i] != NULL)
     {
       if ((uptr = (unsigned char *)en_name_to_addr(vcpus[i])) != NULL)
         {
           strcpy(cpus[num_vme].name,vcpus[i]);
           memcpy(cpus[num_vme].addr,uptr,HW_ADDR_LEN);
           strcpy(cpus[num_vme].inter,en_name_to_interf(vcpus[i]));
           num_vme++;
           if (num_vme >= MAX_VME) break;
         }
       i++;
     }
   printf("\nKnown VME CPUs are:\n");
   for (i=0; i < num_vme; i++)
     {
       if (cpus[i].name[0] == '\0') continue;
       printf("    %s  %s  %2.2x-",cpus[i].name,
                                              cpus[i].inter,cpus[i].addr[0]);
       for (j=1; j < 5; j++) printf("%2.2x-",cpus[i].addr[j]);
       printf("%2.2x\n",cpus[i].addr[5]);
     }
   if (num_vme) strcpy(server,cpus[0].name);
   fprintf(stderr,"\n Working... This will take about 15 seconds\n");
   host = getenv("HOST");
   if (host)
     {
       fprintf(stderr,"\n Checking for VME cpus attached to %s\n",host);
     }
   bootable();
   vme_inquire();
}
/****************************************************************************
*
*   The purpose here is to determine which, if any, VME processors are
*   alive and kicking.  To do this we send a request to each processor
*   for the list of tasks in memory(just the LT program).  All 
*   processors which respond are assumed to be up and running.
***************************************************************************/
void vme_inquire(void)
{
   int  first = 0,i,status;
/*
*   Open the packetfilter interface.
*   Note this returns an Ether header, to be passed on.
*/
   pfd = pkt_open(server, (struct Packet_Header *)&xmitbuf, (char)CODE);
   if (pfd == FAILURE)
     {
       fprintf(stderr,"Can't open device '%s'\n",server);
       exit (30);
     }
/*
*   Send an LT request of each processor
*/
   for (i=0; i < num_vme; i++)
     {
       cpus[i].alive = 0;
       memcpy(xmitbuf.Destination,cpus[i].addr,HW_ADDR_LEN);
       pkt_send();
     }

/*
*   Enable the timer and get packets from any VME processors
*   which answer the LT request.
*/
   memset(source_addr,0,sizeof(source_addr));
   do
     {
       status = pkt_recv(source_addr,(char)PROTO_CODE);
       for (i=0; i < num_vme; ++i)
        {
          if (!memcmp(source_addr,cpus[i].addr,HW_ADDR_LEN))
            {
              if (first == 0)
                {
                  printf("\nCPUs alive and kicking are:\n");
                  first++;
                }
              if (cpus[i].alive == 0)
                {
                  cpus[i].alive = 1;
                  printf("   %s\n",cpus[i].name);
                }
            }
        }
     } while(status != TIMEOUT);

   close(pfd);
}
/****************************************************************************
*  Check for CPUs which are sending a Request_Boot.
****************************************************************************/
void bootable(void)
{
   int  first = 0,i,status;
   int j;
/*
*   Open the packetfilter interface.
*   Note this returns an Ether header, to be passed on.
*/
   pfd = boot_open(server, (struct Packet_Header *)&xmitbuf);
   if (pfd == FAILURE)
     {
       fprintf(stderr,"Can't open device '%s'\n",server);
       exit (30);
     }
   sleep(6);
/*
*   See which CPUs are ready for boot
*/
   memset(source_addr,0,sizeof(source_addr));
   do
     {
       status = pkt_recv(source_addr,(char)PROTO_REQUEST_BOOT);
       for (i=0; i < num_vme; ++i)
        {
          if (!memcmp(source_addr,cpus[i].addr,HW_ADDR_LEN))
            {
              if (first == 0)
                {
                  printf("\nCPUs ready for boot are:\n");
                  first++;
                }
              if (cpus[i].boot == 0)
                {
                  cpus[i].boot = 1;
                  printf("   %s\n",cpus[i].name);
                }
            }
        }
     } while(status != TIMEOUT);

   close(pfd);
}
/****************************************************************************
*   Send a packet.
****************************************************************************/

void pkt_send (void)
{
     int  status,wr_len;
     short *buf = (short *)xmitbuf.Data;

     *buf = DISPLAY_MEM;
     wr_len = sizeof(struct Packet_Header);
     wr_len += 2;
     wr_len = (wr_len < MIN_PACKET) ? MIN_PACKET : wr_len;
     status = write(pfd,(unsigned char *)&xmitbuf,wr_len);
     if (status==FAILURE) {
        perror("vmecpus");
        fprintf(stderr,"vmecpus: Error sending request - exiting\n");
        exit(37);
     }
}
/****************************************************************************
*   Receive a packet and return source address.
******************************************************************************/
int pkt_recv(unsigned char *addr, char proto)
{
    int pkt_len;			/* Length of packet */
    int status = 0;
    fd_set readfds,errfds;
    struct timeval  timeout;
    static struct Ether_Packet recvbuf;
    struct VMEmsg *msg = (struct VMEmsg *)recvbuf.Data;

    /*   Read and wait for the reply  */

    do
     {
      FD_ZERO(&errfds);
      FD_ZERO(&readfds);
      FD_SET(pfd,&readfds);
      FD_SET(pfd,&errfds);

      timeout.tv_sec = 5;
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

      pkt_len = read(pfd,(unsigned char *) &recvbuf,sizeof(recvbuf));
      if (pkt_len < 0)
        {
          perror("read err");
          fprintf(stderr,"vmecpus: Error reading from Ethernet - exiting\n");
          exit (35);
        }
      if (msg->type == 4) continue;
/*
*  If the received packet requests an ACK, send one to make sender happy.
*/
      if (recvbuf.Ack == ACK)
        {
          memcpy(xmitbuf.Destination,recvbuf.Source,HW_ADDR_LEN);
          xmitbuf.Ack = ACKPKT;
          status = write(pfd,(unsigned char *)&xmitbuf,MIN_PACKET);
          if (status < 0) {perror("Send Ack"); return FAILURE;}
        }
       memcpy(addr,recvbuf.Source,HW_ADDR_LEN);
     } while(recvbuf.Protocol[0] != PROTO_PREFIX
                                              || recvbuf.Protocol[1] != proto);
    return(pkt_len);
}
/****************************************************************************
*
*   Special packet filter open routine for 'LT' packets.
***************************************************************************/
int pkt_open(char *dest, struct Packet_Header *pkt_hdr, char proto)
{
  int i,ifindex,status;
  char *ifname;
  struct sockaddr_ll  sll;
  struct ifreq ifrq;
  struct sock_fprog fprog;

  static int filter_len = 6;
  static struct sock_filter filter[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 3),
   BPF_STMT(BPF_LD+BPF_W+BPF_ABS, 16),
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1),
   BPF_STMT(BPF_RET+BPF_K, (u_int) -1),     /* accept packet  */
   BPF_STMT(BPF_RET+BPF_K, 0)               /* ignore packet  */
};

  if ((ifname = en_name_to_interf(dest)) == NULL) 
    {
      printf("vmecpus: Unknown %s\n",dest);
      return FAILURE;
    }
  pfd = socket(PF_PACKET,SOCK_RAW,htons(ETH_P_ALL));
  if (pfd == -1) {perror("socket error"); exit(1);}

/*
*   Must do bind for 'read' and 'write' to work
*/
  memset(&sll, 0, sizeof(sll));
  ifindex = if_nametoindex(ifname);
  sll.sll_family          = AF_PACKET;
  sll.sll_ifindex         = ifindex;
  sll.sll_protocol        = htons(ETH_P_ALL);

  if (bind(pfd, (struct sockaddr *) &sll, sizeof(sll)) == -1) {
                                          perror("bind err"); return FAILURE; }
/*
*   Build filter here
*/
  fprog.len = filter_len;
  fprog.filter = filter;
  filter[1].k = (PROTO_PREFIX << 8 ) + proto;
  filter[3].k = htonl(getpid());
  status = setsockopt(pfd,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
  if (status != 0){ perror("setsockopt"); return FAILURE; }

/*
*   Get "ifname" hardware address
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFHWADDR,&ifrq);
  if (status == -1) { perror("ioctl err"); return FAILURE; }
  memcpy(pkt_hdr->Source,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);

  memcpy(pkt_hdr->Destination,en_name_to_addr(dest),HW_ADDR_LEN);
  pkt_hdr->Protocol[0] = PROTO_PREFIX;
  pkt_hdr->Protocol[1] = proto;
  pkt_hdr->Ack = NOACK;
  pkt_hdr->Order = 0;
  pkt_hdr->Request_Number = getpid();

  return pfd; 
}
/****************************************************************************
*   Special packet filter open routine for Request_Boot packets.
****************************************************************************/
int boot_open(char *dest, struct Packet_Header *pkt_hdr)
{
  int i,ifindex,status;
  char *ifname;
  struct sockaddr_ll  sll;
  struct ifreq ifrq;
  struct packet_mreq mreq;
  struct sock_fprog fprog;

  static int filter_len = 4;
  static struct sock_filter filter[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1),
   BPF_STMT(BPF_RET+BPF_K, (u_int) -1),     /* accept packet  */
   BPF_STMT(BPF_RET+BPF_K, 0)               /* ignore packet  */
};

  if ((ifname = en_name_to_interf(dest)) == NULL)
    {
      printf("vmecpus: Unknown node %s\n",dest);
      return FAILURE;
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

  if (bind(pfd, (struct sockaddr *) &sll, sizeof(sll)) == -1) {
                                          perror("bind err"); return FAILURE; }
/*
*   Build filter here
*/
  fprog.len = filter_len;
  fprog.filter = filter;
  filter[1].k = (PROTO_PREFIX << 8) + PROTO_REQUEST_BOOT;
  status = setsockopt(pfd,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
  if (status != 0){ perror("setsockopt"); return FAILURE; }

/*
*   Setup receive of Multicast packet
*/
  memset(&mreq,0,sizeof(mreq));
  mreq.mr_ifindex = ifindex;
  mreq.mr_alen = HW_ADDR_LEN;
  memcpy(mreq.mr_address,multiaddr,HW_ADDR_LEN);
  mreq.mr_type = PACKET_MR_MULTICAST;
  setsockopt(pfd,SOL_PACKET,PACKET_ADD_MEMBERSHIP,&mreq,sizeof(mreq));
  if (status == -1){ perror("setsockopt"); return FAILURE; }
/*
*   Get "ifname" hardware address
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFHWADDR,&ifrq);
  if (status == -1){ perror("ioctl err"); return FAILURE; }
  memcpy(pkt_hdr->Source,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);

  memcpy(pkt_hdr->Destination,en_name_to_addr(dest),HW_ADDR_LEN);
  pkt_hdr->Protocol[0] = PROTO_PREFIX;
  pkt_hdr->Protocol[1] = PROTO_FORCE_BOOT;
  pkt_hdr->Ack = NOACK;
  pkt_hdr->Request_Number = getpid();

  return pfd;
}
