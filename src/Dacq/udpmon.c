/*-----------------------------------------------------------------
 ether_listen.c 
 A program to listen on an Ethernet interface for packets.  It
 uses the promiscuous mode of the packet filter interface.
 R. Varner, based on "packetfilter(4)" document,

 fix for use as "dmon" with Rtems system. 3/8/06

 Linux version 3/12/03  MCSQ
*/


#include <sys/types.h>		/* u_ types defined */
#include <sys/file.h>		/* Seems like a good idea... */
#include <sys/ioctl.h>		/* ioctl header */
#include <sys/time.h>		/* timeval structure */
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

#include "../include/orph_udp.h"

#define HW_ADDR_LEN      6    /* Length of hardware address in bytes    */
#define SUCCESS          1
#define FAILURE         -1
#define TIMEOUT          0

/*
 *        Function prototypes
 */
void open_ether_listen (char *, int);
int  read_ether (void);
void INTsignal(int);

/*
 *        Global variables
 */
static int pfd;
static unsigned char wksaddr[HW_ADDR_LEN];


/*------------------------------------------------------------------*/
int main (int argc, char *argv[])
{
  char ifname[8] = "eth1";
  char iproto[8];
  int status;			/* status return from functions     */
  char *cptr;
  long proto = 0x0800;
  int  i,len;

  open_ether_listen (ifname, proto);
  if (pfd == FAILURE)
    {
      printf ("Error opening Ethernet interface\n");
      return(FAILURE);
    }
  setuid(getuid());

  signal(SIGINT,INTsignal);
  while(1)
    {
      status = read_ether();
      if (status == FAILURE) printf ("Error in Ether Read.  Continuing...\n");
      else if (status == TIMEOUT) {printf("Timeout\n");  continue;}
    }
}
/******************************************************************************
*    Open packet filter for network listening

   open_ether_listen opens the packet filter device for read.
 * Syntax:
 * open_ether_listen(ifname, proto)
 *
 * status - return status (=-1 if problem)
 * ifname (passed) - string with name of interface, e.g. "eth0"
******************************************************************************/

void open_ether_listen (char *ifname, int proto)
{
  int i,ifindex,status;
  struct sockaddr_ll  sll;
  struct ifreq ifrq;
  struct packet_mreq mreq;
  struct sock_fprog fprog;

/*
*   If a protocol specified on command line, use this filter.
*/
  static int filter1_len = 6;
  static struct sock_filter filter1[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),      /* Get Protocol word from packet */
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 3), /* 0x0000 changed on build */
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 36),      /* Get Dest port from packet */
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1), /* 0x0000 changed on build */
   BPF_STMT(BPF_RET+BPF_K, (u_int) -1),     /* accept packet  */
   BPF_STMT(BPF_RET+BPF_K, 0)               /* ignore packet  */
};

  pfd = socket(PF_PACKET,SOCK_RAW,htons(ETH_P_ALL));
  if (pfd == -1) {perror("socket error"); exit(1);}

/*
*   Must do bind for 'read' and 'write' to work
*/
  ifindex = if_nametoindex(ifname);
  memset(&sll, 0, sizeof(sll));
  sll.sll_family          = AF_PACKET;
  sll.sll_ifindex         = ifindex;
  sll.sll_protocol        = htons(ETH_P_ALL);

  if (bind(pfd, (struct sockaddr *) &sll, sizeof(sll)) == -1)
                                                            perror("bind err");

/*
*   Get "ifname" hardware address
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFHWADDR,&ifrq);
  if (status == -1) perror("ioctl err");

/*
*   Get workstation hardware address.
*/
  memcpy(wksaddr,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);
  printf ("Device H/W address: %2.2x", wksaddr[0]);
  for (i = 1; i < HW_ADDR_LEN; i++) printf(":%2.2x",wksaddr[i]);
  printf ("\n");
/*
*   Get interface flags.  Check for interface UP.  If not
*   UP, try to set the UP flag.
*
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFFLAGS,&ifrq);
  if (status == -1) perror("ioctl err");

  printf("Interface flags = 0x%x\n",ifrq.ifr_flags);
  if (!(ifrq.ifr_flags & IFF_UP))
    {
      ifrq.ifr_flags |= IFF_UP;
      status = ioctl(pfd,SIOCSIFFLAGS,&ifrq);
      if (status == -1) perror("ioctl err");
      sleep(2);
    }
  else  printf("Interface %s is UP\n",ifname);

/*
*   Get interface MTU
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFMTU,&ifrq);
  if (status == -1) perror("ioctl err");
  printf("Interface MTU = %i\n",ifrq.ifr_metric);

/*
*   Build filter here
*/
  fprog.len = filter1_len;
  fprog.filter = filter1;
  filter1[1].k = proto;
  filter1[3].k = 45000 + PROTO_DATA;

  status = setsockopt(pfd,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
  if (status != 0) perror("setsockopt 2");

  return;
}
/******************************************************************************
*   Read function for Ethernet debugging with packet filter
******************************************************************************/
int read_ether (void)
{
  int status;
  int i, j, k,len;
  static struct sockaddr_ll sll;
  int fromlen = sizeof(sll);
  unsigned short *sptr, rbuf[1536];
  unsigned short *lan_buf = (unsigned short *)&rbuf[25];
  int *sizeptr = (int *)&rbuf[23];

/*-----------------------------------------------------------------*/

  while(1)
   {

/*    Read a packet from the Ethernet                              */

     status  = recvfrom(pfd,(unsigned char *)&rbuf,sizeof(rbuf),MSG_TRUNC,
                                             (struct sockaddr *)&sll,&fromlen);
     if (status  < 0) {perror("read err"); return(FAILURE);}

     len = *sizeptr;
     sptr = lan_buf;
     printf ("%4.4x%4.4x  ",*(sptr+1),*sptr);
     sptr += 2;
     for(i=4; i < len; i += 2)
       {
         if(i%32 == 0) printf("\n");
         printf("%4.4x ",*sptr++);
       }
     printf("\n");
     fflush(stdout);

    }
  return(SUCCESS);
}
/****************************************************************************
*
*   Control C handler
*
****************************************************************************/
void INTsignal(int i)
{
   close(pfd);
   fflush(stdout);
   exit(0);
}
