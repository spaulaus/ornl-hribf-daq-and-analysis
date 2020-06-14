/*-----------------------------------------------------------------
 ether_listen.c 
 A program to listen on an Ethernet interface for packets.  It
 uses the promiscuous mode of the packet filter interface.
 R. Varner, based on "packetfilter(4)" document,

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

#include "orph_pf.h"

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
static unsigned char multiaddr[HW_ADDR_LEN] = MULTICAST;
static unsigned char wksaddr[HW_ADDR_LEN];


/*------------------------------------------------------------------*/
main (int argc, char *argv[])
{
  char ifname[8];
  char iproto[8];
  static unsigned char Protocol[2];	/* protocol for filtering   */
  int status;			/* status return from functions     */
  char *cptr;
  long proto = 0;
  int  i,len,proto_err = 0;

/*   Process any arguments (1 or 2 expected)                        */
  if ((argc < 2) || (argc > 3))
    {
      printf ("Usage: listen devname [protocol]\n");
      return 0;
    }
  else
    {
      /* first and only argument is device name */
      strncpy (ifname, argv[1], sizeof(ifname));
      if (strcmp(ifname,"eth0") && strcmp(ifname,"eth1"))
        {
          printf("Unknown Ethernet Interface: %s\n",ifname);
          return(0);
        }
      if (argc == 3)
	{
         /* get the protocol number */
          strncpy(iproto, argv[2], sizeof(iproto));
          cptr = strtok(iproto, "-");
          if (cptr != NULL)
            {
              len = strlen(cptr);
              for (i=0; i < len; i++) if (!isxdigit(*(cptr+i))) proto_err = 1;
	      proto = strtol (cptr, NULL, 16);
              if (proto > 255 || proto < 0) proto_err = 1;
	      Protocol[0] = proto;
              cptr = strtok(NULL, "-");
              if (cptr != NULL)
                {
                  for (i=0; i < len; i++) if (!isxdigit(*(cptr+i)))
                                                                proto_err = 1;
                  proto = strtol (cptr, NULL, 16);
                  if (proto > 255 || proto < 0) proto_err = 1;
	          Protocol[1] = proto;
                }
              else
                {
                  proto_err = 1;
                }
            }
          else
            {
              proto_err = 1;
            }
          if(proto_err)
            {
              printf("Invalid Protocol: %s\n",argv[2]);
              exit(0);
            }
          printf("Protocol:  %x-%x\n",Protocol[0],Protocol[1]);
          proto = (Protocol[0] << 8) + Protocol[1];
	}
    }

  open_ether_listen (ifname, proto);
  if (pfd == FAILURE)
    {
      printf ("Error opening Ethernet interface\n");
      return FAILURE;
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
  static int filter1_len = 4;
  static struct sock_filter filter1[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),      /* Get Protocol word from packet */
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x0000, 0, 1), /* 0x0000 changed on build */
   BPF_STMT(BPF_RET+BPF_K, (u_int) -1),     /* accept packet  */
   BPF_STMT(BPF_RET+BPF_K, 0)               /* ignore packet  */
};

/*
*  If no protocol specified on command line, use this filter.  It limits
*  those accepted to ones used in our system.
*/
  static int filter2_len = 5;
  static struct sock_filter filter2[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),      /* Get Protocol word from packet */
   BPF_JUMP(BPF_JMP+BPF_JGE+BPF_K, 0x4f50, 0, 2),
   BPF_JUMP(BPF_JMP+BPF_JGT+BPF_K, 0x4f5f, 1, 0),
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
 *  Set promiscuous on this socket
 */
  memset(&mreq,0,sizeof(mreq));
  mreq.mr_ifindex = ifindex;
  mreq.mr_type = PACKET_MR_PROMISC;
  status = setsockopt(pfd,SOL_PACKET,PACKET_ADD_MEMBERSHIP,&mreq,sizeof(mreq));
  if (status == -1) perror("setsockopt 1");
/*
*   Get "ifname" hardware address
*/
  memset(&ifrq,0,sizeof(ifrq));
  strcpy(ifrq.ifr_name,ifname);
  status = ioctl(pfd,SIOCGIFHWADDR,&ifrq);
  if (status == -1) perror("ioctl err");

/*
*   Get workstation hardware address.  Not needed in this code,
*   but will be in the packet filter codes.
*/
  memcpy(wksaddr,ifrq.ifr_hwaddr.sa_data,HW_ADDR_LEN);
  printf ("Device H/W address: %2.2x", wksaddr[0]);
  for (i = 1; i < HW_ADDR_LEN; i++) printf(":%2.2x",wksaddr[i]);
  printf ("\n");
/*
*   Get interface flags.  Check for interface UP.  If not
*   UP, try to set the UP flag.
*
*   NOTE:  At some time, I hope the interface will be set UP on
*           system boot!!
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
  if (proto)
    {
     fprog.len = filter1_len;
     fprog.filter = filter1;
     filter1[1].k = proto;     /* protocol was specified on command line  */
    }
  else
    {
     fprog.len = filter2_len;
     fprog.filter = filter2;
    }
  status = setsockopt(pfd,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
  if (status != 0) perror("setsockopt 2");

  return;
}
/******************************************************************************
*   Read function for Ethernet debugging with packet filter
******************************************************************************/
int read_ether (void)
{
  int pkt_len;			/* Length of packet              */
  int dat_len;                  /* lenght of Data part of packet */
  int status;
  char tchar;
  time_t clk;
  int i, j, k;
  static struct sockaddr_ll sll;
  int fromlen = sizeof(sll);
  static struct timeval  tval;
  static struct Ether_Packet rbuf;
  fd_set readfds,errfds;
  struct timeval  timeout;

/*-----------------------------------------------------------------*/

  FD_ZERO(&errfds);
  FD_ZERO(&readfds);
  FD_SET(pfd,&readfds);
  FD_SET(pfd,&errfds);

  timeout.tv_sec = 15;
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

  pkt_len = recvfrom(pfd,(unsigned char *)&rbuf,sizeof(rbuf),MSG_TRUNC,
                                             (struct sockaddr *)&sll,&fromlen);
  if (pkt_len < 0) {perror("read err"); return FAILURE;}
  status = ioctl(pfd,SIOCGSTAMP,&tval);
  if (status == -1) perror("ioctl err");
  clk = tval.tv_sec;

  printf ("*********************  Packet  *************************\n");
  printf ("Time - %s", ctime (&clk));
  printf(" Packet Length: %i\n",pkt_len);
  switch(sll.sll_pkttype)
   {
      case PACKET_HOST:
          printf("To Us     ");
          break;
      case PACKET_BROADCAST:
          printf("Broadcast ");
          break;
      case PACKET_MULTICAST:
          printf("Multicast ");
          break;
      case PACKET_OTHERHOST:
          printf("Other Host ");
          break;
      case PACKET_OUTGOING:
          printf("Transmit   ");
          break;
      default:
          break;
   }

/*   Write out the packet header data                    */

  printf ("\n   Destination: %2.2x", rbuf.Destination[0]);
  for (i = 1; i <= 5; ++i) printf ("-%2.2x", rbuf.Destination[i]);

  printf ("\n        Source: %2.2x", rbuf.Source[0]);
  for (i = 1; i <= 5; ++i) printf ("-%2.2x", rbuf.Source[i]);

  printf ("\n      Protocol: %2.2x-%2.2x", rbuf.Protocol[0], rbuf.Protocol[1]);

  dat_len = pkt_len - HW_ADDR_LEN - HW_ADDR_LEN - PROT_SIZE;

  printf ("\n Buffer Length: %d\n", dat_len);

  if (rbuf.Protocol[0] == PROTO_PREFIX)
    {
     printf("  Ack: %x\n",rbuf.Ack);
     dat_len -= sizeof(rbuf.Ack);
     printf("Order: %x\n",rbuf.Order);
     dat_len -= sizeof(rbuf.Order);
     printf("  PID: %d\n",rbuf.Request_Number);
     dat_len -= sizeof(rbuf.Request_Number);
    }
  for (i = 0; i <= dat_len; i = i + 16)
    {
      k = dat_len - i;
      if (k > 16) k = 16;
      for (j = i; j < i + k; ++j) printf ("%2.2x ", rbuf.Data[j]);
      for (j = k; j < 17; j++) printf ("   ");
      for (j = i; j < i + k; ++j)
	{
	  tchar = rbuf.Data[j];
	  if (rbuf.Data[j] < 0x20 || rbuf.Data[j] >= 0x7f) tchar = '.';
	  printf ("%c", tchar);
	}
      putchar ('\n');
    }
  printf("\n");

  return SUCCESS;
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
