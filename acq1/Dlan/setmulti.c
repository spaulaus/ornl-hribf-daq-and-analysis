#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <netpacket/packet.h>
#include <linux/types.h>
#include <linux/filter.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "orph_pf.h"

struct sock_fprog fprog;
struct sock_filter filter[] = {
   BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 12),
   BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0x4f51, 0, 1),
   BPF_STMT(BPF_RET+BPF_K, (u_int) -1),     /* accept packet  */
   BPF_STMT(BPF_RET+BPF_K, 0)               /* ignore packet  */
};

/****************************************************************************
*
*   Enable a Multicast address for the VME bootstrap process.
*   NOTE:  Only superuser may do this.
****************************************************************************/
main(int argc, char *argv[])
{
     int  i,ifindex,j,k,s,status;
     char ifname[16];
     time_t clk;
     unsigned char *cptr,tbuf[1536];
     unsigned short *sptr;
     struct sockaddr_ll      sll;
     struct timeval  tval;
     static struct ifreq ifrq;
     static struct packet_mreq mreq;
/****
     static unsigned char multiaddr[] = {0x03,0x6d,0x63,0x73,0x71,0x00};
     static unsigned char multiaddr[] = MULTICAST;
****/
     static unsigned char multiaddr[] = MULTICAST;
     static unsigned char vmeaddr[] = {0x00,0x80,0x42,0x04,0x65,0x26};
     static unsigned char wksaddr[] = {0x00,0x80,0x42,0x04,0x65,0x26};

fprog.len = 4;
fprog.filter = filter;

     strcpy(ifname,"eth1");
     memset(&mreq,0,sizeof(mreq));
     s = socket(PF_PACKET,SOCK_RAW,htons(ETH_P_ALL));
     if (s == -1) {perror("socket error"); exit(1);}

     ifindex = if_nametoindex(ifname);
     mreq.mr_ifindex = ifindex;
     mreq.mr_type = PACKET_MR_PROMISC;
     status = setsockopt(s,SOL_PACKET,PACKET_DROP_MEMBERSHIP,&mreq,sizeof(mreq));
     if (status == -1) perror("setsockopt 1");

     mreq.mr_alen = 6;
     memcpy(mreq.mr_address,multiaddr,6);
     mreq.mr_type = PACKET_MR_MULTICAST;
     status = setsockopt(s,SOL_PACKET,PACKET_DROP_MEMBERSHIP,&mreq,sizeof(mreq));
     if (status == -1) perror("setsockopt 2");
/*******
     memset(&mreq,0,sizeof(mreq));
     mreq.mr_ifindex = ifindex;
     mreq.mr_type = PACKET_MR_PROMISC;
*******/
     status = setsockopt(s,SOL_PACKET,PACKET_ADD_MEMBERSHIP,&mreq,sizeof(mreq));
     if (status == -1) perror("setsockopt 3");
/***********
***********/
/*
*   Get "ifname" hardware address
*/
     memset(&ifrq,0,sizeof(ifrq));
     strcpy(ifrq.ifr_name,ifname);
     status = ioctl(s,SIOCGIFHWADDR,&ifrq);
     if (status == -1) perror("ioctl err");

     memcpy(wksaddr,ifrq.ifr_hwaddr.sa_data,6);
     for (i=0; i < 6; i++) printf("%2.2x:",wksaddr[i]);
     printf("\n");
     printf("family = %x\n",ifrq.ifr_hwaddr.sa_family);

/*
*   Get interface flags.  Check for interface UP.  If not
*   UP, try to set the UP flag.
*/
     memset(&ifrq,0,sizeof(ifrq));
     strcpy(ifrq.ifr_name,ifname);
     status = ioctl(s,SIOCGIFFLAGS,&ifrq);
     if (status == -1) perror("ioctl err");

     printf("Interface flags = 0x%x\n",ifrq.ifr_flags);
     if (!(ifrq.ifr_flags & IFF_UP))
       {
         ifrq.ifr_flags |= IFF_UP;
         status = ioctl(s,SIOCSIFFLAGS,&ifrq);
         if (status == -1) perror("ioctl err");
       }
     else  printf("Interace %s is UP\n",ifname);

/*
*   Get interface MTU
*/
     memset(&ifrq,0,sizeof(ifrq));
     strcpy(ifrq.ifr_name,ifname);
     status = ioctl(s,SIOCGIFMTU,&ifrq);
     if (status == -1) perror("ioctl err");

     printf("Interface MTU = %i\n",ifrq.ifr_metric);

     status = setsockopt(s,SOL_SOCKET,SO_ATTACH_FILTER,&fprog,sizeof(fprog));
     if (status != 0) perror("setsockopt 4");
/*
*   Must do bind for 'read' and 'write' to work
*/
        memset(&sll, 0, sizeof(sll));
        sll.sll_family          = AF_PACKET;
        sll.sll_ifindex         = ifindex;
        sll.sll_protocol        = htons(ETH_P_ALL);

        if (bind(s, (struct sockaddr *) &sll, sizeof(sll)) == -1)
                 perror("bind err");

/*
*   Send a reboot request to vme3
*/
     memset(tbuf, 0, sizeof(tbuf));
     cptr = tbuf;
     memcpy(cptr,vmeaddr,6);
     cptr += 6;
     memcpy(cptr,wksaddr,6);
     cptr += 6;
     sptr = (unsigned short *)cptr;
     *sptr = htons(0x4f52);

     status = write(s,tbuf,100);
     if (status < 0) perror("write err");

/*
*   Read and list packets recieved
*/

while(1)
{
   int fromlen = sizeof(sll);
/*******
   status = read(s,tbuf,1536);
*******/
   status = recvfrom(s,tbuf,1536,MSG_TRUNC,(struct sockaddr *)&sll,&fromlen);
   if (status < 0) {perror("read err"); continue;}
   printf("Read count = %i\n",status);
     i = ioctl(s,SIOCGSTAMP,&tval);
     if (i == -1) perror("ioctl err");
   clk = tval.tv_sec;
   printf("Time - %s", ctime(&clk));
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
   for (i=0; i < 6; i++) printf("%2.2x:",sll.sll_addr[i]);
   printf("\n");
   printf("Protocol:  %x\n",ntohs(sll.sll_protocol));
/************
   for (i = 0; i < status; i++) 
     {
       printf("%2.2x ",tbuf[i]);
       if ( (i % 16) == 15) printf("\n");
     }
   printf("\n");
   printf("\n");
************/
   for (i = 0; i < status; i+=16)
     {
       char tchar;

       k = status - i;
       if (k > 16) k = 16;
       for (j = i; j < i + k; ++j) printf("%2.2x ",tbuf[j]);
       for (j = k; j < 17; j++) printf("   ");
       for (j = i; j < i + k; ++j)
         {
           tchar = tbuf[j];
           if (tbuf[j] < 0x20 || tbuf[j] > 0x7f) tchar = '.';
           printf ("%c", tchar);
         }
       printf("\n");
     }
    printf("\n");
}
     return(0);
}
