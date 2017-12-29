/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                              Copyright(C) 2003
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dacq/vme_io_udp.c
*
*    Description:
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/27/02    MCSQ        Original
*
*    3/18/03    MCSQ        Added timout to the Vme_start structure.
*
*    3/28/03    MCSQ        Add limit checks on call paramaters.
*
*    3/09/05    RLV         Mangled for UDP protocol, directly to VME
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "../include/pkt_io.h"

#define getsock(x) 45000+x

static int sockfd = -1;
static char vmeserver[80];
static char server[16] = "vme";
static struct sockaddr_in serv_addr;
static struct sockaddr_in cli_addr;
static struct Vme_start {
       int timeout;
       int cmd_len;
       int rpy_len;
} initvme;

/*****************************************************************************
*   Send a packet to the VME processor for execution.
*
*  Call:   out - Pointer to output buffer
*          in  - Pointer to input buffer
*          proto - Protocal type
*          tmo  - Timeout in seconds
*
*****************************************************************************/
int  vme_io(struct UDP_Packet *out,struct UDP_Packet *in,int proto,int tmo)
{
   int status;

   if(sockfd == -1)
    {
      vme_open(out,in,proto,tmo);
    }
   if (out->DataSize < 0 || out->DataSize > initvme.cmd_len)
     {
       printf("vme_io: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   if (in->DataSize < 0 || in->DataSize > initvme.rpy_len)
     {
       printf("vme_io: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   status = sendto(sockfd, (void *) out, initvme.cmd_len, 
                   0, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
   if (status != initvme.cmd_len) {
       perror(" VMESERVER - write");
       exit(99);
     }
/*************
printf("sent data packet %i\n",initvme.cmd_len);
*************/
   status = recvfrom(sockfd,(void *)in, initvme.cmd_len,
                     0, (struct sockaddr *) 0, (int *) 0);
   if (status <= 0) {
       perror(" VMESERVER - read");
       exit(99);
     }
/*************
printf("receive data packet %i\n",initvme.rpy_len);
*************/
     
   status = 0;
   return (status);
}
/******************************************************************************
******************************************************************************/
int vme_open(struct UDP_Packet *out,struct UDP_Packet *in,int proto,int tmo)
{
   int status,hdr_len;

   if (out->DataSize < 0 || out->DataSize > MAX_TCP_DATA)
     {
       printf("vme_open: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   if (in->DataSize < 0 || in->DataSize > MAX_TCP_DATA)
     {
       printf("vme_open: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   if (proto < DATA || proto > RMSSIO)
     {
       printf("vme_open: UDP socket out_of_range\n");
       exit(99);
     }
   if (tmo < 0) tmo = 0;
   hdr_len = 2*sizeof(int); /* This header is only the DataSize longword */
   if(sockfd == -1)
     {
       char *cptr;
       struct hostent *hostptr;
       struct in_addr address,*inadr;

       if ((cptr = getenv("VMESERVER")) == NULL)
	 {
	   gethostname(vmeserver,sizeof(vmeserver));
	   cptr = vmeserver;
	 }
       address.s_addr = inet_addr(cptr);
       if (address.s_addr == 0xffffffff)
	 {
	   hostptr = gethostbyname(cptr);
	   if (hostptr == NULL && h_errno == 0)
	     {
	       fprintf (stderr," VMESERVER - Unknown Host - %s\n",cptr);
	       exit(99);
	     }
	 }
       else {
	   hostptr = gethostbyaddr(&address,sizeof(struct in_addr),AF_INET);
	 }

       if (hostptr == NULL) {
	   printf(" VMESERVER - Host Lookup error -%s\n",cptr);
	   exit(99);
	 }
       inadr = (struct in_addr *)*(hostptr->h_addr_list);
       
       sockfd = socket(AF_INET,SOCK_DGRAM,0);
       if (sockfd == -1) {
	   perror(" Pkt_io_udp - socket creation error");
	   exit(99);
	 }
       /* Bind the socket for listening on private socket */
       bzero((char *) &cli_addr, sizeof(cli_addr));    /* zero out */
       cli_addr.sin_family      = AF_INET;
       cli_addr.sin_addr.s_addr = htonl(INADDR_ANY);
       cli_addr.sin_port        = htons(0);
       if (bind(sockfd, (struct sockaddr *) &cli_addr, sizeof(cli_addr)) < 0)
	 {
	   perror(" Pkt_io_udp - cannot bind local address");
	   exit(99);
	 }
       /* Define the server address and socket number */
       serv_addr.sin_family = AF_INET;
       serv_addr.sin_port = htons(getsock(proto));
       serv_addr.sin_addr.s_addr = inadr->s_addr;

       /* Save this information for later */
       initvme.timeout = tmo;
       initvme.cmd_len = out->DataSize + hdr_len;
       initvme.rpy_len = in->DataSize + hdr_len;
       /******************
printf("Proto = %x\n",initvme.proto);
printf("Timeout = %x\n",initvme.timeout);
printf ("cmd_len = %i\n",initvme.cmd_len);
printf ("rpy_len = %i\n",initvme.rpy_len);
       ******************/
       
     }
   return(sockfd);
}
/******************************************************************************
******************************************************************************/
void vme_close(void)
{
  close(sockfd);
  sockfd = -1;
}
/******************************************************************************
******************************************************************************/
int vme_send(struct UDP_Packet *out)
{
   int status;

   if (out->DataSize < 0 || out->DataSize > initvme.cmd_len)
     {
       printf("vme_send: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   status = sendto(sockfd, (void *) out, initvme.cmd_len, 
                   0, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
   if (status != initvme.cmd_len)
     {
       perror(" VMESERVER - write");
       exit(99);
     }
/*************
printf("sent data packet %i\n",initvme.cmd_len);
*************/
   return(0);
}
/******************************************************************************
******************************************************************************/
int vme_recv(struct UDP_Packet *in)
{
   int status;

   if (in->DataSize < 0 || in->DataSize > initvme.rpy_len)
     {
       printf("vme_recv: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   status = recvfrom(sockfd,(void *)in, initvme.rpy_len,
                     0, (struct sockaddr *) 0, (int *) 0);
   if (status <= 0)
     {
       perror(" VMESERVER - read");
       exit(99);
     }
/*************
printf("receive data packet %i\n",initvme.rpy_len);
*************/

   status = 0;
   return (status);
}
