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
*    File:         /usr/users/mcsq/Dlinux/Dvme/vme_io.c
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
*    6/15/03    MCSQ        Special version of pkt_io for the VMEXX
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "../Dlan/orph_pf.h"
#include "vme_io.h"

static int sockfd = -1;
static char vmeserver[80];
static char server[16] = "vme";
static struct sockaddr_in serv_addr;
static struct Vme_start initvme;

/*****************************************************************************
*   Send a packet to the VME processor for execution.
*
*  Call:   out - Pointer to output buffer
*          in  - Pointer to input buffer
*          proto - Protocal type
*          tmo  - Timeout in seconds
*
*  Return:  0   -  OK
*           -32 -  No reply from the VME processor
*           -33 -  Packet transmission error
*           -34 -  Open error
*
*****************************************************************************/
int  vme_io(struct Vmed *out,struct Vmed *in,int proto,int tmo)
{
   int status;

   if(sockfd == -1)
    {
      vme_open(out,in,proto,tmo);
    }
   if (out->len < 0 || out->len > initvme.cmd_len)
     {
       printf("vme_io: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   if (in->len < 0 || in->len > initvme.rpy_len)
     {
       printf("vme_io: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   out->error = 0;
   status = writen(sockfd,(unsigned char *)out,initvme.cmd_len);
   if (status != initvme.cmd_len)
     {
       perror(" VMESERVER - write");
       exit(99);
     }
/*************
printf("sent data packet %i\n",initvme.cmd_len);
*************/
   status = readn(sockfd,(unsigned char *)in,initvme.rpy_len);
   if (status <= 0)
     {
       perror(" VMESERVER - read");
       exit(99);
     }
/*************
printf("receive data packet %i\n",initvme.rpy_len);
*************/
     
   status = in->error;
   return (status);
}
/******************************************************************************
******************************************************************************/
static int readn(int fd,unsigned char *ptr,int nbytes)
{
   int nleft,nread;

   nleft = nbytes;
   while(nleft > 0)
     {
       do
         {
           nread = read(fd,ptr,nleft);
         }
       while (nread == -1 && errno == EINTR);
       if (nread < 0)  return(nread);       /* read error  */
       else if (nread == 0) break;          /* EOF         */
       nleft -= nread;
       ptr += nread;
     }
   return(nbytes - nleft);
}
/******************************************************************************
******************************************************************************/
static int writen(int fd,unsigned char *ptr,int nbytes)
{
   int nleft,nwritten;

   nleft = nbytes;
   while(nleft > 0)
     {
       do
         {
           nwritten = write(fd,ptr,nleft);
         }
       while (nwritten == -1 && errno == EINTR);
       if (nwritten <= 0)  return(nwritten);   /* write error  */
       nleft -= nwritten;
       ptr += nwritten;
     }
   return(nbytes - nleft);
}
/******************************************************************************
******************************************************************************/
int vme_open(struct Vmed *out,struct Vmed *in,int proto,int tmo)
{
   int status,hdr_len;

   if (out->len < 0 || out->len > MAX_ORPH_DATA)
     {
       printf("vme_open: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   if (in->len < 0 || in->len > MAX_ORPH_DATA)
     {
       printf("vme_open: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   if (proto < DATA || proto > RMSSIO)
     {
       printf("vme_open: Ether protocol out_of_range\n");
       exit(99);
     }
   if (tmo < 0) tmo = 0;
   hdr_len = sizeof(struct Vmed_hdr);
   if(sockfd == -1)
    {
      char *cptr;
      struct hostent *hostptr;
      struct in_addr address,*inadr;

      if ((cptr = getenv("VME")) == NULL) cptr = server;
      strcpy(initvme.server,cptr);
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
      else
        {
          hostptr = gethostbyaddr(&address,sizeof(struct in_addr),AF_INET);
        }
      if (hostptr == NULL)
        {
          printf(" VMESERVER - Host Lookup error -%s\n",cptr);
          exit(99);
        }
      inadr = (struct in_addr *)*(hostptr->h_addr_list);

      sockfd = socket(AF_INET,SOCK_STREAM,0);
      if (sockfd == -1)
        {
          perror(" VMESERVER - socket error");
          exit(99);
        }
      serv_addr.sin_family = AF_INET;
      serv_addr.sin_port = htons(6996);
      serv_addr.sin_addr.s_addr = inadr->s_addr;
      do
        {
          status = connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr
));
        }
      while (status == -1 && errno == EINTR);
      if (status == -1)
        {
          perror(" VMESERVER - connect");
          exit(99);
        }
      initvme.proto = proto;
      initvme.timeout = tmo;
      initvme.cmd_len = out->len + hdr_len;
      initvme.rpy_len = in->len + hdr_len;
/******************
printf("Proto = %x\n",initvme.proto);
printf("Timeout = %x\n",initvme.timeout);
printf ("cmd_len = %i\n",initvme.cmd_len);
printf ("rpy_len = %i\n",initvme.rpy_len);
******************/
      status = writen(sockfd,(unsigned char *)&initvme,sizeof(struct Vme_start))
;
      if (status <= 0)
        {
          perror(" VMESERVER - write");
          exit(99);
        }
/*************
printf("sent start packet %i\n",sizeof(struct Vme_start));
*************/
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
int vme_send(struct Vmed *out)
{
   int status;

   if (out->len < 0 || out->len > initvme.cmd_len)
     {
       printf("vme_send: Ether transmit buffer size out_of_range\n");
       exit(99);
     }
   out->error = 0;
   status = writen(sockfd,(unsigned char *)out,initvme.cmd_len);
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
int vme_recv(struct Vmed *in)
{
   int status;

   if (in->len < 0 || in->len > initvme.rpy_len)
     {
       printf("vme_recv: Ether receive buffer size out_of_range\n");
       exit(99);
     }
   status = readn(sockfd,(unsigned char *)in,initvme.rpy_len);
   if (status <= 0)
     {
       perror(" VMESERVER - read");
       exit(99);
     }
/*************
printf("receive data packet %i\n",initvme.rpy_len);
*************/

   return (in->error);
}
