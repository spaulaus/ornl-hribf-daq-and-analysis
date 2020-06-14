#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netinet/in.h>
#include  <netdb.h>
#include  <arpa/inet.h>
#include  <net/if.h>
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <unistd.h>
#include  <errno.h>


#define  ETHER_RECEIVE  -32
#define  ETHER_TRANSMIT -33
#define  ETHER_OPEN     -34

/*  Function prototype              */
static int pkt_io(int,int);
static int readn(int ,unsigned char *,int );
static int writen(int ,unsigned char *,int );

extern int h_errno;              /* global host error number */
extern int errno;

/*   CAMAC data buffers             */
static struct Vmed xbuf,rbuf;

/*****************************************************************************
*   Send a packet to the VME processor for execution.
*
*  Call:   len  - Number of data bytes in packet
*          tmo  - Timeout in seconds
*
*  Return:  0   -  OK
*           -32 -  No reply from the VME processor
*           -33 -  Packet transmission error
*           -34 -  Open error
*
*****************************************************************************/
static int  pkt_io(int len,int tmo)
{
          int status;
   static int sockfd = -1;
   static char vmeserver[80];
   static char server[16] = "vme";
   static struct sockaddr_in serv_addr;
   static struct Vme_start initvme;

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
          status = connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
        }
      while (status == -1 && errno == EINTR);
      if (status == -1)
        {
          perror(" VMESERVER - connect");
          exit(99);
        }
      initvme.proto = CODE;
      initvme.cmd_len = sizeof(struct Vmed);
      initvme.rpy_len = sizeof(struct Vmed);
      status = writen(sockfd,(unsigned char *)&initvme,sizeof(struct Vme_start));
      if (status <= 0)
        {
          perror(" VMESERVER - write");
          exit(99);
        }
    }
   xbuf.len = len;
   xbuf.error = 0;
   status = writen(sockfd,(unsigned char *)&xbuf,initvme.cmd_len);
   if (status != initvme.cmd_len)
     {
       perror(" VMESERVER - write");
       exit(99);
     }
   status = readn(sockfd,(unsigned char *)&rbuf,initvme.rpy_len);
   if (status <= 0)
     {
       perror(" VMESERVER - read");
       exit(99);
     }
     
   status = rbuf.error;
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
