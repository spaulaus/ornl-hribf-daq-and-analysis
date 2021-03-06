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
*    File:         /usr/users/mcsq/Dlinux/Dacq/pkt_io_udp.c
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
*
*    05/13/05   RLV         Allow reuse of a socket for different protocols
*                           Add timeout for reply
*                           Examine sequence number
*****************************************************************************/
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include "pkt_io_udp.h"

/*  Retry TORETRY times on timeout  */
#define TORETRY 5

static int sockfd = -1;
//static char server[16] = "vme";
static struct sockaddr_in serv_addr;
static struct sockaddr_in cli_addr;
static int    tout_flg=0;  /* Flag set by signal handler for alarm */

/******************************************************************************
to_alarm - handle the signal from SIGALARM.  All this does is set a flag. 
******************************************************************************/
void to_alarm(int sn)
{
  tout_flg = 1;
}

/******************************************************************************
pkt_io - utility to send a packet to a server and get the reply. 
  Call:   out - Pointer to output buffer, must be UDP_Packet
          in  - Pointer to input buffer
          proto - Protocol type, see orph_udp.h for details
          tmo  - Timeout in seconds
  Return = 0 -> OK
  Return = -1 -> An error.  Check errno on return.
  Special errnos: 
          ETIMEDOUT - waiting for an acknowledgement failed
          EPKTSEQ   - the received packet had a different sequence than
                      that expected.
          EDESTADDRREQ - The VME environment variable was either not set or
                         not set to a valid TCP/IP node name.

******************************************************************************/
int pkt_io(struct UDP_Packet *out, 
	   struct UDP_Packet *in,
	   int proto, int tmo)
{
   int status;
   int tocnt=0;               /* count the number of timeouts */

   errno = 0;     /* make sure we are reset each time */

   if(sockfd == -1) {                /* Open the socket if not already open */
     status = pkt_open(NULL); /* Do not specify a host */
     if (status< 0) {
       fprintf(stderr," pkt_io: Unable to open socket, returning\n");
       return(ETHER_OPEN);
     }
   }
   /*  Increment the sequence number.  Do not rely on caller. */
   out->Sequence++;

   /* Prepare to retry the send/recv, in case of net congestion          */
   /* The assumption is that the other end failed to receive the request */
   /* and so it must be retransmitted.                                   */
   tocnt=0;
   while (tocnt++ < TORETRY) {

     /* Send the message to the server.  Errors here are not recoverable */
     status = pkt_send(out, proto);
     if (status < 0) {
       perror(" pkt_io: Quit after send error");
       return(ETHER_TRANSMIT);
     }

     /* Get the reply message, use geometric backoff*/
     status = pkt_recv(in, proto, tmo*tocnt);
     if (status == -1) {
       if (errno == ETIMEDOUT) {  /* Special error for Time out */
	 //	 fprintf(stderr, "pkt_io: Retry... \n");
	 continue;
       }
       else {                     /* All other errors  */
	 perror(" pkt_io - Quit after recv error");
	 return(ETHER_RECEIVE);
       }
     }
     else /* A successful read means we can leave the while */
       break;
   }
   
   if (tocnt >= TORETRY) {
     fprintf(stderr,
       "pkt_io: Recv timeout error for packet %i, timeout %i, protocol %i\n",
        out->Sequence, tmo, proto);
     return(ETHER_TIMEOUT);
   }

    
   /* Test for messages out of sequence */
   if (in->Sequence != out->Sequence) {
     fprintf(stderr, "pkt_io: Sequence failure: in: %i, out: %i\n",
	     in->Sequence, out->Sequence);
     errno=EPKTSEQ;
     /* Experimental FIX */
     /* Try to fix the problem.  It seems that if we get out of sequence 
        in SCOP with the VME, we stay there forever. */
     out->Sequence = in->Sequence;

     return(ETHER_SEQUENCE);
   }
   
   return(0);
}
/******************************************************************************
pkt_open - open a socket and prepare for sending messages to a VME server
*node is an optional node name for the connection

******************************************************************************/
int pkt_open(char *node)
{
   int status;

   /* The next code tries to obtain the VME hostname from the VME environment
      variable and validate it.  We must get both name and address from the
      lookup.  Any error here returns.
   */
   if (sockfd == -1) {
     char *cptr;
     struct hostent *hostptr;
     struct in_addr address,*inadr;
     
     /*  Get the node name to use */
     if (node == NULL) {
       /* VME environment specifies the node */
       if ((cptr = getenv("VME")) == NULL)
	 {
	   fprintf(stderr,"pkt_open: VME environment variable must be set!\n");
           errno=EDESTADDRREQ; /* E Dest Address Required */
	   return(-1);
	 }
     }
     else     /* Caller specifies the node */
       cptr = node;

     /* Translate the name or address into in_addr */
     status = inet_aton(cptr, &address);
     if (status == 0)  /* cptr is not an address */
       {
	 hostptr = gethostbyname(cptr);
	 if (hostptr == NULL && h_errno == 0)
	   {
	     fprintf (stderr," pkt_open - Unknown Host - %s\n",cptr);
	     errno=EDESTADDRREQ;
	     return(-1);
	   }
       }
     else {            /* cptr is already an address */
       hostptr = gethostbyaddr(&address,sizeof(struct in_addr),AF_INET);
     }
     
     if (hostptr == NULL) {
       fprintf(stderr," pkt_open - Host Lookup error -%s\n",cptr);
       errno=EDESTADDRREQ;
       return(-1);
     }
     
     /* Now we have a valid VME address.  We next obtain a socket, 
	then bind the program to the local socket for listening. 
	This program is the client.
     */
     inadr = (struct in_addr *)*(hostptr->h_addr_list);
     
     sockfd = socket(AF_INET,SOCK_DGRAM,0);
     if (sockfd == -1) {
       perror(" pkt_open - socket creation error");
       return(-1);
     }
     /* Bind the socket for listening on private socket */
     bzero((char *) &cli_addr, sizeof(cli_addr));    /* zero out */
     cli_addr.sin_family      = AF_INET;
     cli_addr.sin_addr.s_addr = htonl(INADDR_ANY);
     cli_addr.sin_port        = htons(0);
     if (bind(sockfd, (struct sockaddr *) &cli_addr, sizeof(cli_addr)) < 0)
       {
	 perror(" pkt_open: cannot bind local address");
	 return(-1);
       }
     /* Define the server address and socket number, but not the port 
	number.  We only need that when we sendto or recvfrom, so we 
	postpone that until just before those routines.
     */
     serv_addr.sin_family = AF_INET;
     serv_addr.sin_addr.s_addr = inadr->s_addr;
     
   }
   
   return(0);
}
/******************************************************************************
pkt_close - closes the socket specified in the global "sockfd".  
******************************************************************************/
int pkt_close(void)
{
  if (close(sockfd) == 0) {
    sockfd = -1;
    return(0);
  }
  else {
    sockfd = -1;
    return(-1);
  }
}
/******************************************************************************
pkt_send - sends a UDP_packet to a server, using a protocol specified by 
the user, on the socket specified in the global "sockfd".  Checks for 
valid DataSize in the packet and protocol in range. Returns 0 on successful 
completion.  Any error crashes the code (exit(99)).
******************************************************************************/
int pkt_send(struct UDP_Packet *out, int proto)
{
   int status, cmd_len;

   errno=0;

   /* Check the DataSize for validity */
   if (out->DataSize < 0 || out->DataSize > MAX_ORPH_DATA)
     {
       fprintf(stderr,"pkt_send: Ether transmit buffer size out_of_range\n");
       errno=EINVAL;
       return(-1);
     }
   cmd_len = out->DataSize + PKTHDRLEN; /* Save the command length */

   /* Check the protocol number for validity */
   if (proto < DATA || proto > RMSSIO)
     {
       fprintf(stderr,"pkt_send: UDP socket out_of_range\n");
       errno=EINVAL;
       return(-1);
     }
   serv_addr.sin_port = htons(protsock(proto)); /* Set the protocol */

   status = sendto(sockfd, (void *) out, cmd_len, 
                   0, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
   if (status != cmd_len) /* An error here must mean interrupts */
     {
       perror(" pkt_send: sendto incomplete");
       return(-1);
     }
   return(0);
}

/***********************************************************************/
/* Timer utility routine using POSIX signals*/
void start_timer(int tmo)
{
   struct sigaction alarm_action;

   if (tmo == 0) {
     /* clear the timer and return */
     alarm(0);
   }
   else {
     /* Set up the signal and timer for timeout. Use POSIX signals because */
     /* Linux cannot seem to get the include files correct for typing the  */
     /* handler.                */
     alarm_action.sa_handler = to_alarm; /* to_alarm is the timeout handler */
     sigemptyset(&alarm_action.sa_mask); /* set no masks for sigaction      */
     alarm_action.sa_flags = 0;          /* set no flags                    */
     sigaction(SIGALRM, &alarm_action,NULL); /* Install the handler */
     tout_flg=0;                         /* global flag to signify timeout */
     alarm(tmo);                         /* Set an alarm for tmo seconds   */
   }
   return;
}
/******************************************************************************
pkt_recv - wait for a packet from a server. 
User specifies the buffer to receive the data, the protocol for listening 
and a timeout after which an error return takes place.  Bad input data 
causes the routine exit.  Timeout returns with error.
******************************************************************************/
int pkt_recv(struct UDP_Packet *in, int proto, int tmo)
{


   int status, rpy_len;
//   struct sigaction alarm_action;

   errno=0;

   /* Check the DataSize for sensibility */
   if (in->DataSize < 0 || in->DataSize > MAX_ORPH_DATA)
     {
       fprintf(stderr,"pkt_recv: Ether receive buffer size out_of_range\n");
       errno=EINVAL;
       return(-1);
     }
   rpy_len = in->DataSize + PKTHDRLEN;

   /* Check the protocol number for validity */
   if (proto < DATA || proto >= MAX_ORPH_PROTO)
     {
       fprintf(stderr,"pkt_send: UDP socket out_of_range\n");
       errno=EINVAL;
       return(-1);
     }
   serv_addr.sin_port = htons(protsock(proto)); /* Set the protocol */

   if (tmo < 0 || tmo > MAX_TIMEOUT) { 
     fprintf(stderr,"pkt_send: requested timeout out_of_range\n");
     errno=EINVAL;
     return(-1);
   }
   
   start_timer(tmo);
   
   /* Now listen for the reply  */ 
   status = recvfrom(sockfd,(void *)in, rpy_len,
		     0, (struct sockaddr *) 0, (socklen_t *) 0);
   if (status <= 0) {
     if (tout_flg) {    /* The recvfrom timed out */
       /* No message to be quiet during the retry */
       //       fprintf(stderr,"pkt_recv - VME recvfrom timed out\n");
       errno=ETIMEDOUT;
       start_timer(0);
       return(-1);
     }
     else  {           /* some other error, probably EINTR */
       perror(" pkt_recv - VME - recvfrom error");
       start_timer(0);
       return(-1);
     }
   }
   
      
   /* Successful return */
   start_timer(0);   /* Turn off the alarm */

   return (0);
}
