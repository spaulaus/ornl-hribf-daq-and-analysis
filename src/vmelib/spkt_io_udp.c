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
#include "spkt_io_udp.h"


static int sockfd = -1;
//static char server[16] = "vme";
static struct sockaddr_in serv_addr;
static struct sockaddr_in cli_addr;

/******************************************************************************
spkt_echo - utility to receive a packet from a client and echo it back. 
******************************************************************************/
int  spkt_echo(struct UDP_Packet *in,int proto)
{
  int status;
  char server[6]="";
  
  if(sockfd == -1) {
    spkt_open(server,proto);
  }
  
  status = spkt_recv(in);
  if (status < 0) {
    perror(" spkt_echo - mysterious recv error");
    return(-1);
  }
  
  status = spkt_send(in);
  if (status < 0) {
    perror(" spkt_echo - mysterious send error");
    return(-1);
  }
  
  status = 0;
  return (status);
}
/******************************************************************************
spkt_open - open a socket and prepare for being a server
******************************************************************************/

int spkt_open(char *node, int proto)
{
//  int status;
  
  /* This routine only executes if there is not an open socket
     which supports the use of the spkt_echo program. */

  if (sockfd == -1) {
//    char *cptr;
//    struct hostent *hostptr;
//    struct in_addr address,*inadr;
    
    
    /* We obtain a socket, then bind the program to the local socket 
       for listening to any address. This program is the server.
    */
    
    sockfd = socket(AF_INET,SOCK_DGRAM,0);
    if (sockfd == -1) {
      perror("sPkt_open: socket creation error");
      return(-1);
    }
    /* Bind the socket for listening on server socket */
    bzero((char *) &serv_addr, sizeof(serv_addr));    /* zero out */
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port        = htons(protsock(proto));
    if (bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0)
      {
	perror("sPkt_open: cannot bind local address");
	return(-1);
      }
  }            /* End of sockfd test */
  return(0);  
}               /* End of spkt_open   */
/******************************************************************************
spkt_close - closes the socket specified in the global "sockfd".  No errors, 
no return value.
******************************************************************************/
int spkt_close(void)
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
spkt_send - sends a UDP_packet to a client, using a protocol specified by 
the user, on the socket specified in the global "sockfd".  Checks for 
valid DataSize in the packet and protocol in range. Returns 0 on successful 
completion. 
******************************************************************************/
int spkt_send(struct UDP_Packet *out)
{
  int status, cmd_len;
  
  errno=0;
  
  /* Check the DataSize for validity */
  if (out->DataSize < 0 || out->DataSize > MAX_ORPH_DATA) {
    printf("sPkt_send: Ether transmit buffer size out_of_range\n");
    errno=EINVAL;
    return(-1);
  }
  cmd_len = out->DataSize + PKTHDRLEN; /* Save the command length */
  status = sendto(sockfd, (void *) out, cmd_len, 0,
                  (struct sockaddr *) &cli_addr, sizeof(cli_addr));
  if (status != cmd_len) { /* Error, return sendto errno  */ 
    perror("sPkt_send: sendto incomplete ");
    return(-1);
  }
  return(0);
}
/******************************************************************************
spkt_recv - 
wait for a packet from a client. User specifies the buffer to receive 
the data.  The protocol is already bound to the socket, and there is 
no timeout.  Bad input data causes the routine exit.  

Return:
      -1 : error in recvfrom
       0 : all is ok
       1 : interrupted by signal, probably just needs to restart

******************************************************************************/
int spkt_recv(struct UDP_Packet *in)
{
  int status, rpy_len;
  socklen_t cli_size;
  //struct sigaction alarm_action;
  
  errno = 0;
  
  /* Check the DataSize for sensibility */
  if (in->DataSize < 0 || in->DataSize > MAX_ORPH_DATA) {
    fprintf(stderr,
       "sPkt_recv: Ether receive buffer %i size out_of_range\n",
        in->DataSize);
    errno=EINVAL;
    return(-1);
  }
  
  rpy_len = MAX_ORPH_DATA + PKTHDRLEN;
  
  
  /* Now listen for requests, saving the client address  */ 
  bzero((char *) &cli_addr, sizeof(cli_addr));    /* zero incoming address */
  cli_size = sizeof(cli_addr);
  status = recvfrom(sockfd,(void *)in, rpy_len, 0,
                    (struct sockaddr *) &cli_addr, &cli_size);
  if (status <= 0) {
    if (errno == EINTR) {  /* interrupted recvfrom, return quietly */
      return(1);
    }
    else {
      perror("spkt_recv: recvfrom error");
      return(-1);
    }
  }
  /* Did we receive a consistent packet */
  if (status != in->DataSize+PKTHDRLEN) {
    fprintf(stderr,"sPkt_recv: data size inconsistent\n");
    fprintf(stderr,"Received %i, internal size %i\n",
	    status,
	    in->DataSize+PKTHDRLEN);
    errno=EINVAL;
    return(-1);
  }
  
  return (0);
}
