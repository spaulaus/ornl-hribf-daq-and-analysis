/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2005
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
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/resetxx.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/26/05    MCSQ         
*
*****************************************************************************/
#include "../include/orph_udp.h"

#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <bsp.h>

/*    Function Prototypes        */


/*      Global data              */


/*****************************************************************************
*****************************************************************************/
void resetxx(void)
{
  static struct sockaddr_in cli_addr,serv_addr;
  int   sockfd,status;
  socklen_t   clilen;
  static struct UDP_Packet in_buf;
  int    size;
  rtems_interval ticks;

  sockfd = socket(AF_INET,SOCK_DGRAM,0);
  if (sockfd == -1) {perror("resetxx - socket error"); exit(1);}
  memset((char *) &serv_addr,0,sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(45000+PROTO_FORCE_BOOT);
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
  if (status == -1) {perror("resetxx - bind"); exit(1);}

  clilen = sizeof(cli_addr);
  size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                (struct sockaddr *)&cli_addr,&clilen);
  if (size < 0) {
    fprintf(stderr,"\nEthernet read error\n");
    exit(0);
  }

/*****************
printf("Reset packet received\n");
*****************/

  status=sendto(sockfd,&in_buf,size,0,(struct sockaddr *)&cli_addr,clilen);
  if (status < 0) {
     perror("resetxx - error at sendto");
     fprintf(stderr,"sockfd=%i, size=%i, clilen=%i\n",sockfd,size,(int)clilen);
  }

/*****************
printf("Reply to reset packet\n");
*****************/

  close(sockfd);

/*
*   Need time delay here
*/
  ticks = 300;  /* Ten seconds */
  rtems_task_wake_after(ticks);

  bsp_reset();
}
