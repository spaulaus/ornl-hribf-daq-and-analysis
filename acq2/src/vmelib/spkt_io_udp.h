/*****************************************************************************
******************************************************************************
*
*    Environment:  VME Acquisition for Linux
*
*    File:         spkt_io_udp.h
*
*    Description:  
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/20/02    MCSQ
*
*    3/18/03    MCSQ
*
*    6/22/05    RLV  Modify for server UDP version
*****************************************************************************/
#ifndef  sPKT_IO_UDP_H_
#define  sPKT_IO_UDP_H_

#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netinet/in.h>
#include  <netdb.h>
#include  <arpa/inet.h>
#include  <net/if.h>
#include  <unistd.h>
#include  "orph_udp.h"

/*  Function prototypes             */
int spkt_echo(struct UDP_Packet *, int);
int spkt_open(char *, int);
int spkt_close(void);
int spkt_send(struct UDP_Packet *);
int spkt_recv(struct UDP_Packet *);



extern int h_errno;              /* global host error number */
extern int errno;

#endif     /* end SPKT_IO_UDP_H_    */
