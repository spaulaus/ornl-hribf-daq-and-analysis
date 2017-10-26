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
*    File: /tera/varner/Acq-development/tcpip/pkt_io/pkt_io_udp.h
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
*    3/18/03    MCSQ
*    10/19/05   RLV - modified the prototypes for the new UDP library
*****************************************************************************/
#ifndef  PKT_IO_UDP_H_
#define  PKT_IO_UDP_H_

#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netinet/in.h>
#include  <netdb.h>
#include  <arpa/inet.h>
#include  <net/if.h>
#include  <unistd.h>
#include  "orph_udp.h"

/*  Function prototypes             */
int pkt_io(struct UDP_Packet *, struct UDP_Packet *, int, int);
int pkt_open(char *);
int pkt_close(void);
int pkt_send(struct UDP_Packet *out, int);
int pkt_recv(struct UDP_Packet *in, int, int);



extern int h_errno;              /* global host error number */
extern int errno;

/* Custom errno */
#define EPKTSEQ 131       /* Packet received out of sequence */

/* Error codes expected by MCSQ orpas programs */
/* These are only returned by pkt_io           */
#define ETHER_RECEIVE  -32
#define ETHER_TRANSMIT -33
#define ETHER_OPEN     -34
#define ETHER_TIMEOUT  -35
#define ETHER_SEQUENCE -36

#endif     /* end PKT_IO_UDP_H_    */
