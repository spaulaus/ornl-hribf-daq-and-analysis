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
*    File:         /usr/users/mcsq/Dlinux/Dacq/pkt_io.h
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
*****************************************************************************/
#ifndef  PKT_IO_H_
#define  PKT_IO_H_

#include  <sys/types.h>
#include  <sys/socket.h>
#include  <netinet/in.h>
#include  <netdb.h>
#include  <arpa/inet.h>
#include  <net/if.h>
#include  <unistd.h>
#include  "../Dlan/orph_pf.h"
#include  "../Dlan/vmedII.h"

/*  Function prototypes             */
int pkt_io(struct Vmed *,struct Vmed * ,int ,int);
int readn(int ,unsigned char *,int );
int writen(int ,unsigned char *,int );
int pkt_open(struct Vmed *,struct Vmed *,int ,int);
void pkt_close(void);
int pkt_send(struct Vmed *);
int pkt_recv(struct Vmed *);


extern int h_errno;              /* global host error number */
extern int errno;

#endif     /* end PKT_IO_H_    */
