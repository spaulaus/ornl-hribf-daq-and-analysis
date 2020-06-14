/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 2001-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmeio.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/ 8/01    MCSQ
*
*    7/27/02    MCSQ        Linux version       
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <errno.h>  
#include  "pkt_io_udp.h"
#include  "orph_udp.h"
#include  "vmexx.h"

/*****************************************************************************
*   Send command packet and receive reply from the VME processor.
*
*  Call:   len  - Number of data bytes in packet
*
*  Return:  0  -  OK
*          ETHER_RECEIVE -  No reply from the VME processor
*          ETHER_TRANSMIT -  Packet transmission error
*          ETHER_OPEN -  Packet filter open error
*****************************************************************************/
int  vmeio(unsigned char *xbuf, int xlen, unsigned char *rbuf, int *rlen)
{
     //int status,i;
     int status;
     static struct UDP_Packet out,in;

     *rlen = 0;
     out.DataSize = xlen;
     in.DataSize = xlen;
     memcpy(out.Data,xbuf,xlen);
     status = pkt_io(&out,&in,VMEIO,1);
     if (status) {
       perror("vmeio");
       switch (errno) {
       case ETIMEDOUT: 
	 return (ETHER_RECEIVE);
	 break;
       case EPKTSEQ:
	 return (ETHER_TRANSMIT);
	 break;
       }
     }
     *rlen = in.DataSize;
     memcpy(rbuf,in.Data,in.DataSize);
     if (status != 0) return(status);
     status = ((struct vmeio_stat *)in.Data)->rpystat;
     return (status);
}
