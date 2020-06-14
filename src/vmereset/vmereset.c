/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmereset.c
*
*    Description:  Routine to reset the VME processor.  This should cause
*                  the VME processor to start t's Ethernet loader.  The
*                  host should receive a boot request packet every 10 seconds.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/20/92    MCSQ         Original
*
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    5/ 1/93    MCSQ        VME processors now use a multicast to request
*                           boot by host.
*
*    4/10/96    MCSQ        Add check of the source address in the packet
*                           filter so that we only get replys from the
*                           the processor we reset.
*
*    7/31/02    MCSQ        Adapted from Alpha version
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*****************************************************************************/
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pkt_io_udp.h"
#include "orph_udp.h"

/*
*    Global variables
*/
struct UDP_Packet rbuf,xbuf;

/***************************************************************************
*
***************************************************************************/
int main(int argc, char *argv[])
{
   int status;

/*
*   Send a reboot request to the VME processor
*/
   xbuf.DataSize = 10;
   rbuf.DataSize = 100;
   status = pkt_io(&xbuf,&rbuf,FORCE_BOOT,11);
   if (status != 0)
     {
/************
       if (status == ETHER_OPEN) printf("vmereset: Ethernet open failure\n");
       else if (status == ETHER_RECEIVE)
                                 printf("vmereset: Ethernet receive timeout\n");
       exit(99);
************/
     }
   return(0);
}
