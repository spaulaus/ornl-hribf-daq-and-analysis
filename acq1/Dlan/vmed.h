/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                              Copyright(C) 2002
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
*    File:         /usr/users/mcsq/Dlinux/Dlan/vmed.h
*
*    Description:  Data structures VME Server 
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/20/02    MCSQ
*****************************************************************************/
#ifndef  VMED_H_
#define  VMED_H_

/*
*   An Ethernet packet has a maximum data size of 1500 bytes.  The IP
*   header is 20 bytes and the TCP header is 20 bytes.  This means that
*   the size of struct Vmed is 1460.
*/

#define  MAX_TCPDATA  1460

struct  Vmed {
  struct Vmed_hdr {
          short len;                 /* Number of data bytes in buf         */
          short error;               /* Error code returned by server       */
  };
 unsigned char  buf[MAX_TCPDATA-4];  /* Data buffer                         */
};

struct  Vme_start {
          char server[16];           /* VME processor name                  */
          int  proto;                /* Protocal type                       */
          int  cmd_len;              /* Length of command data packet       */
          int  rpy_len;              /* Length of reply data packet         */
};


#endif     /* end VMED_H_    */
