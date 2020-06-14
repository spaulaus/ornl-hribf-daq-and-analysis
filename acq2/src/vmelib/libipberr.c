/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992,1993
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
*    File:         /usr/users/mcsq/Dacq/ipberr.c
*
*    Description:  IPBERR is called by BHIO to report a CAMAC error.
*                  Errors are reported to standard error in the 
*                  workstation enviroment.  However, users may provide
*                  a private version of IPBERR as long as it appears
*                  before the library /usr/users/mcsq/Dacq/pcam.a
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/26/92    MCSQ        Original
*
*    4/23/93    MCSQ      Put in messages for timeout and all illegal
*                         parameter errors.
*****************************************************************************/
#include <stdio.h>

void ipberr_(char *ipb,int *stat)
{
  fprintf(stderr,"CAMAC Error - C,N,A,F =");
  fprintf(stderr,"%3i%3i%3i%3i  ",ipb[0],ipb[1],ipb[2],ipb[3]);
  if (*stat == 0xa000)
    {
      fprintf(stderr,"Crate Off_Line\n");
    }
  else if (*stat <= 3)
    {
     if (*stat == 1) fprintf(stderr,"GOOD X  -  BAD Q\n");
     else if (*stat == 2) fprintf(stderr,"GOOD Q  -  BAD X\n");
     else  fprintf(stderr,"BAD Q  -  BAD X\n");
    } 
  else if (*stat == 0x80) fprintf(stderr,"Time Out\n");
  else if (*stat == 0xc004) fprintf(stderr,"Illegal Crate\n");
  else if (*stat == 0xc005) fprintf(stderr,"Illegal Module\n");
  else if (*stat == 0xc006) fprintf(stderr,"Illegal Subaddress\n");
  else if (*stat == 0xc009) fprintf(stderr,"Illegal Function\n");
  else
    {
      fprintf(stderr,"Status = %x\n",*stat);
    }
}
