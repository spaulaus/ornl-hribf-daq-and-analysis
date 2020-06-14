/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
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
*    File:         /usr/users/mcsq/Dacq/camerr.c
*
*    Description:  CAMERR is called by CAMACIO to report a CAMAC error.
*                  Errors are reported to standard error in the 
*                  workstation enviroment.  However, users may provide
*                  a private version of CAMERR as long as it appears
*                  before the library /usr/users/mcsq/Dacq/vmelib.a
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/23/93    MCSQ        Original
*
*****************************************************************************/

void camerr_(char *ipb,int *stat)
{
   ipberr_(ipb,stat);
}
