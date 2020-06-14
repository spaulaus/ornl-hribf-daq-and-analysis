/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dacq/for_acq_ctrl.c
*
*    Description:  FORTRAN callable routine for control of front-end
*                  VME acquisition system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/31/93    MCSQ        FORTRAN callable routine adapted from
*                           the standalone program acq_run.c
*
*    4/25/93    MCSQ        Changed the returned status.  Now negative
*                           values are always errors and positive values
*                           mean successful operation.  In the case of
*                           a status request, the status is positive and
*                           nonzero.
*
*    9/25/95    MCSQ       New ethernet library in /usr/users/mcsq/Dlan.
*
*    9/24/03    MCSQ       Now just a dummy routine which calls
*                          acq_vme_ctrl.
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/****************************************************************************
*  FORTRAN callable subroutine:
*
*        CALL FOR_ACQ_CTRL(ISTAT,STRING)
*
*  Call:   string = ASCII command string.  Commands are start, stop, init
*                   and status.
*
*  Return  istat = positive means successful operation
*                  negative means error.
*
*                  Error and status codes are defined in the file
*                  /usr/users/mcsq/Dlinux/Dacq/for_acq_ctrl.for.
*
****************************************************************************/
int for_acq_ctrl__(int* istat,char *for_cmd,int for_len)
{
    acq_vme_ctrl_(istat,for_cmd,for_len);
}
int for_acq_ctrl_(int* istat,char *for_cmd,int for_len)
{
    acq_vme_ctrl_(istat,for_cmd,for_len);
}
