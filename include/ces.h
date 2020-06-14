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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/ces.h
*
*    Description:  Parameters for the CES 8170 high speed memory module.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/25/93    MCSQ         
*****************************************************************************/
struct hsm {
     unsigned short mem[0x80000];
     volatile uint32_t intr;
     volatile uint32_t ctrl;
     volatile uint32_t mem_ptr;
     volatile uint32_t wrd_cnt;
      };
