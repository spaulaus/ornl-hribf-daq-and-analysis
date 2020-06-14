/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1994
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
*    File:         /usr/users/mcsq/Dvme3/lrs1190.h
*
*    Description:  Include file for LeCroy Research Corp. model 1190 VME
*                  Dual Port Memory.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/21/94     MCSQ         
*
*****************************************************************************/

#ifndef  LRS1190_H_
#define  LRS1190_H_


struct LRS1190 {
           unsigned short dat[65536];  /* Internal data memory            */
  volatile unsigned short addr;        /* Address counter                 */
           unsigned short mode;        /* Mode register                   */
 } ;

/*  addr is a READ_ONLY register.  However, the address register is loaded
 *  during a read of data memory.  Reading dat[0] leaves the address counter
 *  set to zero.
 *
 *  The mode register is a WRITE_ONLY register.  Writing a "1" to the mode
 *  register enables the Front Panel input.  Writing a "0" enables VME 
 *  access.
 *
 *  CAUTION:  When the Front Panel is enabled, only the mode register is
 *            addressable from the VME port!.
 */

#endif      /* end  LRS1190_H_   */
