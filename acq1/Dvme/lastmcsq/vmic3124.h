/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1999
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
*    Environment: RMS Control System
*
*    File:         /usr/users/mcsq/Dvme3/vmic3124.h
*
*    Description:  Include file for a VME Microsystems International Corp.
*                  Model 3124, 32 Channel, 12-bit ADC Module.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/16/99    MCSQ         
*
*****************************************************************************/
#ifndef  VMIC3124_H_
#define  VMIC3124_H_

/*
*   Registers for the VMIC 3124 ADC
*/
struct vmic3124_adc {
         unsigned char  id;       /* Board ID word                          */
         unsigned char  cfr;      /* Configuration register                 */
volatile unsigned char  csr;      /* Control and Status register            */
         unsigned char  cpr;      /* Channel Pointer register               */
         unsigned short dum1[30]; /* reserved                               */
                  short data[32]; /* ADC data array                         */
};

#define  VMIC3124_LED_OFF     0x80

#endif     /* end VMIC3124_H_    */
