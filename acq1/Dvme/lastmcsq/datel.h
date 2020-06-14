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
*    File:         /usr/users/mcsq/Dvme3/datel.h
*
*    Description:  Include file for DATEL 613 16-bit ADC and DATEL 626 16-bit
*                  DAC.  Modules used in RMS control system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/18/94     MCSQ         
*
*****************************************************************************/

#ifndef  DATEL_H_
#define  DATEL_H_

/*
 *  Register structure for the DATEL 626 16-bit DAC and DATEL 628
 *  12-bit DAC.  NOTE that the 626 only has 6 DACs.
 */
struct datel_dac_regs  {
                unsigned char id[64];
                         char dum[96];
                unsigned short dac0;
                unsigned short dac1;
                unsigned short dac2;
                unsigned short dac3;
                unsigned short dac4;
                unsigned short dac5;
                unsigned short dac6;
                unsigned short dac7;
} ;

/*
 *  Register structure for the DATEL 613 16-bit ADC
 */
struct datel_adc_regs {
        volatile unsigned short csr;
                 unsigned short addr;    /* write only */
                 unsigned short start;   /* write only */
        volatile unsigned short data;    /* read only  */
        volatile unsigned short vector;
                 unsigned short cal;     /* write only */
                 unsigned short out;     /* write only */
        volatile unsigned short in;      /* read only  */
} ;

#define  EOC_613       0x8000   /* End of Conversion flag               */
#define  OVR_SMP_613   0x100    /* Conversion start while ADC busy      */
#define  INT_ENB_613   0x80     /* Interrupt enable                     */
#define  LED_ON_613    0x40     /* Front panel LED on                   */
#define  CNV_ENB_613   0x4      /* Conversion enable                    */
#define  START_REG_613  0       /* Start conversion on write start reg  */
#define  DATA_REG_613   1       /* Start conversion on read data reg    */
#define  TIMER_613      2       /* Start conversion on internal timer   */
#define  EXT_TRG_613    3       /* Start conversion on external trigger */

#endif      /* end  DATEL_H_   */
