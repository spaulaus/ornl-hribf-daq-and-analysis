/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1996
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
*    File:         /usr/users/mcsq/Dvme3/vmic3113a.h
*
*    Description:  Include file for a VME Microsystems International Corp.
*                  Model 3113A, 64 Channel, 12-bit ADC Module.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/22/96    MCSQ         
*
*****************************************************************************/
#ifndef  VMIC3113A_H_
#define  VMIC3113A_H_

/*
*   Registers for the VMIC 3313A ADC
*/
struct vmic_adc {
         unsigned short id;       /* Board ID word                          */
volatile unsigned short csr;      /* Control and Status register            */
         unsigned short cfr;      /* Configuration register                 */
         unsigned short dum1[5];  /* reserved                               */
         unsigned short icr;      /* Interrupt control register             */
         unsigned short dum2[3];  /* reserved                               */
         unsigned short ivr;      /* Interrupt vector register              */
         unsigned short dum3[3];  /* reserved                               */
         unsigned short timer0;   /* timer 0                                */
         unsigned short timer1;   /* timer 1                                */
         unsigned short timer2;   /* timer 2                                */
         unsigned short tcr;      /* Timer control register                 */
         unsigned short dum4[4];  /* reserved                               */
                  short rcdr;     /* ramdom conversion data register        */
                  short dum5[39]; /* reserved                               */
                  short data[64]; /* ADC data array                         */
};

#define  VMIC_LED_OFF         0x4000
#define  VMIC_START_CNV       0x2000
#define  VMIC_TIMER_START     0x0400
#define  VMIC_SOFT_RESET      0x0200
#define  VMIC_SCAN_INTR       0x0100

#define  VMIC_NEW_DATA        0x8000
#define  VMIC_END_SCAN        0x2000

#define  VMIC_ADC_INTR_ENA    0x1d   /* level 5 */

#endif     /* end VMIC3113A_H_    */
