/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1997
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
*    File:         /usr/users/mcsq/Dvme3/vmic3128.h
*
*    Description:  Include file for a VME Microsystems International Corp.
*                  Model 3128, 64 Channel, 14-bit ADC Module.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/18/97    MCSQ         
*
*****************************************************************************/
#ifndef  VMIC3128_H_
#define  VMIC3128_H_

/*
*   Registers for the VMIC 3128 ADC
*/
struct vmic3128_adc {
         unsigned short id;       /* Board ID word                          */
volatile unsigned short csr;      /* Control and Status register            */
         unsigned short bcr;      /* Buffer control                         */
         unsigned short dum1[5];  /* reserved                               */
         unsigned short icr;      /* Interrupt control register             */
         unsigned short dum2[3];  /* reserved                               */
         unsigned short ivr;      /* Interrupt vector register              */
         unsigned short dum3[3];  /* reserved                               */
         unsigned short timer0;   /* timer 0                                */
         unsigned short timer1;   /* timer 1                                */
         unsigned short dcr;      /* Data counter                           */
         unsigned short tcr;      /* Timer control register                 */
         unsigned short dum4[44]; /* reserved                               */
         unsigned short buff[1024]; /* Data buffer                          */
                  short ram[960];   /* scratch pad memory                   */

};

#define  V3128_LED_OFF         0x8000
#define  V3128_START_CNV       0x0100
#define  V3128_TIMER_START     0x0200
#define  V3128_SOFT_RESET      0x4000
#define  V3128_SCAN_INTR       0x0100
#define  V3128_SET_GAIN        0xc0
#define  V3128_SET_SCANM       0x80
#define  V3128_SET_RAND        0x40

#define  V3128_NEW_DATA        0x1000
#define  V3128_END_SCAN        0x2000

#define  V3128_ADC_INTR_ENA    0x1d   /* level 5 */

#endif     /* end VMIC3128_H_    */
