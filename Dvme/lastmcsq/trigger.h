/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-1994
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
*    File:         /usr/users/mcsq/Dvme3/trigger.h
*
*    Description:  Include file for Event trigger I/O module.  This is a
*                  custom module based on a VME prototype board Model
*                  V-WRAP-A110 from:
*                  American ELTEC, Inc.
*                  4340 Stevens Creek Blvd., Suite 204
*                  San Jose, CA  95129
*                  (408) 244-4700
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/28/92    MCSQ         
*
*    7/11/93    MCSQ        Corrected ORNL_COUNT pointer definition.
*                           Changed name of I/O register bit 4 from
*                           ORNL_SPARE to ORNL_STOPPED.
*
*    3/17/94    MCSQ        Moved the hardware address definition to
*                           vme_sys.h were all the others are.
*****************************************************************************/
#ifndef  TRIGGER_H_
#define  TRIGGER_H_

#ifndef  VME_SYS_H_
#define  VME_SYS_H_
#include "vme_sys.h"
#endif

/*  Register structure for the TS68901 Multi-Function Peripheral chip
*   on the ELTEC module.
*/
struct trig {
         unsigned char dum1;
         unsigned char gpip;      /* General Purpose I/O register             */
         unsigned char dum2;
         unsigned char aer;       /* Active edge register                     */
         unsigned char dum3;
         unsigned char ddr;       /* Data Direction register                  */
         unsigned char dum4;
         unsigned char iera;      /* Interrupt enable register A              */
         unsigned char dum5;
         unsigned char ierb;      /* Interrupt enable register B              */
         unsigned char dum6;
volatile unsigned char ipra;      /* Interrupt pending register A             */
         unsigned char dum7;
volatile unsigned char iprb;      /* Interrupt pending register B             */
         unsigned char dum8;
volatile unsigned char isra;      /* Interrupt in-service register A          */
         unsigned char dum9;
volatile unsigned char isrb;      /* Interrupt in-service register B          */
         unsigned char dum10;
         unsigned char imra;      /* Interrupt mask register A                */
         unsigned char dum11;
         unsigned char imrb;      /* Interrupt mask register B                */
         unsigned char dum12;
         unsigned char vr;        /* Interrupt vector register                */
         unsigned char dum13;
         unsigned char tacr;      /* Timer A control register                 */
         unsigned char dum14;
         unsigned char tbcr;      /* Timer B control register                 */
         unsigned char dum15;
         unsigned char tcdcr;     /* Timers C & D control register            */
         unsigned char dum16;
volatile unsigned char tadr;      /* Timer A data register                    */
         unsigned char dum17;
volatile unsigned char tbdr;      /* Timer B data register                    */
         unsigned char dum18;
volatile unsigned char tcdr;      /* Timer C data register                    */
         unsigned char dum19;
volatile unsigned char tddr;      /* Timer D data register                    */
         unsigned char dum20;
         unsigned char scr;       /* Synchronous character register           */
         unsigned char dum21;
         unsigned char ucr;       /* USART control register                   */
         unsigned char dum22;
volatile unsigned char rsr;       /* Receiver status register                 */
         unsigned char dum23;
volatile unsigned char tsr;       /* Transmitter status register              */
         unsigned char dum24;
volatile unsigned char udr;       /* USART data register                      */
         unsigned char duma[17];
volatile unsigned char event_clear;
         unsigned char dumb[63];
volatile unsigned char delay_time;
};

/*
*   Interrupt enable codes
*/
#define  EVT_INTR_ENA    0x80         /* Enable Event interrupt               */
#define  TIMER_INTR_ENA  0x20         /* Timer A interrupt enable             */
/*
*   General purpose I/O register bits
*/
#define  ORNL_CLEAR   1
#define  ORNL_BUSY    2
#define  ORNL_STOPPED 4

#define  ORNL_COUNT  (TRIGGER + 0x81)

#endif             /* end TRIGGER_H_      */
