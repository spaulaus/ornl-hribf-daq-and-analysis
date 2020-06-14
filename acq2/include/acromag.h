/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1992
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
*    File:         /usr/users/mcsq/Dvme3/acromag.h
*
*    Description:  Include file for the Acromag Model 9480 Digital I/O
*                  module.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/19/92    MCSQ         
*****************************************************************************/
#ifndef  ACROMAG_H_
#define  ACROMAG_H_

/*  Registers in the Acromag Model 9480 Digital I/O Module                    */
struct acromag {
         unsigned char dum1[128];
volatile unsigned char ip_clr;    /* Interrupt pending/ interrupt clear reg   */
volatile unsigned char csr;       /* Control and status register              */
         unsigned char intr_in;   /* Interrupt inputs                         */
         unsigned char intr_ena;  /* Interrupt enables                        */
         unsigned char intr_level; /* VMEbus interrupt level                  */
         unsigned char intr_pol;  /* Interrupt input polarity reg             */
         unsigned char dum2;
         unsigned char intr_vec0; /* Interrupt vector - point 0               */
         unsigned char dum3;
         unsigned char intr_vec1; /* Interrupt vector - point 1               */
         unsigned char dum4;
         unsigned char intr_vec2; /* Interrupt vector - point 2               */
         unsigned char dum5;
         unsigned char intr_vec3; /* Interrupt vector - point 3               */
         unsigned char dum6;
         unsigned char intr_vec4; /* Interrupt vector - point 4               */
         unsigned char dum7;
         unsigned char intr_vec5; /* Interrupt vector - point 5               */
         unsigned char dum8;
         unsigned char intr_vec6; /* Interrupt vector - point 6               */
         unsigned char dum9;
         unsigned char intr_vec7; /* Interrupt vector - point 7               */
         unsigned char dum10[106];
volatile unsigned char port0;     /* I/O port 0, interrupt port               */
volatile unsigned char port1;
volatile unsigned char port2;
volatile unsigned char port3;
volatile unsigned char port4;
volatile unsigned char port5;
volatile unsigned char port6;
volatile unsigned char port7;
};

/*
*    Control and status register (csr) bit definitions.
*/
#define  AVME_RESET     0x10	/* Reset module - write only                  */
#define  AVME_INTR_ENA     8	/* Interrupt enable                           */
#define  AVME_INTR_PEND    4	/* Interrupt pending - read only              */
#define  AVME_GRN_ON       2	/* LED control bits                           */
#define  AVME_GRN_OFF      0
#define  AVME_RED_ON       0
#define  AVME_RED_OFF      1


#endif             /* end ACROMAG_H_      */
