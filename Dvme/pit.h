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
*    File:         /usr/users/mcsq/Dvme3/pit.h
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/ 9/93    MCSQ         
*****************************************************************************/
#ifndef  PIT_H_
#define  PIT_H_


struct pit_regs {
 volatile unsigned char pgcr;   /* Port general control reg.                */
 volatile unsigned char psrr;   /* Port service request reg.                */
          unsigned char paddr;  /* Port A direction register                */
          unsigned char pbddr;  /* Port B direction register                */
          unsigned char pcddr;  /* Port C direction register                */
          unsigned char pivr;   /* Port interrupt vector                    */
          unsigned char pacr;   /* Port A control register                  */
          unsigned char pbcr;   /* Port B control reigster                  */
 volatile unsigned char padr;   /* Port A data register                     */
 volatile unsigned char pbdr;   /* Port B data register                     */
 volatile unsigned char paar;   /* Port A alternate register                */
 volatile unsigned char pbar;   /* Port B alternate register                */
 volatile unsigned char pcdr;   /* Port C data register                     */
 volatile unsigned char psr;    /* Port status register                     */
                   char dum1;
                   char dum2;
          unsigned char tcr;    /* Timer control register                   */
          unsigned char tivr;   /* Timer interrupt vector                   */
                   char dum3;
          unsigned char cprh;   /* Timer preload register high              */
          unsigned char cprm;   /* Timer preload register middle            */
          unsigned char cprl;   /* Timer preload register low               */
                   char dum4;
 volatile unsigned char cntrh;  /* Counter register high                    */
 volatile unsigned char cntrm;  /* Counter register middle                  */
 volatile unsigned char cntrl;  /* Counter register low                     */
 volatile unsigned char tsr;    /* Timer status register                    */
 };

#endif             /* end PIT_H_      */
