/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-2001
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
*    File:         /usr/users/mcsq/Dvme3/devices.h
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/29/92    MCSQ         
*
*    5/ 1/94    MCSQ       Added the VMIC 6015 quad-serial interfaces
*                          for RMS control system.
*
*    7/21/94    MCSQ       Added LeCroy 1190 Dual Port Memory module.
*
*    9/29/94    MCSQ       Temp addition of ITG1300 and DATEL628
*                          modules for RMS control system.
*
*   11/22/94    MCSQ       Change DATEL628 to DATEL626 and add DATEL613
*                          for RMS control system.
*
*    3/25/95    MCSQ       Add Green Spring stepper motor controller.
*
*    5/27/95    MCSQ       New modules for DRS control system.
*
*   10/16/95    MCSQ       Add DATEL 628 12-bit DAC
*
*    6/24/96    MCSQ       Add VMIC 3113A 12-bit ADC
*
*    2/17/97    MCSQ       Add three LRS1190 interfaces.  Add VMIC 3128
*                          64 channel ADC for DRS.
*
*   12/13/97    MCSQ       Add a second KSC 2917 CAMAC interface
*
*   12/ 1/98    MCSQ       New VME interface to CAMAC
*
*    2/ 8/99    MCSQ       Add VMIC 3124 ADC for LN fill system
*
*    4/ 4/01    MCSQ       Add 10 CAEN V785 ADCs
*
*    4/11/01    MCSQ       Add 10 CAEN V775 TDCs
*    6/14/06    MCSQ       Add SIS3820 VME Scaler
*    2008       RLV        Add V775 and V785 11 and 12
*    30 July 2009 RLV      Add V792
*    31 July 2015 RLV      Add ANL/Gammasphere MyRIAD module
*****************************************************************************/
#ifndef  DEVICES_H_
#define  DEVICES_H_

#include <stddef.h>
#include "../include/vme_sys.h"

/*
*   Table of flags which indicate which devices were located at run-time.
*   The code devchk.c checks for a device present and sets the flag
*   nonzero if the device is found or to zero if the device does not
*   respond.
*
*   The table is stored in the system ram and can be accessed by 
*   any task in the VME processor.
*/
struct devices {
  char ksc2917a;     /* CAMAC interface module                       */
  char ksc3982;      /* List sequencer module                        */
  char lrs1131;      /* FASTBUS interface module                     */
  char lrs1821;      /* FASTBUS sequencer                            */
  char ces8170;      /* FERA interface module                        */
  char acromag;      /* Digital I/O module                           */
  char trigger;      /* ORNL trigger module                          */
  char myriad;       /* ANL Myriad time/trigger module               */
  char vmic6015a;    /* Quad-Serial I/O #1                           */
  char vmic6015b;    /* Quad-Serial I/O #2                           */
  char lrs1190a;     /* LeCroy FERA interface  #1                    */
  char datel626a;    /* RMS 16-bit DAC                               */
  char datel613a;    /* RMS 16-bit ADC                               */
  char datel613b;    /* RMS 16-bit ADC                               */
  char stepper;      /* Stepper motor controller                     */
  char datel628a;    /* RMS 12-bit DAC                               */
  char vmic3113a;    /* LN fill ADC                                  */
  char vmic3128a;    /* DRS 64 channel 14-bit ADC                    */
  char lrs1190b;     /* LeCroy FERA interface  #2                    */
  char lrs1190c;     /* LeCroy FERA interface  #3                    */
  char lrs1190d;     /* LeCroy FERA interface  #4                    */
  char ksc2917b;     /* Second CAMAC interface module                */
  char ornlaux;      /* ORNL Fast CAMAC interface                    */
  char vmic3124;     /* LN fill ADC                                  */
  char caen785_1;    /* First CAEN V785 ADC                          */
  char caen785_2;
  char caen785_3;
  char caen785_4;
  char caen785_5;
  char caen785_6;
  char caen785_7;
  char caen785_8;
  char caen785_9;
  char caen785_10;
  char caen785_11;
  char caen785_12;   
  char caen785_13; 
  char caen785_14;
  char caen785_15;
  char caen785_16;
  char caen785_17;
  char caen785_18;
  char caen785_19;
  char caen785_20;
  char caen785_21;
  char caen785_22;
  char caen785_23;
  char caen785_24;   /* Last CAEN V785 ADC                           */
  char caen775_1;    /* First CAEN V775 TDC                          */
  char caen775_2;
  char caen775_3;
  char caen775_4;
  char caen775_5;
  char caen775_6;
  char caen775_7;
  char caen775_8;
  char caen775_9;
  char caen775_10;
  char caen775_11;
  char caen775_12;   /* Last CAEN V775 TDC                           */
  char caen792_1;    /* First CAEN V792 QDC                          */
  char caen792_2;    
  char caen792_3;    
  char caen792_4;    
  char caen792_5;    
  char caen792_6;    
  char caen792_7;    
  char caen792_8;    
  char caen792_9;    
  char caen792_10;    
  char caen792_11;    
  char caen792_12;   /* Last  CAEN V792 QDC                          */
  char sis3820_1;    /* First SIS3820 Scaler                         */
  char sis3820_2;    /* Last  SIS3820 Scaler                         */
  char myriad_1;     /* MyRIAD trigger module                        */
      };

/*********************
#define  DEVTBL ((struct devices *)(SYSRAM + offsetof(struct SYRAM,_devtbl[0])))
*********************/

#endif             /* end DEVICES_H_      */
