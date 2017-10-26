/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2001
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
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /tera/mcsq/Drtems/Dvmeacq/devchk.c
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
*    3/26/93    MCSQ       Added CES8170 test.
*
*    4/12/93    MCSQ       Added ORNL trigger module test.
*
*    2/19/94    MCSQ       Call exit(0) at end of main.  This is needed since
*                          there is a change to supervisor mode to access
*                          the ORNL trigger module.  This means that the
*                          call to dev_test after we changed to supervisor
*                          mode puts the two arguments on the SUPERVISOR
*                          Stack!!   If the compiler trys to keep count
*                          of the number of bytes on the stack, it will
*                          be WRONG by 8 bytes when it adjusts the stack
*                          before the RTS instruction.  Putting the call
*                          exit(0) at the end means the return from this
*                          routine is NEVER taken.
*
*    5/ 1/94    MCSQ       Added VMIC 6015 quad-serial I/O modules for
*                          RMS control system.
*
*    7/21/94    MCSQ       Added LeCroy 1190 Dual Port Memory module
*
*    9/29/94    MCSQ       Added ITG1300 and DATEL628 for RMS control system.
*
*   11/18/94    MCSQ       Changed DATEL 628 to DATEL626
*
*   11/22/94    MCSQ       Added DATEL 613 16-bit ADC
*
*    3/25/95    MCSQ       Added Green Spring IP-Stepper/IP-Digital 24
*
*   10/16/95    MCSQ       Add DATEL 628 12-bit DAC
*
*    6/24/96    MCSQ       Add VMIC 3113A ADC
*
*    2/17/97    MCSQ       Add three LRS1190 interfaces.  Add VMIC 3128
*                          64 channel ADC for DRS.
*
*   12/13/97    MCSQ       Add a second KSC 2917 CAMAC interface
*
*   12/ 1/98    MCSQ       New VME interface to CAMAC
*
*    2/ 8/99    MCSQ       Add  VMIC 3124 ADC for LN fill system
*
*    4/ 4/01    MCSQ       Add 10 CAEN V785 ADCs
*
*    4/11/01    MCSQ       Add 10 CAEN V775 TDCs
*
*    2/06       RLV & MCSQ Changed for use with Rtems.  Uses all old
*                          devices except the ces8170.  
*
*    7/14/06    MCSQ       Add SIS3820 VME scaler module.  Also rework
*                          LRS1190 module detection.
*****************************************************************************/
#include <bsp/VME.h>
#include <libcpu/io.h>

#include "../include/ksc.h"
#include "../include/lrs.h"
#include "../include/ces.h"
#include "../include/acromag.h"
#include "../include/devices.h"
#include "../include/trigger.h"
#include "../include/myriad.h"
#include "../include/vmic6015.h"
#include "../include/lrs1190.h"
#include "../include/datel.h"
#include "../include/stepper.h"
#include "../include/vmic3113a.h"
#include "../include/vmic3128.h"
#include "../include/vmic3124.h"
#include "../include/fcam.h"
#include "../include/caen.h"
#include "../include/sis3820.h"

#include <rtems.h>
#include <stdio.h>
#include <stdlib.h>

#define  MOD  0x0    /* 1821 device code  */

extern struct devices DEVTBL;

/*      Global data              */
static struct devices *devtbl = &DEVTBL;

static struct KSC_VME *ksc1_ptr = (struct KSC_VME *)KSC1;
static struct KSC_VME *ksc2_ptr = (struct KSC_VME *)KSC2;

static struct LRS_VME *lrs_ptr = (struct LRS_VME *)LRS1;

static struct FCAM *fcam = (struct FCAM *)ORNLAUX;

static unsigned short *lrs = (unsigned short *)LRS1;

static struct acromag *avme = (struct acromag *)ACROMAG;
/****  static struct hsm *ces_ptr = (struct hsm *)CES1;  ****/
static struct trig *trigger = (struct trig *)TRIGGER;

static struct MyRIAD_Registers *myriad = (struct MyRIAD_Registers *)MYRIAD_1;

static struct vmic_ch *vmic6015a = (struct vmic_ch *)VMIC6015A;
static struct vmic_ch *vmic6015b = (struct vmic_ch *)VMIC6015B;

static struct LRS1190 *lrs1190a = (struct LRS1190 *)LRS1190A;
static struct LRS1190 *lrs1190b = (struct LRS1190 *)LRS1190B;
static struct LRS1190 *lrs1190c = (struct LRS1190 *)LRS1190C;
static struct LRS1190 *lrs1190d = (struct LRS1190 *)LRS1190D;

static struct datel_dac_regs *datel626a = (struct datel_dac_regs *)DATEL626A;
static struct datel_adc_regs *datel613a = (struct datel_adc_regs *)DATEL613A;
static struct datel_adc_regs *datel613b = (struct datel_adc_regs *)DATEL613B;
static struct stepper_ip *gs = (struct stepper_ip *)STEPPER;
static struct datel_dac_regs *datel628a = (struct datel_dac_regs *)DATEL628A;

static struct vmic_adc *vmic3113a = (struct vmic_adc *)VMIC3113A;
static struct vmic3124_adc *vmic3124 = (struct vmic3124_adc *)VMIC3124;
static struct vmic3128_adc *vmic3128a = (struct vmic3128_adc *)VMIC3128A;

static struct CAEN *caen785_1 = (struct CAEN *)CAEN785_1;
static struct CAEN *caen785_2 = (struct CAEN *)CAEN785_2;
static struct CAEN *caen785_3 = (struct CAEN *)CAEN785_3;
static struct CAEN *caen785_4 = (struct CAEN *)CAEN785_4;
static struct CAEN *caen785_5 = (struct CAEN *)CAEN785_5;
static struct CAEN *caen785_6 = (struct CAEN *)CAEN785_6;
static struct CAEN *caen785_7 = (struct CAEN *)CAEN785_7;
static struct CAEN *caen785_8 = (struct CAEN *)CAEN785_8;
static struct CAEN *caen785_9 = (struct CAEN *)CAEN785_9;
static struct CAEN *caen785_10 = (struct CAEN *)CAEN785_10;
static struct CAEN *caen785_11 = (struct CAEN *)CAEN785_11;
static struct CAEN *caen785_12 = (struct CAEN *)CAEN785_12;

static struct CAEN *caen775_1 = (struct CAEN *)CAEN775_1;
static struct CAEN *caen775_2 = (struct CAEN *)CAEN775_2;
static struct CAEN *caen775_3 = (struct CAEN *)CAEN775_3;
static struct CAEN *caen775_4 = (struct CAEN *)CAEN775_4;
static struct CAEN *caen775_5 = (struct CAEN *)CAEN775_5;
static struct CAEN *caen775_6 = (struct CAEN *)CAEN775_6;
static struct CAEN *caen775_7 = (struct CAEN *)CAEN775_7;
static struct CAEN *caen775_8 = (struct CAEN *)CAEN775_8;
static struct CAEN *caen775_9 = (struct CAEN *)CAEN775_9;
static struct CAEN *caen775_10 = (struct CAEN *)CAEN775_10;
static struct CAEN *caen775_11 = (struct CAEN *)CAEN775_11;
static struct CAEN *caen775_12 = (struct CAEN *)CAEN775_12;

static struct CAEN *caen792_1 = (struct CAEN *)CAEN792_1;
static struct CAEN *caen792_2 = (struct CAEN *)CAEN792_2;
static struct CAEN *caen792_3 = (struct CAEN *)CAEN792_3;
static struct CAEN *caen792_4 = (struct CAEN *)CAEN792_4;
static struct CAEN *caen792_5 = (struct CAEN *)CAEN792_5;
static struct CAEN *caen792_6 = (struct CAEN *)CAEN792_6;
static struct CAEN *caen792_7 = (struct CAEN *)CAEN792_7;
static struct CAEN *caen792_8 = (struct CAEN *)CAEN792_8;
static struct CAEN *caen792_9 = (struct CAEN *)CAEN792_9;
static struct CAEN *caen792_10 = (struct CAEN *)CAEN792_10;
static struct CAEN *caen792_11 = (struct CAEN *)CAEN792_11;
static struct CAEN *caen792_12 = (struct CAEN *)CAEN792_12;

static struct SIS3820 *sis3820_1 = (struct SIS3820 *)SIS3820_1;

/*
*   Function prototype
*/
static int device_test_(int, volatile void *);
static void device_set_(int, volatile void *, int);

/*****************************************************************************
*****************************************************************************/
void devchk(void)
{
  int    tmp;
  unsigned short ushort;

  devtbl->ksc2917a = 0;
  devtbl->ksc3982 = 0;

  devtbl->ornlaux = 0;

  devtbl->lrs1131 = 0;
  devtbl->lrs1821 = 0;
  devtbl->ces8170 = 0;

  devtbl->acromag = 0;

  devtbl->trigger = 0;

  devtbl->myriad = 0;

  devtbl->vmic6015a = 0;
  devtbl->vmic6015b = 0;

  devtbl->lrs1190a = 0;
  devtbl->lrs1190b = 0;
  devtbl->lrs1190c = 0;
  devtbl->lrs1190d = 0;

  devtbl->datel626a = 0;
  devtbl->datel613a = 0;
  devtbl->datel613b = 0;
  devtbl->stepper = 0;
  devtbl->datel628a = 0;

  devtbl->vmic3113a = 0;
  devtbl->vmic3128a = 0;

  devtbl->ksc2917b = 0;
  devtbl->ornlaux = 0;
  devtbl->vmic3124 = 0;

  devtbl->caen785_1 = 0;
  devtbl->caen785_2 = 0;
  devtbl->caen785_3 = 0;
  devtbl->caen785_4 = 0;
  devtbl->caen785_5 = 0;
  devtbl->caen785_6 = 0;
  devtbl->caen785_7 = 0;
  devtbl->caen785_8 = 0;
  devtbl->caen785_9 = 0;
  devtbl->caen785_10 = 0;
  devtbl->caen785_11 = 0;
  devtbl->caen785_12 = 0;

  devtbl->caen775_1 = 0;
  devtbl->caen775_2 = 0;
  devtbl->caen775_3 = 0;
  devtbl->caen775_4 = 0;
  devtbl->caen775_5 = 0;
  devtbl->caen775_6 = 0;
  devtbl->caen775_7 = 0;
  devtbl->caen775_8 = 0;
  devtbl->caen775_9 = 0;
  devtbl->caen775_10 = 0;
  devtbl->caen775_11 = 0;
  devtbl->caen775_12 = 0;

  devtbl->caen792_1 = 0;
  devtbl->caen792_2 = 0;
  devtbl->caen792_3 = 0;
  devtbl->caen792_4 = 0;
  devtbl->caen792_5 = 0;
  devtbl->caen792_6 = 0;
  devtbl->caen792_7 = 0;
  devtbl->caen792_8 = 0;
  devtbl->caen792_9 = 0;
  devtbl->caen792_10 = 0;
  devtbl->caen792_11 = 0;
  devtbl->caen792_12 = 0;

  devtbl->sis3820_1 = 0;

  devtbl->ksc2917a = device_test_(2,&ksc1_ptr->csr);
  devtbl->ksc2917b = device_test_(2,&ksc2_ptr->csr);

/****************************************************************************
  The ORNL CAMAC interface is a special case since it is hard to tell 
  the difference between not present and found.  The trick is to write
  the command register and then read the command register.  The command
  register is normally a write only register.  The write leaves the
  internal bus set to the write data.  The read then reads the previous
  write data.

****************************************************************************/

  fcam->cmd = RS_FIFO | RS_CAM;
  eieio();
  tmp = fcam->cmd;
  if (tmp != 0xffffffff) devtbl->ornlaux = 1;

  /* LRS 1131 is painful.  After the test for the 1821 succeeds, it 
     thinks it should be in DMA mode, which blocks the upcoming I-O
     of the LRS 1190.  The simple solution is to zero the CSR of the
     1131, which disables the DMA mode. 
  */
  device_set_(2, (volatile void *)&lrs_ptr->csr, 0);
  devtbl->lrs1131 = device_test_(2,&lrs_ptr->csr);
  if (devtbl->lrs1131) {
     devtbl->lrs1821 = device_test_(2,lrs+SIB_DIRECT(MOD,7));
    if (!devtbl->lrs1821) {
       lrs_ptr->csr=1;  /* Reset the 1131 after the failure */
       eieio();
     }
     else {
       lrs_ptr->csr=0;  /* Reset to clear the ERR light  */
       eieio();
     }
  }
  /* LRS 1190 needs to be set to a finite value before testing */
  device_set_(2, (volatile void *)lrs1190a->dat, 0);
  device_set_(2, (volatile void *)lrs1190b->dat, 0);
  device_set_(2, (volatile void *)lrs1190c->dat, 0);
  device_set_(2, (volatile void *)lrs1190d->dat, 0);
  eieio();

  devtbl->acromag = device_test_(1,&avme->csr);

ushort = lrs1190a->addr;
printf("lrs1190a = %x\n",ushort);
if (ushort != 0xffff) devtbl->lrs1190a = 1;
eieio();
ushort = lrs1190b->addr;
printf("lrs1190b = %x\n",ushort);
if (ushort != 0xffff) devtbl->lrs1190b = 1;
eieio();
ushort = lrs1190c->addr;
printf("lrs1190c = %x\n",ushort);
if (ushort != 0xffff) devtbl->lrs1190c = 1;
eieio();
ushort = lrs1190d->addr;
printf("lrs1190d = %x\n",ushort);
if (ushort != 0xffff) devtbl->lrs1190d = 1;
eieio();


  devtbl->datel626a = device_test_(1,&datel626a->id[0]);
  devtbl->datel613a = device_test_(2,&datel613a->csr);
  devtbl->datel613b = device_test_(2,&datel613b->csr);
  devtbl->vmic3128a = device_test_(2,&vmic3128a->id);

  tmp = device_test_(1,&gs->m1_ctrl_stat);
  if (tmp) devtbl->stepper = device_test_(1,&gs->stepio[0]);

  devtbl->caen785_1 = device_test_(2,&caen785_1->firmware);
  devtbl->caen785_2 = device_test_(2,&caen785_2->firmware);
  devtbl->caen785_3 = device_test_(2,&caen785_3->firmware);
  devtbl->caen785_4 = device_test_(2,&caen785_4->firmware);
  devtbl->caen785_5 = device_test_(2,&caen785_5->firmware);
  devtbl->caen785_6 = device_test_(2,&caen785_6->firmware);
  devtbl->caen785_7 = device_test_(2,&caen785_7->firmware);
  devtbl->caen785_8 = device_test_(2,&caen785_8->firmware);
  devtbl->caen785_9 = device_test_(2,&caen785_9->firmware);
  devtbl->caen785_10 = device_test_(2,&caen785_10->firmware);
  devtbl->caen785_11 = device_test_(2,&caen785_11->firmware);
  devtbl->caen785_12 = device_test_(2,&caen785_12->firmware);

  devtbl->caen775_1 = device_test_(2,&caen775_1->firmware);
  devtbl->caen775_2 = device_test_(2,&caen775_2->firmware);
  devtbl->caen775_3 = device_test_(2,&caen775_3->firmware);
  devtbl->caen775_4 = device_test_(2,&caen775_4->firmware);
  devtbl->caen775_5 = device_test_(2,&caen775_5->firmware);
  devtbl->caen775_6 = device_test_(2,&caen775_6->firmware);
  devtbl->caen775_7 = device_test_(2,&caen775_7->firmware);
  devtbl->caen775_8 = device_test_(2,&caen775_8->firmware);
  devtbl->caen775_9 = device_test_(2,&caen775_9->firmware);
  devtbl->caen775_10 = device_test_(2,&caen775_10->firmware);
  devtbl->caen775_11 = device_test_(2,&caen775_11->firmware);
  devtbl->caen775_12 = device_test_(2,&caen775_12->firmware);

  devtbl->caen792_1 = device_test_(2,&caen792_1->firmware);
  devtbl->caen792_2 = device_test_(2,&caen792_2->firmware);
  devtbl->caen792_3 = device_test_(2,&caen792_3->firmware);
  devtbl->caen792_4 = device_test_(2,&caen792_4->firmware);
  devtbl->caen792_5 = device_test_(2,&caen792_5->firmware);
  devtbl->caen792_6 = device_test_(2,&caen792_6->firmware);
  devtbl->caen792_7 = device_test_(2,&caen792_7->firmware);
  devtbl->caen792_8 = device_test_(2,&caen792_8->firmware);
  devtbl->caen792_9 = device_test_(2,&caen792_9->firmware);
  devtbl->caen792_10 = device_test_(2,&caen792_10->firmware);
  devtbl->caen792_11 = device_test_(2,&caen792_11->firmware);
  devtbl->caen792_12 = device_test_(2,&caen792_12->firmware);

  devtbl->sis3820_1 = device_test_(4,&sis3820_1->modid);

  devtbl->trigger = device_test_(1,&trigger->iera);
  devtbl->myriad = device_test_(2,&myriad->board_id);
  devtbl->vmic6015a = device_test_(1,&vmic6015a->cmdreg);
  devtbl->vmic6015b = device_test_(1,&vmic6015b->cmdreg);
  devtbl->datel628a = device_test_(1,&datel628a->id[0]);
  devtbl->vmic3113a = device_test_(2,&vmic3113a->id);
  devtbl->vmic3124 = device_test_(1,&vmic3124->id);

     printf("Available Interface Modules are:\n\n");
     if (devtbl->ksc2917a != 0)
                             printf("KSC 2917A -  CAMAC Interface Module\n");
     if (devtbl->ksc2917b != 0)
                         printf("KSC 2917B -  Second CAMAC Interface Module\n");

     if (devtbl->lrs1131 != 0)
       {
         printf("LRS 1131  -  FASTBUS Interface Module\n");
         if (devtbl->lrs1821 != 0)
	   printf("          -  FASTBUS LRS 1821 SMI Module\n");
         else  
	   printf("         - No LRS1821 SMI found\n");
       }
     
     if (devtbl->ces8170 != 0)
       printf("CES 8170  -  FERA Readout Module\n");

     if (devtbl->trigger != 0)
       printf("TRIGGER   -  ORNL Trigger Module\n");

     if (devtbl->myriad != 0)
       printf("MyRIAD  -  ANL/DGS Time/Trigger Module\n");

     if (devtbl->acromag != 0)
       printf("ACROMAG   -  Digital I/O Module\n");

     if (devtbl->vmic6015a != 0)
       printf("VMIC6015A -  Quad-Serial I/O Module\n");
     if (devtbl->vmic6015b != 0)
       printf("VMIC6015B -  Quad-Serial I/O Module\n");

     if (devtbl->lrs1190a != 0)
       printf("LRS1190A  -  LeCroy FERA Readout Module\n");
     if (devtbl->lrs1190b != 0)
       printf("LRS1190B  -  LeCroy FERA Readout Module\n");
     if (devtbl->lrs1190c != 0)
       printf("LRS1190C  -  LeCroy FERA Readout Module\n");
     if (devtbl->lrs1190d != 0)
       printf("LRS1190D  -  LeCroy FERA Readout Module\n");

     if (devtbl->datel626a != 0)
       printf("DATEL626A -  Datel 16-bit DAC Module\n");
     if (devtbl->datel613a != 0)
       printf("DATEL613A -  Datel 16-bit ADC Module\n");
     if (devtbl->datel613b != 0)
       printf("DATEL613B -  Datel 16-bit ADC Module\n");

     if (devtbl->stepper != 0)
       printf("STEPPER   -  Green Spring IP Module\n");

     if (devtbl->datel628a != 0)
       printf("DATEL628A -  Datel 12-bit DAC Module\n");

     if (devtbl->vmic3113a != 0)
       printf("VMIC3113A -  64 Chan, 12-bit ADC Module\n");
     if (devtbl->vmic3124 != 0)
       printf("VMIC3124  -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->vmic3128a != 0)
       printf("VMIC3128A -  64 Chan, 14-bit ADC Module\n");

     if (devtbl->ornlaux != 0)
       printf("ORNLAUX   -  Fast VME interface to CAMAC\n");

     if (devtbl->caen785_1 != 0)
       printf("CAEN785_1 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_2 != 0)
       printf("CAEN785_2 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_3 != 0)
       printf("CAEN785_3 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_4 != 0)
       printf("CAEN785_4 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_5 != 0)
       printf("CAEN785_5 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_6 != 0)
       printf("CAEN785_6 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_7 != 0)
       printf("CAEN785_7 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_8 != 0)
       printf("CAEN785_8 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_9 != 0)
       printf("CAEN785_9 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_10 != 0)
       printf("CAEN785_10 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_11 != 0)
       printf("CAEN785_11 -  32 Chan, 12-bit ADC Module\n");
     if (devtbl->caen785_12 != 0)
       printf("CAEN785_12 -  32 Chan, 12-bit ADC Module\n");

     if (devtbl->caen775_1 != 0)
       printf("CAEN775_1 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_2 != 0)
       printf("CAEN775_2 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_3 != 0)
       printf("CAEN775_3 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_4 != 0)
       printf("CAEN775_4 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_5 != 0)
       printf("CAEN775_5 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_6 != 0)
       printf("CAEN775_6 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_7 != 0)
       printf("CAEN775_7 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_8 != 0)
       printf("CAEN775_8 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_9 != 0)
       printf("CAEN775_9 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_10 != 0)
       printf("CAEN775_10 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_11 != 0)
       printf("CAEN775_11 -  32 Chan, 12-bit TDC Module\n");
     if (devtbl->caen775_12 != 0)
       printf("CAEN775_12 -  32 Chan, 12-bit TDC Module\n");

     if (devtbl->caen792_1 != 0)
       printf("CAEN792_1 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_2 != 0)
       printf("CAEN792_2 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_3 != 0)
       printf("CAEN792_3 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_4 != 0)
       printf("CAEN792_4 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_5 != 0)
       printf("CAEN792_5 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_6 != 0)
       printf("CAEN792_6 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_7 != 0)
       printf("CAEN792_7 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_8 != 0)
       printf("CAEN792_8 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_9 != 0)
       printf("CAEN792_9 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_10 != 0)
       printf("CAEN792_10 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_11 != 0)
       printf("CAEN792_11 -  32 Chan, 12-bit QDC Module\n");
     if (devtbl->caen792_12 != 0)
       printf("CAEN792_12 -  32 Chan, 12-bit QDC Module\n");

     if (devtbl->sis3820_1 != 0)
       printf("SIS3820_1  -  32 Chan, VME Scaler Module\n");
rtems_task_delete(RTEMS_SELF);
getchar ();
exit(0);
}
/*****************************************************************************
*****************************************************************************/
static int device_test_(int size,volatile void *ptr)
{
  int tmp,code;
  
  switch (size)
    {
    case 1:
      tmp =*(unsigned char *)ptr;
      eieio();
      if (tmp != 0xff) code =1;
      else code = 0;
      break;
    case 2:
      tmp = *(unsigned short *)ptr;
      eieio();
      if (tmp != 0xffff) code = 1;
      else code = 0;
      break;
    case 4:
      tmp = *(unsigned long *)ptr;
      eieio();
      if (tmp != 0xffffffff) code = 1;
      else code = 0;
      break;
    default:
      code = 0;
      break;
    }
  return(code);
}
/*****************************************************************************
 *****************************************************************************/
static void device_set_(int size,volatile void *ptr,int value)
{
  switch (size)
    {
    case 1:
      *(unsigned char *)ptr = (char)value;
      eieio();
      break;
    case 2:
      *(unsigned short *)ptr=(short)value;
      eieio();
      break;
    default:
      break;
    }
  return;
}
