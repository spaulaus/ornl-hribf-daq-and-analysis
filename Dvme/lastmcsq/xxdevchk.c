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
*    File:         /usr/users/mcsq/Dvme3/devchk.c
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
*****************************************************************************/
#include "ksc.h"
#include "lrs.h"
#include "ces.h"
#include "acromag.h"
#include "devices.h"
#include "vmeprom.h"
#include "trigger.h"
#include "vmic6015.h"
#include "lrs1190.h"
#include "datel.h"
#include "stepper.h"
#include "vmic3113a.h"
#include "vmic3128.h"
#include "vmic3124.h"
#include "fcam.h"
#include "caen.h"

#include <stdio.h>
#include <stdlib.h>

#define  MOD  0xe    /* 1821 device code  */

/*      Global data              */
static struct KSC_VME *ksc1_ptr = (struct KSC_VME *)KSC1;
static struct KSC_VME *ksc2_ptr = (struct KSC_VME *)KSC2;
static struct LRS_VME *lrs_ptr = (struct LRS_VME *)LRS1;
static struct FCAM *fcam = (struct FCAM *)ORNLAUX;
static unsigned short *lrs = (unsigned short *)LRS1;
static struct acromag *avme = (struct acromag *)ACROMAG;
static struct hsm *ces_ptr = (struct hsm *)CES1;
static struct trig *trigger = (struct trig *)TRIGGER;
static struct devices *devtbl = DEVTBL;
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

/*
*   Function prototype
*/
int device_test_(int, volatile void *);

#define  DEVL ((struct devices *)(offsetof(struct SYRAM,_devtbl[0])))
#define  EVTL ((struct devices *)(offsetof(struct SYRAM,_evtlist[0])))
#define  CLK100 ((struct devices *)(offsetof(struct SYRAM,_clk100)))

/*****************************************************************************
*****************************************************************************/
main()
{
  int    level,tmp;

  printf("_devtbl address = %x \n",DEVL);
  printf("_devtbl address = %x \n",DEVTBL);
  printf("_evtlist address = %x\n",EVTL);
  printf("_clk100 address = %x\n",CLK100);
printf(" devtbl address = %x\n",&devtbl->ksc2917a);
  devtbl->ksc2917a = 0;
  devtbl->ksc3982 = 0;
  devtbl->ornlaux = 0;
  devtbl->lrs1131 = 0;
  devtbl->lrs1821 = 0;
  devtbl->ces8170 = 0;
  devtbl->acromag = 0;
  devtbl->trigger = 0;
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

  level = set_intr_level_(0x500);
  devtbl->ksc2917a = device_test_(2,&ksc1_ptr->csr);
  devtbl->ksc2917b = device_test_(2,&ksc2_ptr->csr);
  devtbl->ornlaux = device_test_(2,&fcam->cmd);
  devtbl->lrs1131 = device_test_(2,&lrs_ptr->csr);
  if (devtbl->lrs1131)
                    devtbl->lrs1821 = device_test_(2,lrs+SIB_DIRECT(MOD,7));
  devtbl->acromag = device_test_(1,&avme->csr);
  devtbl->ces8170 = device_test_(2,&ces_ptr->ctrl);
  devtbl->lrs1190a = device_test_(2,lrs1190a->dat);
  devtbl->lrs1190b = device_test_(2,lrs1190b->dat);
  devtbl->lrs1190c = device_test_(2,lrs1190c->dat);
  devtbl->lrs1190d = device_test_(2,lrs1190d->dat);
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
  super_mode_();
  devtbl->trigger = device_test_(1,&trigger->iera);
  devtbl->vmic6015a = device_test_(1,&vmic6015a->cmdreg);
  devtbl->vmic6015b = device_test_(1,&vmic6015b->cmdreg);
  devtbl->datel628a = device_test_(1,&datel628a->id[0]);
  devtbl->vmic3113a = device_test_(2,&vmic3113a->id);
  devtbl->vmic3124 = device_test_(1,&vmic3124->id);
  user_mode_();
  set_intr_level_(level);
  exit(0);
}
