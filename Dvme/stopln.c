#include "devices.h"
#include "vme_sys.h"
#include "syram.h"
#include "vmeprom.h"
#include "lnfvme.h"
#include "vmic3113a.h"
#include "vmic6015.h"

/*      Prototype      */
void mainloop(void);

/*****************************************************************************
*
*  Dummy main routine.  All it does is change to supervisor mode and
*  call the real main - mainloop.
*****************************************************************************/
void main()
{
    super_mode_();
    mainloop();
}
/*****************************************************************************
*****************************************************************************/
void mainloop(void)
{
   struct devices *devtbl = DEVTBL;
   struct vmic_adc *adc = (struct vmic_adc *)VMIC3113A;
   struct vmic_ch *serial;
   struct vmic_bim *bim;
   int  i;

/*
*  If the VMIC 3113A is present, disable interrupts.
*/
   set_intr_level_(0x700);
   if (devtbl->vmic3113a) adc->icr = 0x00;
   if (!devtbl->vmic6015a) return;
   serial = (struct vmic_ch *)VMIC6015A;
   bim = (struct vmic_bim *)(VMIC6015A + BIM);
   for (i=0; i < 4; i++)
     {
       serial->intctl = 0x0;
       serial++;
     }
   bim->cr0 = bim->cr1 = bim->cr2 = bim->cr3 = 0;
   bim->vr0 = bim->vr1 = bim->vr2 = bim->vr3 = 0;
}
