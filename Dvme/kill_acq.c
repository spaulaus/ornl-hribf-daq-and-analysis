/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1994
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
*    File:         /usr/users/mcsq/Dvme3/kill_acq.c
*
*    Description:  Routine to disable event hardware interrupts and patch
*                  interrupt vectors setup by the acquisition code.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/ 1/92    MCSQ         
*
*    5/ 3/94    MCSQ       Previously the default interrupt handler
*                          was taken from vector 255.  This handler address
*                          was used to replace heandlers defined in 
*                          acquisition codes.  However, Force uses vector
*                          255 as a spurious VME interrupt handler.  The
*                          default vector is now define in vme_sys.h and
*                          currently is the illegal instruction vector.
*                          This frees vector 255 for it's intended use.
*****************************************************************************/
#include "vme_sys.h"
#include "devices.h"
#include "pit.h"
#include "trigger.h"
#include "vmeprom.h"

main()
{
   struct trig *trigger = (struct trig *)TRIGGER;
   struct devices *devtbl = DEVTBL;

   if (devtbl->trigger)
     {
       super_mode_();
       trigger->imra = 0;
       trigger->imrb = 0;
       user_mode_();
     }
   *((char *)PIT2_CTL) = 0;
   *((void (**)())(PIT2_VEC*4)) = *((void (**)())(DEF_VEC*4));
   *((void (**)())(VIRQ5_VEC*4)) = *((void (**)())(DEF_VEC*4));
   *((void (**)())(ORNL_EVT_VEC*4)) = *((void (**)())(DEF_VEC*4));
   *((void (**)())(ORNL_TIM_VEC*4)) = *((void (**)())(DEF_VEC*4));
}
