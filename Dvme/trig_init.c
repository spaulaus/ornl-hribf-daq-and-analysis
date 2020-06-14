#include "vme_sys.h"
#include "vmeprom.h"
#include "system.h"
#include "devices.h"
#include "trigger.h"

struct devices *devtbl = DEVTBL;

void main()
{
   struct trig *trigger = (struct trig *)TRIGGER;

   if (!devtbl->trigger) return;

   super_mode_();
   trigger->imra = 0;
   trigger->imrb = 0;
   trigger->iera = 0;
   trigger->ierb = 0;
   trigger->gpip = 0;
   trigger->ddr = ORNL_CLEAR | ORNL_BUSY | ORNL_STOPPED;
   trigger->gpip = ORNL_STOPPED;

   trigger->event_clear = 0;
   user_mode_();

}
