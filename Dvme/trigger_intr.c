#include "vme_sys.h"
#include "trigger.h"


extern void process_event(void);
extern void dump_event(void);
extern void wait_msg_buf(void);

void acq_int_(void)
{
#pragma ghs interrupt

   struct trig *trigger = (struct trig *)0xfcfe0000;

   process_event();                         /* call readout routine   */
   trigger->event_clear = 0;
}
void dump_int_(void)
{
#pragma ghs interrupt

   dump_event();
}
void wait_int_(void)
{
#pragma ghs interrupt

   wait_msg_buf();
}
