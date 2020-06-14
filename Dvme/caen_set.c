#include "vme_sys.h"
#include "vmeprom.h"
#include "caen.h"


void main()
{
    int  i;
    struct CAEN *caen = (struct CAEN *)CAEN785_1;

printf("firm = %lx\n",caen->firmware);

    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */
    caen->event_trigger_reg = 0;
    for (i=0; i < 32; i++)
      {
        if (i < 32) caen->thresholds[i] = 1;
        else  caen->thresholds[i] = 0x100;
      }
    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */
    caen->event_counter_reset = 0;

    printf("Status Reg1 = %lx\n",caen->status_reg1);
    printf("Status Reg2 = %lx\n",caen->status_reg2);

    caen = (struct CAEN *)CAEN785_2;

    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */
    caen->event_trigger_reg = 0;
    for (i=0; i < 32; i++)
      {
        if (i < 32) caen->thresholds[i] = 1;
        else  caen->thresholds[i] = 0x100;
      }
    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */
    caen->event_counter_reset = 0;
}

