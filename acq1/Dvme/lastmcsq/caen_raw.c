#include "vme_sys.h"
#include "vmeprom.h"
#include "caen.h"


unsigned int *Event = (unsigned int *)0x30000;

void main()
{
    int  i,tmp1,tmp2;
    struct CAEN *caen = (struct CAEN *)CAEN785_2;

    Event = (unsigned int *)0x30000;
    for (i=0; i < 2048; i++) *Event++ = 0;
    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */

    while (1)
      {
        tmp1 = caen->buf[0];
        tmp2 = tmp1 & HDR_MASK;
        if (tmp2 == HEADER)
          {
            Event = (unsigned int *)0x30000;
            *Event++ = tmp1;
            while(1)
              {
                tmp1 = caen->buf[0];
                tmp2 = tmp1 & HDR_MASK;
                *Event++ = tmp1;
                if (tmp2 == NOT_VALID) break;
              }
          }
        delay_(2);
      }
}

