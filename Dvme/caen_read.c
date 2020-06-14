#include "vme_sys.h"
#include "vmeprom.h"
#include "caen.h"


void main()
{
    int  i,tmp,ch,val;
    struct CAEN *caen = (struct CAEN *)CAEN785_2;
    unsigned short *Event = (unsigned short *)0x30000;

    for (i=0; i < 2048; i++) *Event++ = 0;
    caen->bit_set2 = 0x04;    /* clear data */
    caen->bit_clear2 = 0x04;  /* remove clear data */

    while (1)
      {
        tmp = caen->buf[0];
        switch (tmp & HDR_MASK)
         {
           case HEADER:
              break;
           case VALID:
              ch = (tmp >> 16) & 0x3f;
              ch = ch | 0x8000;
              val = tmp & 0xfff;
              if (val > 0xf00) continue;
              *Event++ = ch;
              *Event++ = val;
              break;
           case EOB:
              *Event++ = 0x8020;
              *Event++ = (tmp >> 16) & 0xff;
              *Event++ = 0x8021;
              *Event++ = tmp;
              break;
           case NOT_VALID:   
              if (Event != (unsigned short *)0x30000)
                {
Event = (unsigned short *)0x30000;
                  *Event++ = 0xffff;
                  *Event++ = 0xffff;
return;
                }
/*              if (Event < (unsigned short *)0x30040) return; */
              Event = (unsigned short *)0x30000;
              delay_(2);
              break;
           default:
              break;
          }
      }
}

