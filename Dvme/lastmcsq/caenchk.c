#include <stdio.h>
#include <stdlib.h>
#include "caen.h"


/*      Global data              */

/*
*   Function prototype
*/
int device_test_(int, volatile void *);
short **save = (short **)0x30000;

/*****************************************************************************
*****************************************************************************/
main()
{
  int    i,level,tmp;
  short *caen = (short *)0xfb000000;

/*************
  level = set_intr_level_(0x500);
*************/
  for (i=0; i < 255; i++)
    {
      tmp = device_test_(4,caen);
      if (tmp != 0)
        {
          *save = caen;
          exit(0);
        }
      printf("address = %lx\n",caen);
      caen = caen + 32768;
    }
/*************
  super_mode_();
  user_mode_();
  set_intr_level_(level);
*************/
  exit(0);
}
