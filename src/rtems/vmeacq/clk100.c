#include <rtems.h>
#include <bsp.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern unsigned short clk100[2];

void clkxx(void)
{
  unsigned long *lptr = (unsigned long *)clk100;

  while(1)
   {
     rtems_task_wake_after(1);
     *lptr += 1;
   }
}
