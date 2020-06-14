#include "vmic6015.h"
#include "acromag.h"
#include "xxlnfvme.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/*
*   Global data
*/
struct manifold  groups[NUM_GROUP];        /* detector groups data           */
struct detector  detectors[NUM_DETECTOR];  /* detector data                  */
struct room_temp room;                     /* Room temperature set points    */

/*****************************************************************************
*
*  Dummy main routine.  All it does is change to supervisor mode and
*  call the real main - mainloop.
*****************************************************************************/
void main()
{
  int size1,size2;

  size1 = sizeof(struct manifold);
  printf(" struct manifold size = %i   %x\n",size1,size1*NUM_GROUP);
  size1 = sizeof(struct detector);
  printf(" struct detector size = %i   %x\n",size1,size1*NUM_DETECTOR);
  size1 = sizeof(struct room_temp);
  printf(" struct room_temp size = %i   %x\n",size1,size1);
  return;
}
