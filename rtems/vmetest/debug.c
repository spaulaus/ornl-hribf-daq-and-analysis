#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "debug.h"

struct rlvdebug vmedebug = {1024*1024, 0, 0}; 
 
/* Initialize the debug storage */
void debuginit(void)
{
   vmedebug.data = (unsigned short *)malloc(vmedebug.size*2);
   return;
}

/* Insert value into data */
void debuginsert(short value)
{
   if (vmedebug.index < vmedebug.size) {
      vmedebug.data[vmedebug.index++] = value;
   }
   else {
   }
   return;
}

/* Display some IP statistics */
#include <string.h>
void debugprint(void)
{ 
  int i,j;
  int idxStart=0, idxCount=0;   /* final indices of start and count */
  char inputline[80];       /* line reading the input line */
  char *cStart, *cCount;      /* parsed tokens with start and count*/

  printf("Debugging data\n");
  printf("Enter Start address, Number to print\n");
  if (gets(inputline)!= NULL) { /* this should get a string */
      /* Safely get the first number - error returns*/
      cStart = strtok(inputline, " ");
      if (cStart != NULL) {
         errno = 0;
         idxStart = strtol(cStart, NULL, 10);
         if (errno == EINVAL) return;
      }
      else return;
      /*Safely get the second number - error returns*/
      cCount = strtok(NULL, " ");
      if (cCount != NULL) {
         errno = 0;
         idxCount = strtol(cCount, NULL, 10);
         if (errno == EINVAL) return;
      }
      else return;
  }
  else 
     return;
  /* Print the array over the range requested */
//  for (i=0; i<vmedebug.index; i+=5) {
  if (idxStart+idxCount >vmedebug.index)
      idxCount = vmedebug.index - idxStart;
  for (i=idxStart; i<idxStart+idxCount; i+=12) {
      printf("[%i]",i);
    for (j=0;j<12;j++)
      printf(" %4x", vmedebug.data[i+j]);
    printf("\n");
  }
  return;
}

/* Clear the debug array */
void debugzero(void)
{
  int i;
  printf("Zeroing debug data\n");
  for (i=0; i<vmedebug.size; i++) {
    vmedebug.data[i]=0;
  }
  vmedebug.index=0;
  return;
}

