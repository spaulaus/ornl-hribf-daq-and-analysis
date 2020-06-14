/******************************************************************************
*
*  Routine to concatenate two FORTRAN character*(*) variables.
*
*     function strappns(dest,scor)
*  CALL:   dest  -  character*(*) variable
*          sorc  -  character*(*) variable
*
*  RETURN: number of discarded characters
*
*  Appends sorc string to destination string (leaving one space between the
*  two strings).  If the length of the combined
*  string exceeds the destination storage, the right most characters 
*  of the source string are lost.
******************************************************************************/
#include <string.h>

int strappns_(char *dst,char *sorc,int lendst,int lensorc)
{
  int  i,j,lost = 0;

  if (lendst < 1 || lensorc < 1) return;
  i = strlen_(dst,lendst) + 1;
  j = strlen_(sorc,lensorc);
  strncpy(dst+i-1," ",1);
  if (i+j > lendst)
    {
      lost = i + j - lendst;
      j = lendst - i;
    }
  strncpy(dst+i,sorc,j);
  return (lost);
}
