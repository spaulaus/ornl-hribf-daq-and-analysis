/******************************************************************************
*
*  Routine to concatenate two FORTRAN character*(*) variables.
*
*     function strappend(dest,scor)
*  CALL:   dest  -  character*(*) variable
*          sorc  -  character*(*) variable
*
*  RETURN: number of discarded characters
*
*  Appends sorc string to destination string.  If the length of the combined
*  string exceeds the destination storage, the right most characters 
*  of the source string are lost.
******************************************************************************/
#include <string.h>

int strappend_(char *dst, const char *sorc, int lendst, int lensorc)
{
  int  i,j,lost = 0;

  if (lendst < 1 || lensorc < 1) return;
  i = strlen_(dst,lendst);
  j = strlen_(sorc,lensorc);
  if (i+j > lendst)
    {
      lost = i + j - lendst;
      j = lendst - i;
    }
  //  strncpy((char *)(dst+i),(const char *)sorc, (size_t)j);
  strncpy(dst+i, sorc, j);
  return (lost);
}
