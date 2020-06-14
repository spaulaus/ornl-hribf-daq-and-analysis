/******************************************************************************
*
*  Routine to change a FORTRAN character*(*) variable to upper case.
*
*     subroutine strupper(c)
*  CALL:  c  -  character*(*) variable
*
******************************************************************************/
void strupper_(char *line,int len)
{
  int  i;
  for (i=0; i < len; i++) line[i] = toupper(line[i]);
}
