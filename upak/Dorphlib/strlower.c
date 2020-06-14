/******************************************************************************
*
*  Routine to change a FORTRAN character*(*) variable to lower case.
*
*     subroutine strlower(c)
*  CALL:   c  -  character*(*) variable
*
******************************************************************************/
void strlower_(char *line,int len)
{
  int  i;
  for (i=0; i < len; i++) line[i] = tolower(line[i]);
}
