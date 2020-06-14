/******************************************************************************
*
*     function strlen(c)
*  CALL:     c   -  character*(*) variable
*  RETURN:   len - length of string stored in the variable c
******************************************************************************/
int  strlen_(char *line,int len)
{
  int  i;

  i = len;
  while (i > 0)
    {
      if (line[i-1] != ' ' && line[i-1] != '\n' && line[i-1] != '\0'
                                                && line[i-1] != '\t') break;
      --i;
    }
  return (i);
}
