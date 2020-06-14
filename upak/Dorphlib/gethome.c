#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void gethome_(char*string,int*retlen,int len)
{
     char *cptr;
     memset(string,0X20,len);
     cptr = getenv("HOME");

     if(cptr == NULL)
     {
      *retlen = 0;
      return;
     }

      *retlen = strlen(cptr);
 
     if(*retlen > len)
     {
      *retlen = 0;
     }

else
 
    {
     strncpy(string,cptr,*retlen);
    }
 
return;
 
}
