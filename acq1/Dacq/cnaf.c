/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2002
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dacq/cnaf.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/15/96    MCSQ        Adapted from Fortran code cnaf.f
*
*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

/*
*     Local Function Prototypes
*/
char *getfield(char *,char *,int);
void help(void);


/******************************************************************************
******************************************************************************/
main()
{
    char cmd[80],tmp[16],tmp2[16];
    char *cptr;
    int  c,n,a,f,d,stat,nwds = 1;
    int  off,qq,xx;
    int  i,ierr,var,args;
    char imessage[] = {
"\n  CNAF - Linux System Version\n\
   Type HELP for a list of commands\n\n"};

    printf("%s",imessage);
    c=n=a=f=d=stat = 0;
    off=qq=xx = 0;
    while(1)
     {
       printf("cnaf> ");
       while (fgets(cmd,sizeof(cmd),stdin) == (char *)NULL)
        {
          if (feof(stdin) != 0) return;
          printf("\ncnaf> ");
        }
/*
 *  Replace the new line character with a NULL. Find the
 *  number of arguments on the command line.
 */
       cmd[strlen(cmd)-1] = '\0';
       cptr = cmd;
       args = 0;
       while (cptr != NULL)
         {
           cptr = getfield(tmp,cptr,sizeof(tmp));
           args++;
         }
       args--;
       if (args < 1) continue;
/*
 *  Get the first field of the command line
 */
       cptr = cmd;
       cptr = getfield(tmp,cptr,sizeof(tmp));
/*
 *  Check for one of our internal commands
 */
       if (!strcmp(tmp,"end")) exit(0);
       if (!strcmp(tmp,"exit")) exit(0);
       if (!strcmp(tmp,"wait"))
         {
           sleep(1);
           continue;
         }
       if (!strcmp(tmp,"help"))
         {
           help();
           continue;
         }
       if (!strcmp(tmp,"l"))
         {
           printf(" C= %i N= %i A= %i F= %i D= %6.6x Q= %i X= %i\n",
                   c,n,a,f,d,qq,xx);
           continue;
         }
       if (!strcmp(tmp,"e") || !strcmp(tmp,"x"))
         {
/*
 *   Execute and list CNAF commands
 */
           int x;

           cptr = getfield(tmp2,cptr,8);
           if (cptr == NULL) x = 1;
           else if (sscanf(tmp2,"%i",&x) != 1) x = 1;
           for (i=0; i < x; i++)
             {
               int istat = 0, one = 1;

               camacio_(&one,&c,&n,&a,&f,&d,&nwds,&istat);
               off=qq=xx= 0;
               if (istat == 0xa000) off = 1;
               else
                 {
                   if ((istat & 1) == 0) qq = 1;
                   if ((istat & 2) == 0) xx = 1;
                 }
             }
           if (!strcmp(tmp,"e"))
             {
               printf(" C= %i N= %i A= %i F= %i D= %6.6x Q= %i X= %i\n",
                       c,n,a,f,d,qq,xx);
             }
           continue;
         }
       cptr = getfield(tmp2,cptr,sizeof(tmp2));
       if (args > 2)
         {
           printf("*** Too Many Arguments\n");
           continue;
         }
       if (sscanf(tmp2,"%x",&var) != 1)
         {
           printf("*** Invalid Argument\n");
           continue;
         }
       if (!strcmp(tmp,"d"))
         {
           d = var & 0xffffff;
           continue;
         }
       if (sscanf(tmp2,"%i",&var) != 1)
         {
           printf("*** Invalid Argument\n");
           continue;
         }
       if (!strcmp(tmp,"c"))
         {
           c = var;
           if (c < 0 || c > 17 || c == 8 || c == 9)
             {
               printf("*** Invalid Crate number\n");
               c = 0;
             }
           continue;
         }
       if (!strcmp(tmp,"n"))
         {
           n = var;
           if (n < 0 || n > 30)
             {
               printf("*** Invalid Slot number\n");
               n = 0;
             }
           continue;
         }
       if (!strcmp(tmp,"a"))
         {
           a = var;
           if (a < 0 || a > 15)
             {
               printf("*** Invalid Subaddress\n");
               a = 0;
             }
           continue;
         }
       if (!strcmp(tmp,"f"))
         {
           f = var;
           if (f < 0 || f > 31)
             {
               printf("*** Invalid Function code\n");
               f = 0;
             }
           if ((f & 0x8) != 0) nwds = 0;
           else  nwds = 1;
           continue;
         }
       printf("*** Command NOT recognized\n");
     }
}
/***************************************************************************
*
*  Extract one field from the source string.  Fields are delimited by
*  space(s), tab(s) and new_line characters.
*
* Call:  s1  -  pointer to destination string
*        s2  -  pointer to source string
*        n   -  number of characters(including the NULL at end of string)
*               to store in destination.  If the field in the source
*               is greater than n-1, the left most n-1 characters are
*               returned.
*
* Return:  Pointer to next character in source sting following this
*          field.
***************************************************************************/
char *getfield(char *s1,char *s2,int n)
{
   char *cptr;

   *s1 = '\0';
   for (;*s2 == ' ' || *s2 == '\t'; s2++);
   for (cptr = s2; *cptr != ' ' && *cptr != '\0' && *cptr != '\n'; cptr++);
   if (cptr - s2 != 0)
     {
       if (cptr - s2 < n)
         n = cptr - s2;
       else
         n = n -1;
         strncpy(s1,s2,(size_t)n);
         *(s1 + n) = '\0';
         return(cptr);
      }
    else
      return ((char *)NULL);
}
/***************************************************************************
***************************************************************************/
void help(void)
{
   int   i,j,rows,cols,ierr;
   FILE *infile;
   char line[128];
   static char file[] = "/usr/acq/wks/cnaf.hlp";

   winsize_(&cols,&rows,&ierr);
   infile = fopen(file,"r");
   if (infile == NULL) {printf("Can't open file %s\n",file); return;}
   for (i=0; i < 10; i++)
     {
       for (j=0; j < (rows -1); j++)
         {
           if (fgets(line,sizeof(line),infile) != NULL) printf("%s",line);
           else
             {
               fclose(infile);
               return;
             }
         }
       printf("Type return to continue or q to exit ? ");
       fgets(line,sizeof(line),stdin);
       if (line[0] == 'Q' || line[0] == 'q') {fclose(infile); return;}
     }
   fclose(infile);
   return;
}
