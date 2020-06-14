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
*    File:         /usr/users/mcsq/Dlinux/Dacq/fifo.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/24/02    MCSQ       Adapted from the Fortran code fifo.f
*
*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include "cnaf.h"

/*
*     Local Function Prototypes
*/
char *getfield(char *,char *,int);
void rfifo(void);
void wfifo(void);


int  b,crem,nrem,stat;
int  rdat[256],wdat[256];

/******************************************************************************
******************************************************************************/
int main(int argc, char *argv[])
{
    char cmd[80],tmp[80];
    char *cptr;
    int a,f,mode,tmo,nwds;
    int cnt=0,count=1000;
    time_t tod,todstr=-1;
    
    char imessage[] = {
"\n  FIFO test routine\n\n"};

    printf("%s",imessage);
    b = 2;
    crem,nrem = -1;

    if (argc == 3) {
      crem = strtol(argv[1], NULL, 10);
      nrem = strtol(argv[2], NULL, 10);

      if ((crem < 0 || crem > 17) ||
	  (crem == 8 || crem == 9) ||
	  (nrem < 1 || nrem > 23)) 
	printf("Invalid Crate %i or Slot %i. Try again.\n",crem,nrem);	  

    }

    else {

      while(1)
	{
	  printf(" Crate Number?  ");
	  while (fgets(cmd,sizeof(cmd),stdin) == (char *)NULL)
	    {
	      if (feof(stdin) != 0) return;
	      printf("\ncnaf> ");
	    }
	  /*
	   *  Replace the new line character with a NULL
	   */
	  cmd[strlen(cmd)-1] = '\0';
	  /*
	   *  Get the first field of the command line
	   */
	  cptr = cmd;
	  cptr = getfield(tmp,cptr,sizeof(tmp));
	  sscanf(tmp,"%i",&crem);
	  if (crem < 0 || crem > 17) continue;
	  if (crem == 8 || crem == 9) continue;
	  break;
	}
      while(1)
	{
	  printf(" FIFO Slot?  ");
	  while (fgets(cmd,sizeof(cmd),stdin) == (char *)NULL)
	    {
	      if (feof(stdin) != 0) return;
	      printf("\ncnaf> ");
	    }
	  /*
	   *  Replace the new line character with a NULL
	   */
	  cmd[strlen(cmd)-1] = '\0';
/*
 *  Get the first field of the command line
 */
	  cptr = cmd;
	  cptr = getfield(tmp,cptr,sizeof(tmp));
	  sscanf(tmp,"%i",&nrem);
	  if (nrem < 1 || nrem > 23) continue;
	  break;
	}
    }
/*
 *   Clear FIFO
 */
  mode = 1;
  tmo = 1;
  nwds = 0;
  a = 0;
  f = 9;
  cmcbsc_(&b,&crem,&nrem,&a,&f,&mode,&tmo,rdat,&nwds,&stat);
  if (stat == 0x80)
    {
      printf(" CAMAC error\n");
      exit(99);
    }
  if (stat != 1) printf(" Error Clearing FIFO - Status = %x\n",stat);
  time(&todstr);
  while(1)
    {
       wfifo();
       rfifo();
       cnt++;
       if ((cnt% count) == 0)
        {
          time(&tod);
          printf(" %i cycles in %i sec at %s",cnt,tod-todstr,ctime(&tod));
        }
    }
}
/***************************************************************************
***************************************************************************/

void rfifo(void)
{
  int i;
  int stat,err = 0;
  int tdat,diff;
  int a,f,mode,tmo,nwds;

  mode = 1;
  tmo = 1;
  nwds = (sizeof(rdat))/(sizeof(int));
  a = 0;
  f = 0;
  cmcbsc_(&b,&crem,&nrem,&a,&f,&mode,&tmo,rdat,&nwds,&stat);
  if (stat != 0) {printf(" Read Status Error - %x\n",stat); exit(99);}

  for (i=0; i < 256 ; i++)
    {
      tdat = wdat[i] & 0xffffff;
      if (rdat[i] != tdat)
       {
         if (err == 0 && i > 0)
         printf (" Buffer Addr= %3i Expected Data= %6x Read Data= %6x\
 *************\n",i-1,wdat[i-1],rdat[i-1]);
         err = 1;
         diff = rdat[i] ^ tdat;
         printf (" Buffer Addr= %3i Expected Data= %6x Read Data= %6x\
  XOR= %6x\n",i,tdat,rdat[i],diff);
       }
    }
}

/***************************************************************************
***************************************************************************/
void wfifo(void)
{
  int i,j = 0;
  int stat;
  int dat;
  static int seed = -1;
  int a,f,mode,tmo,nwds;

  if (seed == -1)
   {
     seed = 1001;
     srandom((unsigned int)seed);
   }
  for (i=0; i < 256; i++)
    {
      wdat[i] = random() & 0xffffff;
      j++;
    }
  mode = 1;
  tmo = 1;
  nwds = (sizeof(wdat))/(sizeof(int));
  a = 0;
  f = 16;
  cmcbsc_(&b,&crem,&nrem,&a,&f,&mode,&tmo,wdat,&nwds,&stat);
  if (stat != 0) printf(" Write Status Error - %x\n",stat);
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
