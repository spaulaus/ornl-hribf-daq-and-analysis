/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1995
*
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dwks/acqcleanup.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    9/11/95    MCSQ    Original     
*
****************************************************************************/

/*
*   When you compile for hhirf directory, define HHIRF in this code
*   or use 'makeall hhirf'
#define  HHIRF
*/
#ifdef  HHIRF

#define  LOGGER    "/usr/acq/wks/logger"
#define  XTAPE     "/usr/acq/wks/tape"
#define  FEMSG     "/usr/acq/wks/femsg"
#define  PFTOIPC   "/usr/acq/wks/pftoipc"

#else

/*
*   WHO must be defined as the top level directory of the acquisition
*   software.
*/

#ifdef __mips
#define WHO        "/usr/users/mcsq"
#else
#define WHO        "/tera/mcsq"
#endif

#define  LOGGER    WHO"/Dwks/logger"
#define  XTAPE     WHO"/Dwks/tape/tape"
#define  FEMSG     WHO"/Dwks/femsg"
#define  PFTOIPC   WHO"/Dwks/pftoipc"

#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <sys/ioctl.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef __ultrix
#include <wait.h>
#endif

#include "orphmsg.h"
#define  ACQ
#include "../Dshm/ipcdefs.h"
#include "acqlib.h"
#include "acqshm.h"

#ifdef __ultrix

/*  For ULTRIX eyes only                                         */
/*  Included here since sys/msg.h does not define argument types */
extern int msgget(key_t,int);
extern int msgctl(int,int,struct msqid_ds *);
extern int msgrcv(int,void *,int,long,int);
extern int msgsnd(int,void *,size_t,int);
extern char *getcwd(char *,int);
#endif

/*
*    VME processor name
*/
char server[12];

/*
*     Local Function Prototypes
*/
char *getfield(char *,char *,int);
char *keysearch(char *,char *);
void loadchk(void);

/*
*      Global variables
*/
char in_line[257];

/***************************************************************************
*
***************************************************************************/
int main(int argc,char *argv[])
{
   int status;
   char  *cptr;

/*
*   Get name of VME processor
*/
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);
   else  strcpy(server,"vme");
/*
*    Find an kill processes associated with acquisition for this VME system.
*/
   loadchk();

/*
*    Kill IPC resources for this VME system.
*/
   remove_acq_ipc_(server,&status,sizeof(server));
   return (0);
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
*
***************************************************************************/
char *keysearch(char *line,char *key)
{
   char *cptr,prechar,postchar;

   if ((cptr = strstr(line,key)) == NULL) return NULL;
   postchar = *(cptr + strlen(key));
   if (postchar != ' ' && postchar != '\t' && postchar != '\0'
                                           && postchar != '\n') return NULL;
   if (cptr == line) return cptr;
   prechar = *(cptr-1);
   if (prechar != ' ' && prechar != '\t') return NULL;
   return cptr;
}
/***************************************************************************
*   Check what processes are loaded.
***************************************************************************/
void loadchk(void)
{
   int  killit,status = 0,pid,fd;
   FILE *infile;
   char *cptr;
   char tmpfilename[L_tmpnam],pidstring[10],tmpstr[16];

   static char *ps[] = {"/bin/ps",
                             "auxw",
                              NULL };
/*
*   Run "ps auxw" to get processes running
*/
   strcpy(tmpfilename,P_tmpdir);
   strcat(tmpfilename,"/CLNXXXXXX");
   fd = mkstemp(tmpfilename);
   if ((pid = fork()) == 0)
    {
      freopen(tmpfilename,"w",stdout);
      execvp(ps[0],ps);
      perror(ps[0]);
      _exit(0);
    }
   else if (pid <= 0) {printf("acqcleanup loadchk fork failure \n"); exit(99);}
   while(wait(&status) != pid);
   close(fd);
/*
*   Open the input file
*/
   if ((infile = fopen(tmpfilename,"r")) == (FILE *)NULL)
     {
       fprintf(stdout,"acqcleanup loadchk - Cannot open input file\n");
       exit(2);
     }
/*
*   Now check for existence of processes already using the IPC resources
*   we would like to use.  Kill any we find.
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      cptr = getfield(pidstring,in_line,sizeof(pidstring));
      cptr = getfield(pidstring,cptr,sizeof(pidstring));
      if (sscanf(pidstring,"%i",&pid) == 0) continue;
      strcpy(tmpstr,"PID = ");
      strcat(tmpstr,pidstring);
      killit = 0;
      if (strstr(in_line,PFTOIPC)  != NULL)
        {
          if (keysearch(in_line,server) != NULL) killit = 1;
        }
      else if (strstr(in_line,FEMSG)  != NULL)
        {
          if (keysearch(in_line,server) != NULL) killit = 1;
        }
      else if (strstr(in_line,XTAPE)  != NULL)
        {
          if (keysearch(in_line,server) != NULL) killit = 1;
        }
      else if (strstr(in_line,LOGGER)  != NULL)
        {
          if (keysearch(in_line,server) != NULL) killit = 1;
        }
      if (killit)
        {
          if (kill(pid,SIGKILL) == -1)
            {
              perror(tmpstr);
              printf("%s",in_line);
             }
           else  printf("Kill Process %i:\n*** %s",pid,in_line);
         }
    }
  fclose(infile);
  unlink(tmpfilename);
}
