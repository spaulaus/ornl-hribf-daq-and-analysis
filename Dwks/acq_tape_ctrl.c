/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1996
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
*    File:         /usr/users/mcsq/Dwks/acq_tape_ctrl.c
*
*    Description:  FORTRAN callable routines to command tape during
*                  data acquisition.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 2/96    MCSQ    Original     
*
*    7/24/98    MCSQ    Eliminate default VME processor.
****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>

#include "orphmsg.h"

#ifdef __ultrix

/*  For ULTRIX eyes only                                         */
/*  Included here since sys/msg.h does not define argument types */
extern int msgget(key_t,int);
extern int msgctl(int,int,struct msqid_ds *);
extern int msgrcv(int,void *,int,long,int);
extern int msgsnd(int,void *,size_t,int);
#endif

/*
*     Function Prototypes
*/
void acq_tape_ctrl_(char *,int *,int);
static char *getfield(char *,char *,int);
static int  tape_cmd(char *);
void  acq_tape_error(int *,char *,int);
void  acq_tape_error_(int *,char *,int);

/*
*      Global variables
*/
static struct fortinmsg message,prompt;

static char  pacpidfile[80];
static pid_t tapepid = -1;
static int   tape_msg_id = -1;

/***************************************************************************
*  FORTRAN callable subroutine:
*
*        CALL ACQ_TAPE_CTRL(STRING,IERR)
*
*  Call:   string = ASCII command string.  Commands are start, stop, init
*                   and status.
*
*  Return  ierr  = 0 means OK and nonzero means error.  A call to
*                  acq_tape_error converts the error code to an
*                  ASCII string.
*
*                  Error are defined in the file
*                  /usr/users/mcsq/Dwks/acq_tape_ctrl.for.
*
***************************************************************************/
void acq_tape_ctrl_(char *cmd,int *ierr,int len)
{
   static char server[12],in_line[80],tmpstr[32],tcmd[80];
          char *cptr;
          int  i,j,pid,status;
          FILE *pidfile;

   *ierr = 0;
   if (tapepid == -1)
     {
/*
*   Get name of VME processor
*/
       if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);
       else
         {
           strcpy(server,"vme");
         }
/*
*   Open a file written by pacman which contains the PID of
*   TAPEOU process and the ID of TAPEOU's command message queue.
*/
       strcpy(pacpidfile,getenv("HOME"));
       strcat(pacpidfile,"/.pacpid.");
       strcat(pacpidfile,server);
       if ((pidfile = fopen(pacpidfile,"r")) == (FILE *)NULL)
         {
           fprintf(stderr,"Cannot open file - %s\n",pacpidfile);
           *ierr = -1;
           return;
         }
/*
*   Read the file and extract the PID and ID
*/
       while (fgets(in_line,sizeof(in_line),pidfile) != NULL)
         {
           cptr = getfield(tmpstr,in_line,sizeof(tmpstr));
           if (cptr != NULL) cptr = getfield(tmpstr,cptr,sizeof(tmpstr));
           else
             {
               *ierr = -2;
               return;
             }
           if (sscanf(tmpstr,"%i",&pid) == 0) continue;
           if (strstr(in_line,"Tape_PID") != NULL)
             {
               tapepid = (pid_t)pid;
             }
           else if (strstr(in_line,"Tape_Message_ID") != NULL)
             {
               tape_msg_id = (key_t)pid;
             }
         }
       fclose(pidfile);
       if (tapepid == -1 || tape_msg_id == -1)
         {
           *ierr = -3;
           return;
         }
     }
/*
*   Copy user command string to temp buffer.
*/
   i = 0;
   j = len;
   if (j > sizeof(tcmd)) j = sizeof(tcmd) ;
   while(i < j)
     {
       tcmd[i] = cmd[i];
       i++;
     }
/*
*   Delete trailing spaces and Null terminate the command string
*/
   i = j -1;
   while (i >= 0)
     {
       if (tcmd[i] != ' ' && tcmd[i] != '\0') break;
       i--;
     }
   i++;
   tcmd[i] = '\0';

/*
*   TSTOP command is special.  For this one we need to send
*   SIGINT signal to TAPEOU.
*/
   if (!strcmp(tcmd,"tstop") || !strcmp(tcmd,"TSTOP"))
     {
       status = kill(tapepid,SIGINT);
       if (status == -1)
         {
           perror("acq_tape_ctrl - tape CTRL/C failure");
           *ierr = -4;
           return;
         }
     }
   else  *ierr = tape_cmd(tcmd);
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
static char *getfield(char *s1,char *s2,int n)
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
*   Put a command into the TAPEOU message queue.
***************************************************************************/
static int tape_cmd(char *buf)
{
    int len,status;
/*
*   Get prompt message from TAPEOU process
*/
    do
      {
        status = msgrcv(tape_msg_id,&prompt, FTN_SIZE, 1L, IPC_NOWAIT);
      }
    while(status == -1 && errno == EINTR);
    if (status == -1)
      {
        if (errno == ENOMSG) return (1);
        if (errno == EACCES) return (2);
        if (errno == EINVAL) return (3);
        return (4);
      }
/*
*   Just send input line to TAPEOU
*/
    strcpy(message.text,buf);
    len = strlen(message.text);
    if (len > FTN_SIZE) len = FTN_SIZE;
    message.type = 2;
    msgsnd(tape_msg_id,&message,(size_t)len,IPC_NOWAIT);
    return (0);
}
/******************************************************************************
*
*  Returns an ASCII string for an error code.  This one is for a C caller.
*  It returns a NULL terminated string.
*
*******************************************************************************/
void  acq_tape_error(int *error,char *string,int len)
{
   char *s1;

/*
*   First call the FORTRAN callable routine.
*/
   acq_tape_error_(error,string,len);
/*
*  NULL terminate the string returned above.
*/
   s1 = string + len -1;
   while(s1 > string)
    {
      if (*s1 != ' ')
        {
          s1++;
          *s1 = '\0';
          break;
        }
      s1--;
    }
}
/******************************************************************************
*
* Call:   CALL ACQ_TAPE_ERROR(ERROR,STRING)
*
* where:
*           INT*4  ERROR - Error code return by a routine in this package
* return:
*           CHARACTER*(*)  -  ASCII message for this error code.
*******************************************************************************/
void  acq_tape_error_(int *error,char *string,int len)
{
  int  i,ierr,j;
  char **cptr;

  static char *tapeou_msg[] = {
    "TAPEOU process busy",
    "TAPEOU access failure",
    "TAPEOU command message queue failure",
    "Unknown error code"
};

  static char *softerr_msg[] = {
    "pacman pid/id file does not exist",
    "pacman pid/id file format error",
    "Invalid pacman pid/id file",
    "TSTOP command failed",
    "Unknown error code"
};

  for (i=0; i < len; i++) string[i] = ' ';
  if (*error == 0) return;
  ierr = *error;
  if (ierr < 0)
    {
      j = sizeof(softerr_msg)/sizeof(char *);
      ierr = -ierr;
      if (ierr > j) ierr = j;
      cptr = softerr_msg + ierr - 1;
    }
  else
    {
      j = sizeof(tapeou_msg)/sizeof(char *);
      if (ierr > j) ierr = j;
      cptr = tapeou_msg + ierr - 1;
    }
  sprintf(string,"Error = %i: ",*error);
  i = strlen(string);;
  len = len - i;
  if (len)
    {
      i = strlen(*cptr);
      if (i >= len) i = len;
      strncat(string,*cptr,(size_t)i);
    }
}
