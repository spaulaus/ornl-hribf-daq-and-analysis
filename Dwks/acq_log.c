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
*    File:         /usr/users/mcsq/Dwks/acq_log.c
*
*    Description:  FORTRAN callable routine to put message into the
*                  data acquisition log file.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 6/96    MCSQ    Original     
*
*    7/24/98    MCSQ    Eliminate default VME processor.
****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
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
void acq_log_(char *,char *,int *,int,int);
static char *getfield(char *,char *,int);
static int log_msg(char *,char *,int,int);
void  acq_log_error(int *,char *,int);
void  acq_log_error_(int *,char *,int);

/*
*      Global variables
*/

static char  pacpidfile[80];
static int   log_msg_id = -1;

/***************************************************************************
*  FORTRAN callable subroutine:
*
*        CALL ACQ_LOG(PROG,MESS,IERR)
*
*  Call:   prog  = Program name, 8 characters max
*          mess  = ASCII message for the log file
*
*  Return  ierr  = 0 means OK and nonzero means error.  A call to
*                  acq_log_error converts the error code to an
*                  ASCII string.
*
***************************************************************************/
void acq_log_(char *code,char *msg,int *ierr,int code_len,int msg_len)
{
   static char server[12],in_line[80],tmpstr[32];
          char *cptr;
          int  i,j,id,status;
          FILE *pidfile;

   *ierr = 0;
   if (log_msg_id == -1)
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
*   Open a file written by pacman which contains the ID of
*   the logger's  message queue.
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
*   Read the file and extract the logger message queue ID
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
           if (sscanf(tmpstr,"%i",&id) == 0) continue;
           if (strstr(in_line,"Log_Message_ID") != NULL)
             {
               log_msg_id = (key_t)id;
               break;
             }
         }
       fclose(pidfile);
       if (log_msg_id == -1)
         {
           *ierr = -3;
           return;
         }
     }
/*
*   Format and send message
*/
   *ierr = log_msg(code,msg,code_len,msg_len);
   return;
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
/****************************************************************************
*
*   Send message to logger queue
*
****************************************************************************/
static int log_msg(char *code,char *msg,int code_len,int msg_len)
{
    int  i,len;
    time_t tod;
    struct orphmsg message;

    message.type = MSG_INFORM;
    for (i=0; i < 136; i++) message.text[i] = ' ';
    if (code_len < MSG_SEND_LEN) len = code_len;
    else  len = MSG_SEND_LEN;
    strncpy(message.text,code,len);
    time(&tod);
    strftime(&message.text[MSG_TIME_COL-1],21,"%d-%b-%y %H:%M:%S  ",
                                                            localtime(&tod));
    len = msg_len;
    i = 0;
    while (len > 0)
      {
        if (msg[i] != ' ' && msg[i] != '\0') break;
        i++;
        len--;
      }
    if (len <= MSG_USER_LEN)
      {
        strcpy(&message.text[MSG_USER_COL-1],msg);
      }
    else
      {
        strncpy(&message.text[MSG_USER_COL-1],msg,MSG_USER_LEN);
        message.text[MSG_USER_COL+MSG_USER_LEN] = '\0';
      }
    len += MSG_SEND_LEN+MSG_TIME_LEN +3 +1;
    if (msgsnd(log_msg_id,&message,len,IPC_NOWAIT) == -1)
      {
        if (errno == EACCES) return (1);
        if (errno == EINVAL) return (2);
        return (3);
      }
    return(0);
}
/******************************************************************************
*
*  Returns an ASCII string for an error code.  This one is for a C caller.
*  It returns a NULL terminated string.
*
*******************************************************************************/
void  acq_log_error(int *error,char *string,int len)
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
void  acq_log_error_(int *error,char *string,int len)
{
  int  i,ierr,j;
  char **cptr;

  static char *tapeou_msg[] = {
    "Log queue access failure",
    "Log message queue failure",
    "Unknown error code"
};

  static char *softerr_msg[] = {
    "pacman pid/id file does not exist",
    "pacman pid/id file format error",
    "Invalid pacman pid/id file",
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
