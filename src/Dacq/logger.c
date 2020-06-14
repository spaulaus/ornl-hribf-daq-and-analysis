/*==============================================================*/
/* logger.c - daemon to accept messages from the ORPHASMSG
  msg queue and display them on the console, with date time 
  and other stamps.

Revisions:
  11/29/92  MCSQ   Added a list of signals to ignore.  Changed error
                   outputs from this code.  Send errors to STDERR.
                   The log file - ORPHAS.LOG - is put in the users
                   home directory.  Send a couple of Bells before
                   all messages which are not action type 'inform'.

  12/ 2/92  MCSQ   Changed the message format.  Added test for 
                   EINTR error following msgrcv call.  Remove
                   stale messages before starting.

  12/ 5/92  MCSQ   Added prototypes for message queue system call
                   since argument types are not included in sys/msg.h.

  12/ 6/92  MCSQ   Disable CTRL\S and CTRL\Q

  12/ 7/92  MCSQ   Commment out the disable CTRL\S and CTRL\Q.  Now 
                   we reset CTRL\S whenever we want to do output.

   2/ 8/93  MCSQ   Flush the log file stream after each 100 lines
                   added to file so user can examine the log file
                   with an editor.

   6/17/93  MCSQ   Changed name of the log file from ORPHAS.LOG
                   to ORPHAS.log

  11/14/93  MCSQ   New shared memory buffer scheme

   6/ 1/94  MCSQ   When some jerk pastes text cut from a window into
                   the logger window, the xterm term process which
                   owns the logger window goes into a highly excited
                   state.  The logger process input buffer fills and
                   xterm runs continuously until it can move the paste
                   buffer into the logger process input buffer.  This
                   can consume a lot of cpu time.  Now, after every
                   message, the input buffer is flushed until it is
                   completely empty.

   4/ 6/98  MCSQ   Flush output file after 25 messages or greater than
                   30 minutes since last flush.

   8/ 7/02  MCSQ   Adapted for Linux
 ---------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <termios.h>
#include "orphmsg.h"
#define  ACQ
#include "ipcdefs.h"
#include "acqlib.h"

/*  Prototypes    */

void TermHandle(int);

FILE *MsgLogFile;         /* log the messages     */
int   msgid;              /* Message queue ID     */
struct msqid_ds statbuf;
struct acq_keys keys;
int nbytes;

/*====================================================================
   main program 
====================================================================*/
int main(int argc, char *argv[])
{
    struct orphmsg rcvmsg;    /* message structure received */
    int msglen;
    int line_count = 0;
    int ierr;
    time_t last_out;
    char Start = ('q' & 037);  /* CTRL Q character */

    char outstring[256];      /* formatted output string */

    if (argc < 2)
      {
        printf("Need ACQ device name\n");
        exit(99);
      }

/*  Attach to message queue for other processes to send message which
*   we are to put in the log file and output to STDOUT
*/
    get_acq_key(argv[1],(int)strlen(argv[1]),&keys,NULL);
    msgid = attach_msg(keys.log_msg,&ierr);
    if (msgid == -1)
     {
       fputs("\n\7Can't find a message queue for log messages\n",stderr);
       fputs("logger - TERMINATED!\7\n",stderr);
       exit(EXIT_FAILURE);
     }

/*     Open the log file - this is the core of this function
*      Put file orphas.vmeX in users home directory
*/
    strcpy(outstring,getenv("HOME"));
    strcat(outstring,"/");
    strcat(outstring,"orphas.");
    strcat(outstring,argv[1]);
    MsgLogFile = fopen(outstring, "a+");  /* Open the log file */
    if (MsgLogFile == NULL) {
       perror("\n\7logger - fopen");
       fprintf(stderr,"Can't open the log file - %s\n",outstring);
       fputs("logger - TERMINATED!\7\n",stderr);
       exit(EXIT_FAILURE);
    }
/*   Ignore many signals      */
    signal(SIGINT, SIG_IGN);
    signal(SIGQUIT, SIG_IGN);
    signal(SIGTSTP, SIG_IGN);
    signal(SIGCONT, SIG_IGN);
    signal(SIGUSR1, SIG_IGN);
    signal(SIGUSR2, SIG_IGN);

/*
*   Catch the SIGTERM signal so we can wipe before leaving.
*/
    signal(SIGTERM, TermHandle);


/*  Loop, getting messages and writing them to the screen */
/*  and to a log file */

    last_out = time(NULL);
    while (1) {
        do
          {
            msglen=msgrcv(msgid, &rcvmsg, sizeof(struct orphmsg), 0, 0);
          }
        while (msglen == -1 && errno == EINTR);
/*
*  Fake a CTRL Q, just in case someone typed a CTRL S
*/
        ioctl(0,TIOCSTI,&Start);

        if (msglen == -1)
          {
            fputs("\n\7Error receiving message to be logged\n",stderr);
            fputs("logger - TERMINATED!\7\n",stderr);
            break;
          }

        if (rcvmsg.type > MSG_INFORM) fputs("\7\7",stdout);
        fputs(rcvmsg.text,stdout);
        fputs("\n",stdout);
        fputs(rcvmsg.text,MsgLogFile);
        fputs("\n",MsgLogFile);
        if (++line_count >= 25 || ((time(NULL) - last_out) >= 1800))
          {
            fflush(MsgLogFile);
            line_count = 0;
            last_out = time(NULL);
          }
/*
*  Check the terminal input buffer.  If there are any characters there,
*  flush the buffer.  Repeat the check and flush at 1 second intervals
*  until the buffer is completely empty.
*/
        do
          {
            if (ioctl(1,FIONREAD,&nbytes) == -1)
              {
                perror("# chars");
                break;
              }
            if (nbytes)
              {
                tcflush(0,TCIFLUSH);
                sleep(1);
              }
          }
        while(nbytes);
    }
    fclose(MsgLogFile);
    exit(EXIT_FAILURE);
    return(0);
} 

/*=====================================================================
   TermHandle - function to accept SIGTERM signal so we can wipe before
   we leave the pottie.
 =====================================================================*/
void TermHandle (int sig)
{
    fclose(MsgLogFile);
    exit(EXIT_SUCCESS);
}
