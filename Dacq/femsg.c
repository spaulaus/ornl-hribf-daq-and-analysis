/*======================================================================
   femsg - reads messages from the front end VME host in the ORPHAS data
acquisition system.
Robert Varner

Revisions:
   11/28/92   MCSQ    Added signal handling.  Changed error messages.
                      Changed 'help" test to reference the correct argument.
                      Send all error message(at least those within this
                      code) to STDERR.

   12/ 3/92   MCSQ    Changed format of messages to logger process.

   12/ 5/92   MCSQ    Added message queue system call prototypes.

   11/14/93   MCSQ    New message queue key.

    4/30/94   MCSQ    Send PANIC messages to standard output device

    8/ 3/02   MCSQ    Ported to Linux

    12/28/05  RLV     Adapted to TCP/IP version of Acquisition
========================================================================*/
/*    Include files  */
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include "spkt_io_udp.h"
#include "orphmsg.h"
#define  ACQ
#include "ipcdefs.h"
#include "acqlib.h"

/*   Function Prototypes   */

void TermHandle(int);   /* Signal handler prototype  */
char server[12] = "vme";
struct UDP_Packet rbuf;

main (int argc, char *argv[])
{
  int status;                        /* function status return */
  int  dum,len;
  time_t tod;
  char *vptr;
  struct VMEmsg *femess = (struct VMEmsg *)&rbuf.Data[0];
  struct orphmsg message;
  
  if ((vptr = getenv("VME")) != NULL) strcpy(server,vptr);
  
  /*-------------------------------------------------------------------*/
  
  status = spkt_open(server, FEMSG);
  if (status) {
    perror("Femsg open socket");
    return (-1);
  }
  
  open_acq_ipc_(server,&dum,&status,20);
  if (status != 0) {
    fprintf(stderr,"\n\7femsg - No message Queue for Front-end messages\7\n");
    exit (EXIT_FAILURE);
  }
  /*
   *   Setup to ignore most signals
   */
  signal(SIGINT, SIG_IGN);
  signal(SIGTSTP, SIG_IGN);
  signal(SIGCONT, SIG_IGN);
  signal(SIGUSR1, SIG_IGN);
  signal(SIGUSR2, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  /*
   *   Accept SIGTERM signal to exit
   */
  signal(SIGTERM, TermHandle);
  
  /*-------  Wait for messages  --------------*/
  
  while (1) {

   status = spkt_recv(&rbuf); 
   if (status) {
     perror(" Femsg - read");
     fputs("Communication with Front-end processor failed!\n",stderr);
     fputs("femsg - TERMINATED!\7\n",stderr);
     break;
   }

    if (rbuf.DataSize == 0) continue;
    if (!strncmp(femess->sender,"LN_FILL",7)) continue;
    if (femess->type == MSG_PANIC) {
      printf("%s: %s\n",femess->sender,femess->text);
    }
    message.type = femess->type;
    strcpy(message.text,femess->sender);
    message.text[MSG_SEND_LEN] = ' ';
    time(&tod);
    strftime(&message.text[MSG_TIME_COL-1],21,"%d-%b-%y %H:%M:%S  ",
	     localtime(&tod));
    if ((len = strlen(femess->text)) <= MSG_USER_LEN) {
      strcpy(&message.text[MSG_USER_COL-1],femess->text);
    }
    else {
      strncpy(&message.text[29],femess->text,MSG_USER_LEN);
      message.text[MSG_USER_COL+MSG_USER_LEN] = '\0';
    }
    len += MSG_SEND_LEN+MSG_TIME_LEN +3 +1;
    msgsnd(Ids.log_msg,&message,len,IPC_NOWAIT);
  }
  exit(EXIT_FAILURE);
  return(0);
}
/****************************************************************************
*
*   Catch signal SIGTERM.  Just cleanup and exit.
*
****************************************************************************/
void TermHandle(int sig)
{
   spkt_close();
   exit(EXIT_SUCCESS);
}
