/*======================================================================
  udptoipc.c
  Receives data packets from the Ethernet and rebuffers them to the IPC
  system used by LEMO, SCAN and SCANU.
11 August 1992  R.L. Varner

Revisions:
  11/28/92  MCSQ  Changed many error messages from this code.  Send errors
                  to STDERR.  Added list of signals to ignore.  Now exit
                  on signal SIGTERM.  Changed signal handler to exit
                  immediately just in case no data is being received
                  from the Front-end.  Added a SIGALRM handler which
                  now does the rate calculations and logs them.  This
                  also makes possible messages warning the user that
                  NO data is being received from the Front-end.  Changed
                  'help' to check the correct argument.  Included herein
                  function FindEventEnd which was a seperate file.  Also,
                  functions in file 'timing.c' are no longer used.

   3/25/93  MCSQ  Several changes of the last 10 days include:
                  a) Eliminated one buffer copy by calling system
                     function - read - instead of en_read. See
                     function data_recv.

                  b) Added function split_pkt which determines
                     how a packet can be split between two shared
                     memory buffers.  For 8Kbyte buffers and "full"
                     ethernet packets, 12.1% of the buffer space was
                     wasted. Note that there is still an integral number
                     events per buffer.

                  c) Detect the "stop acquisition" packet and force
                     a swap of shared memory buffers.  This is useful
                     for very low rates.

                  d) Replaced function FindEventEnd with one which works.

                  e) Fixed HandleContinue function to tolerate missed
                     packets.  Also fixed so that events which are 
                     exactly 2 or more packets will not be rejected!

  11/14/93  MCSQ  New shared memory buffer scheme.

   2/26/95  HQJ   Added a TIMEOUT loop in data_recv().

   3/ 5/97  MCSQ  Allow VME processor names longer than 4 characters.

   3/ 5/98  MCSQ  Change rate of 'No front-end data received' messages.

   3/23/03  MCSQ  Ported to Linux

   7/14/04  MCSQ  Fixed some exit messages to report process name

   10/24/05 RLV adapted to UDP/IP data receiving - 
========================================================================*/
/*    Include files  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>              /* needed for time keeping functions */
#include <time.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <signal.h>
#include <errno.h>
#include "orpas_data.h"
#include "spkt_io_udp.h"
#include "orphmsg.h"
#define ACQ
#include "ipcdefs.h"
#include "acqlib.h"
#include "acqshm.h"

/*  Structure used to set and get times  */
struct itimerval getval, setval;         /* used to set, get realtime         */

/*   Prototypes  */
void TermHandle(int);
void sigtimer(int);
int IPCSetup(char *);
char *HandleContinue(int,struct orpas_data *,int,int *);
int FindEventEnd(unsigned short *, int);
int split_pkt(unsigned char *,int *,int ,int );
struct UDP_Packet * pktprocess(struct UDP_Packet *,int);
void  buffers_init(void);

/*   Input data Global Variables */
struct UDP_Packet *pkt_buf,pkt_data;

/*   IPC Global variables  */

static char server[6]="";

static int packets;                /* Number packets received this interval */
static unsigned int FECount;       /* Event count from front-end            */
static unsigned int BaseEvents;
static unsigned int TotalBytes;    /* Number bytes received this interval   */
static unsigned int EventCount;    /* Number events received this interval  */
static int TimerPreset;            /* Interval timer preset seconds         */

static struct orphmsg msg_pfipc={MSG_INFORM,"udptoipc   "};

struct orpas_data *orbuf;
       char mess[78];

/* Communicate with getopt routine  */
extern char *optarg;
extern int optind, opterr, optopt;

/*===================================================================
   Main routine of udptoipc - copies data from packetfilter to 
   an IPC channel
====================================================================*/
int main(int argc, char *argv[]) 
{
   int status;                     /* function status return */
   int retlen;                     /* Length of return       */
   static int ipass=1;

/*-------------------------------------------------------------------*/

/*    Print help if requested  */
   if (argc > 1 && !strcmp(argv[1], "-?")) {
      printf("Usage: udptoipc [-d nodename]\n");
      return(1);
   }

/*
*    Signal initialization.  First the ones to ignore.
*/
   signal(SIGINT, SIG_IGN);
   signal(SIGTSTP, SIG_IGN);
   signal(SIGCONT, SIG_IGN);
   signal(SIGUSR1, SIG_IGN);
   signal(SIGUSR2, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);
/*
*    Use the Interval timer for our rate calculations
*/
signal(SIGALRM, sigtimer);
/*
*    Finally, and most importantly, catch the Terminate signal so we can wipe
*    before getting off the white bowl!.
*/
signal(SIGTERM, TermHandle);

/*
*   Initialize interval timer
*/
   setval.it_value.tv_sec=15;
   setval.it_value.tv_usec=0;
   TimerPreset = 15;
   status=setitimer(ITIMER_REAL, &setval, &getval);
   if (status != 0)
     {
       perror("\n\7StartTiming");
       fputs(" udptoipc - Data rates will NOT be logged.\7\n",stderr);
     }

/*------- Setup the udp socket interfaces ----------*/

   /*
    *     Look for "-d" on the command line
    */
   char c;
   const char *optstring="d:";

   server[0] = '\0';
   while (1) {
     c = getopt(argc, argv, optstring);
     if (c == -1)
       break;
     
     switch (c) {
     case 0:        /* A perfectly good option. */
       break;
     case 'd':
       strncpy(server, optarg, sizeof(server));
       break;
     default:
       printf ("Error - getopt returned character code 0%o ??\n", c);
     }
   }
   
   printf("The server entered is: %s\n",server);

   /* Open the socket for the server */

   status = spkt_open(server, DATA); 
   if (status == -1) {
      fputs("\n\7udptoipc - Failed to open socket connection\n",stderr);
      fputs("udptoipc - TERMINATED\7\n",stderr);
      return(1);
   }

/*------- Setup the IPC interface ---------------------*/
/*   Get the IPC file name to use.                     */

   status = IPCSetup(server);
   if (status == -1) {
      fputs("\n\7udptoipc - Failed to open IPC Interface\n",stderr);
      fputs("udptoipc - TERMINATED\7\n",stderr);
      return(1);
   }
   buffers_init();
 
/*-------  Wait for and copy data packets        --------------*/

   while(1)  {         /* Loop forever */
/*
*  Get a packet from the front-end
*/
       status=spkt_recv(pkt_buf);
       if (status == 1)   /* interrupted call, just keep going */
	 continue;   
       else if (status==-1) {
           fputs("\n\7udptoipc - Error reading from Front-end\n",stderr);
           break;
       }
//       else 
//	 continue;
	
       packets++;
       retlen = pkt_buf->DataSize;       /* Size of data only */
       TotalBytes=TotalBytes+retlen;   

       orbuf = (struct orpas_data *)pkt_buf->Data;
       if (ipass) {BaseEvents = orbuf->TotalEvents; --ipass;}
       EventCount = EventCount + orbuf->NumberEvents;
       FECount = orbuf->TotalEvents + orbuf->NumberEvents;
/*
*    Process the buffer of data into IPC
*/
//       fprintf(stderr,"udptoipc - got buffer #%x size=%i\n",packets, retlen);
       pkt_buf = pktprocess(pkt_buf,retlen);
       if (pkt_buf == NULL) break;

   }    /* End of while(1) */
/*
*   If we ever get here, try to wipe before we leave.
*/
   TermHandle(0);
   return 1;
}
/****************************************************************************   
*
*    Interval timer routine.
*
*  If logging is possible, compute rates and send to message queue for
*  logging.  Otherwise, set to ignore interval timer signals from now
*  on.
*
***************************************************************************/
void sigtimer(int sig)
{
   int status, Warn_msg;
   time_t tod;

/*  Variables needed for timing and rate routines */
   float elapsed, pktRate, byteRate, evntRate, lossRate, percentLoss;
   static int TotalEvents, LostEvents, Warn_count = -1;

   elapsed = TimerPreset;
   TotalEvents = FECount - BaseEvents;
   pktRate =  packets/ elapsed;
   byteRate = TotalBytes/elapsed;
   evntRate = EventCount/elapsed;
   LostEvents = TotalEvents-EventCount;
   lossRate = LostEvents/elapsed;
   if (TotalEvents) percentLoss = ((float)LostEvents*100)/TotalEvents;
   else  percentLoss = 0;

   time(&tod);
   strftime(&msg_pfipc.text[9],21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));

   if (packets > 0) {
     /*
      *   If some packets have been received, report rates
      */
     sprintf(&msg_pfipc.text[29],
	     "Total Events: %d, Seen Events: %d",TotalEvents,EventCount);
     if (msgsnd(Ids.log_msg, &msg_pfipc, sizeof(struct orphmsg), IPC_NOWAIT) != 0)
       {
	 fputs("\n\7udptoipc - Error writing Message Queue\n",stderr);
	 TermHandle(0);
       }
     sprintf(&msg_pfipc.text[29],
	     "Time: %5.0f sec, %5.1f pkts/sec, %5.1f bytes/sec, %5.1f evts/s",
	     elapsed, pktRate, byteRate, evntRate);
     
     if (msgsnd(Ids.log_msg, &msg_pfipc, sizeof(struct orphmsg), IPC_NOWAIT) != 0)
       {
	 fputs("\n\7udptoipc - Error writing Message Queue\n",stderr);
	 TermHandle(0);
       }
     sprintf(&msg_pfipc.text[29],
	     "Lost Events: %d - %5.1f lost/sec - %2.2f Percent Loss",
	     LostEvents,lossRate,percentLoss);
     if (msgsnd(Ids.log_msg, &msg_pfipc, sizeof(struct orphmsg),
		IPC_NOWAIT) != 0)
       {
	 fputs("\n\7udptoipc - Error writing Message Queue\n",stderr);
	 TermHandle(0);
       }
     Warn_count = 0;
   }
   else {
     /*
      *   Not receiving data from front-end. WARN user.
      */
     Warn_msg = 0;
     if (Warn_count < 8) Warn_msg = TimerPreset;
     else if ((Warn_count < 24) &&  ((Warn_count %4) == 3))
       Warn_msg = TimerPreset * 4;
     else if ((Warn_count < 144) &&  ((Warn_count %24) == 23))
       Warn_msg = TimerPreset * 24;
     else if ((Warn_count %144) == 143) Warn_msg = TimerPreset * 144;
     if (Warn_msg)
       {
	 msg_pfipc.type = MSG_WARN;
	 sprintf(&msg_pfipc.text[29],
		 "No front-end data received in %i seconds",Warn_msg);
	 if (msgsnd(Ids.log_msg, &msg_pfipc, sizeof(struct orphmsg),
		    IPC_NOWAIT) != 0)
	   {
	     fputs("\n\7udptoipc - Error writing Message Queue\n",stderr);
	     TermHandle(0);
	   }
       }
     Warn_count++;
   }
   
   msg_pfipc.type = MSG_INFORM;
   packets=0; TotalBytes=0; 
   BaseEvents=FECount;
   EventCount=0;
/*
*   Reset the interval timer and rearm timer signal
*/
   setval.it_value.tv_sec=Shm->log_interval;
   setval.it_value.tv_usec=0;
   TimerPreset = Shm->log_interval;
   status=setitimer(ITIMER_REAL, &setval, &getval);
   if (status != 0) {
     perror("\n\7Reseting timer");
     fputs(" udptoipc - Data rates will NOT be logged.\7\n",stderr);
     signal(SIGALRM,SIG_IGN);
     return;
   }
   signal(SIGALRM,sigtimer);
}
/*=====================================================================
   TermHandle - function to accept SIGTERM signal and gracefully
   terminate the udptoipc program
 =====================================================================*/

void TermHandle (int sig)
{
   time_t tod;

   time(&tod);
   strftime(&msg_pfipc.text[9],21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   strcpy(&msg_pfipc.text[29],"udptoipc EXITING!");
   msgsnd(Ids.log_msg, &msg_pfipc, sizeof(struct orphmsg),IPC_NOWAIT);
   spkt_close();
   close_shm(O_WRITE);
   exit(EXIT_FAILURE);
}


/*================================================================
    IPCSetup - setup the IPC connection 
==================================================================*/

int IPCSetup(char *ipcnam)
{
  int  ierr,size;

  open_acq_ipc_(ipcnam,&size,&ierr,strlen(ipcnam));
  if (ierr != 0)
    {
      acq_error(&ierr,mess,sizeof(mess));
      printf("\n\7udptoipc - %s\n",mess);
      return(-1);
    }
  open_shm(O_WRITE,&ierr);
  if (ierr != 0)
    {
      acq_error(&ierr,mess,sizeof(mess));
      printf("\n\7udptoipc - %s\n",mess);
      return(-1);
    }
  return 0;
}
/*****************************************************************************
*
*
*****************************************************************************/
int split_pkt(unsigned char *buffer,int *events,int buflen,int avail)
{
   int *bufstr = (int *)buffer;
   int *buf,*bufend,evt;

   buflen = buflen - buflen % 4;
   avail = avail - avail % 4;

   if (avail < buflen >> 2)
     {
       int *tmp = bufstr;

       buf = bufstr;
       bufend = (int *)(buffer + buflen);
       evt = 0;
       while (buf < bufend)
         {
           if (*(buf++) == 0xffffffff)
             {
               ++evt;
               if ((unsigned char *)buf - buffer <= avail) tmp = buf;
               else  { --evt; break;}
             }
         }
       *events = evt;
       return ((unsigned char *)tmp - buffer);
     }
   else
     {
       evt = *events;
       buf = (int *)(buffer + buflen - 4);
       avail = avail - 4;
       while (buf >= bufstr)
         {
           if (*buf == 0xffffffff)
             {
               if ((unsigned char *)buf - buffer <= avail) break;
               else  --evt;
             }
           --buf;
         }
       *events = evt;
       return ((unsigned char *)buf - buffer + 4);
     }
}
/*****************************************************************************
*
*   Find number of data bytes in an Ethernet minium size packet.
*
*   
*****************************************************************************/
int FindEventEnd(unsigned short *buffer, int bufferlen)
{
   int *bufstr = (int *)buffer;
   int *buf = (int *)((char *)buffer + bufferlen -4);

   while (buf >= bufstr)
     {
       if (*buf == 0xffffffff) break;
       --buf;
     }
   ++buf;
   return((char *)buf - (char *)buffer);
}
