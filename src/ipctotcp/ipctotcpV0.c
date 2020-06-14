/*
 * ORNL Physics Division; 
 * A server to send orphas ipc buffers to external clients, using TCP/IP.
 * Based on McConnell's ipcmon and routines udptoipc.
 * RLV 14 Nov 2017
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <ctype.h>
#define  ACQ
#include "../include/ipcdefs.h"
#include "../include/acqlib.h"


/*   Global variables       */

unsigned short buffer[16384];/*32768 bytes is the length of buffers */
int   seen, count, total_evts, evts;
unsigned int   total, sum, evtfirst, evtnext,bufnum; /*event counters */
int   msgf = 0x20202020; /* Interrupt handlers set this to non-blank */
char  mess[80];

void INTsignal (int);

/****************************************************************************
****************************************************************************/
int main (int argc, char *argv[])
{
   int   i, len, size, tmp, ierr, ipass, lu;
   unsigned short *sptr;
   char *cptr;
   static char device[80] = {"vme"};
   static char help1[] = {"Usage:  ipctotcp "};
   static char help2[] = {"[device] "};

   count = 0;
   total_evts = 0;

   /* Process any arguments */
   if ((cptr = getenv("VME")) != NULL) 
      strcpy(device,cptr);

   i = 1;
   if (argc > 2) {
      printf ("%s%s\n",help1,help2);
      exit (1);
   }
   else if (argc == 2) {
      if (isalpha(*argv[1]))
         strcpy(device,argv[1]);
   }

   /* Connect to the ipc resources assigned to this VME device */
   lu = 0;
   size = 32768; //size of the buffers
   open_acq_ipc__ (device, &size, &ierr,5); //"5" is the length of device str
   if (ierr != 0) {
      acq_error(&ierr,mess,sizeof(mess));
      printf ("%s - %s\n", device,mess);
      exit (1);
   }

   /* Catch SIGINT to detect that we should stop?*/
   signal (SIGINT, INTsignal);

   /* Setup the TCP listener - looking for connections */

   /* Finished setting up listener */



   /* Read loop over the ipc buffers */
   ipass = 0;
   sum = 0;
   len = 1;
   unsigned int nbuffers=0;
   /* executes while len!=0 */
   while (len) {

      read_shm_ ((unsigned char *)buffer, &size, &len,
		                          &total, &seen,
                                          &bufnum, &ierr, &msgf);
      if (ierr != 0) break;

      printf("Size = %i - \n", len);
      int ic;
      for (ic=0; ic<16; ic+=2) 
	printf("Word %i = %x Word %i = %x \n", ic, *(buffer+ic), 
	       ic+1, *(buffer+ic+1));


      printf("Buffers since starting: %i\n", nbuffers++);
      evtnext = total + seen;
   }

   /* End of the server, print summary */
   printf ("\nIPC events seen = %i\n", sum);
   printf ("FE events       = %i\n", evtnext - evtfirst);

   /* Close the units */
   lu = O_READ;
   close_shm_(&lu);

   return (0);
}
/****************************************************************************
****************************************************************************/
void INTsignal (int sig)
{
   msgf = 0;
}
