#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <ctype.h>
#define  ACQ
#include "../Dshm/ipcdefs.h"
#include "acqlib.h"


/*   Global variables       */

unsigned short buffer[16384];
int   param, minval, maxval, count, total_evts, evts;
int   minfound, maxfound,seen;
unsigned int   total, sum, evtfirst, evtnext,bufnum;
int   msgf = 0x20202020;
char  mess[80];

void INTsignal (int);

/****************************************************************************
****************************************************************************/
main (int argc, char *argv[])
{
   int   i, len, size, tmp, ierr, ipass, lu;
   unsigned short *sptr;
   char *cptr;
   static char device[80] = {"vme"};
   static char help1[] = {"Usage:  ipcmon "};
   static char help2[] = {"[device] "};
   static char help3[] = {"parmeter_number [min_value] [max value]"};

   minval = 1;
   maxval = 8191;
   minfound = 65535;
   maxfound = 0;
   param = 0x8001;
   count = 0;
   total_evts = 0;

   if ((cptr = getenv("VME")) != NULL) strcpy(device,cptr);
   i = 1;
   if (argc < 2)
   {
      printf ("%s%s%s\n",help1,help2,help3);
      exit (1);
   }
   else if (argc >= 3)
   {
      if (isalpha(*argv[1]))
       {
         strcpy(device,argv[1]);
         i = 2;
       }
   }
   if (sscanf (argv[i], "%i", &tmp) != 0)
      param = 0x8000 + tmp;
   else
   {
      printf ("Invalid parameter number\n");
      exit (1);
   }
   i++;
   if (argv[i] != NULL && sscanf (argv[i], "%i", &tmp) != 0) minval = tmp;
   i++;
   if (argv[i] != NULL && sscanf (argv[i], "%i", &tmp) != 0) maxval = tmp;
   printf ("Search Param = %x, min value = %d, max value = %d\n", param,
                                                               minval, maxval);
   lu = 0;
   size = 32768;
   open_acq_ipc__ (device, &size, &ierr,5);
   if (ierr != 0)
   {
      acq_error(&ierr,mess,sizeof(mess));
      printf ("%s - %s\n", device,mess);
      exit (1);
   }

   signal (SIGINT, INTsignal);
   ipass = 0;
   sum = 0;
   len = 1;
   while (len)
   {
      read_shm_ ((unsigned char *)buffer, &size, &len,&total,&seen,
                                                        &bufnum,&ierr, &msgf);
      if (ierr != 0) break;
      if (!ipass)
      {
	 evtfirst = total;
	 ipass = 1;
      }
      sptr = buffer + len / 2;
      while (*(--sptr) == 0xffff);
      len = (sptr - buffer + 3) * 2;
      i = len / 4;
      sptr = buffer;
      for (; i > 0; i--)
      {
	 if (*sptr == 0xffff) total_evts++;
	 if (*sptr++ == param)
	 {
	    if (*sptr >= minval && *sptr <= maxval) count++;
	    if (*sptr < minfound) minfound = *sptr;
	    if (*sptr > maxfound) maxfound = *sptr;
	    ++evts;
	 }
	 sptr++;
      }
      sum += seen;
      evtnext = total + seen;
      if (msgf == 0) break;
   }
   printf ("\nIPC events seen = %i\n", sum);
   printf ("FE events       = %i\n", evtnext - evtfirst);
   printf ("Search Param = %x, min value = %d, max value = %d\n",
	                                                param, minval, maxval);
   printf ("min value = %d, max value = %d, total events = %d\n",
                                               minfound, maxfound, total_evts);
   printf ("Events matching ID        = %d\n", evts);
   printf ("Events match ID and range = %d\n", count);
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
