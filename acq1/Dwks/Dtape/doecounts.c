/* doecntlog.c 

   RLV.  10/20/03

   A routine to log event counts to a central log file for management
   recordkeeping.  The messages will be labelled "doecounts", the priority
   will be LOG_INFO, the facility will be LOG_LOCAL0.

   RLV 01 October 2008
   Add elapsed time to the record.
*/

#include <stdlib.h>
#include <syslog.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

char vmestr[6]="vmexx\0";        /* This string holds the VME being used */
char exptstr[8]="RIBxxx\0\0";

void strnblnk(char *, char *);

/* This sends the message, with a date */
/* The message is likely to be a Milner messlog, 112 bytes */
void doecntlog_(char *message,int *beam,
                double *sumevt, int *recs, int *elapsed)
{
  char logmsg[113];
  char beamstr[4];
  int i;

  for(i=0;i<112;i++)
    logmsg[i]=' ';
  logmsg[113]='\0';

  if (*beam == 1)
    strcpy(beamstr," ON");
  else if (*beam == 0)
    strcpy(beamstr,"OFF");
  else
    strcpy(beamstr,"ERR");

  sprintf(logmsg, 
       "%s  %s %s Evt %12.0f, Rec %i, DT %i\n",
	  exptstr,
	  vmestr, 
	  beamstr,
	  *sumevt,
	  *recs,
          *elapsed);

  syslog(LOG_INFO, "%s\0", logmsg);/* Send the message */
  strncpy(message,logmsg,strlen(logmsg));
  return;

}

/* This routine opens the log channel and initializes some information */
void initdoelog_()
{
  char *envvme;

  openlog("doecounts", 0, LOG_LOCAL0);

  envvme = getenv("VME");
  if (envvme != (char *)NULL)
    sscanf(envvme,"%5s",vmestr);
/*    strcpy(vmestr, envvme);*/

  envvme = getenv("EXPT");
  if (envvme != (char *)NULL)
    sscanf(envvme,"%6s",exptstr);
/*    strcpy(exptstr, envvme); */
  return;
}


/* This routine closes the log channel. */
void closedoelog_()
{
  closelog();
  return;
}

/* Here we try to find the LAST non-blank character in the string*/
/* Then copy it to the output with a \0 appended.  The assumption*/
/* is that Milner gave me a blanked-out string.                  */
void strnblnk(char *output, char *input)
{
  int i,strend;

  for (i=0; i<112; i++)
    *(output+i) = '\0';  /* Set the input string to nulls */

  for (i=111; i>=0; i--) {
    if (*(input+i) != ' ') {
       strend=i;
       break;
    }
  }
  for (i=0;i<strend;i++) 
    *output++ = *input++;

  return;
}
