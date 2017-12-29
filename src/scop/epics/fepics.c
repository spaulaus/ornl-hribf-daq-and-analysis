/* fepics contains fortran-callable functions to open a connection to
John Sinclair's EPICS interface for the data acquisition system.  
This implementation depends upon global data "connection" and so is possibly
NOT thread-safe.*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include "dataServer.h"

#define BASEPORT 60000
#define NUMPORTS 6

dsHandle connection;
char *serverfile="/usr/acq/etc/scop.server";
char serverhost[16];

#define NUMBL 5
char *beamlines[6]={"drs","rms","enge","bl21","bl23",""};
int blsize[6]={3,3,4,4,4,0};

/* This function is used only to read a standard file in the
   acquisition system to get the server address, which may 
   change on occasion.
*/
void getServerHost(char *file, char *host)
{
   FILE *srvrfp;  /* server file pointer */
   char *srvrmode="r";

   srvrfp = fopen(file, srvrmode);
   if (srvrfp != 0) {
      host = fgets(host, 16, srvrfp);
   }
   else {
      strncpy(host, "192.168.18.41", 16);
   }
   return;
}

/* 
Call this routine to initialize access to the EPICS scalers
   From Fortran
   CALL FOPEN_EPIC(NVAL, BL, ERRNUM, ERRMSG)
   INTEGER NVAL - number of scalers to be used
   CHARACTER BL - name of the beamline, one of 
                  "bl21","bl23","rms","enge","drs"
   INTEGER ERRNUM - a numerical error code, 0 means no error
   CHARACTER ERRMSG - a string with an error message if errnum=0
*/
void fopen_epics__(int *nval,char *bl, int *errcode, char *errmsg)
{

  int i, ii, stat;
  char buf[256];
  char *tmpchar;
  int totchar;
  char tmpbl[5];
  int npvs;


  stat=0;
  *errcode=0;

  /* Validate the input */
  /* Number of PV's must be less than 9*/
  npvs = *nval - 1;
  if ((npvs<0) || (npvs>9)) {
    *errcode=1;
    sprintf(errmsg,"Number of PV's = %i",*nval);
    return;
  }
  /* Check that the beamline is legal */
  strncpy(tmpbl, bl, 5);
  tmpbl[4]='\0';
  for (i=0; i<NUMBL; i++)
    if (!strncmp(tmpbl, beamlines[i],blsize[i])) 
      break;
  if (i>=NUMBL) {
    *errcode=1;
    sprintf(errmsg,"Bad beamline name = %s",bl);
    return;
  }

  strcpy(tmpbl, beamlines[i]);  /*Get a safe copy*/

  /* Get the serverhost from the standard filename */
  getServerHost(serverfile, serverhost);

  /* Search for an open server.  They listen on six fixed, 
     consecutive ports */
  for (i=0; i<NUMPORTS; i++) {
    stat = dsInit( serverhost, BASEPORT+i, &connection );
    if ( !( stat & 1 ) ) {
      dsTerminate( connection );
      continue;
    }
    else
      break;
  }

  if (i >= NUMPORTS) {
    /* Search for an open server has failed, return last message */
    *errcode = stat;
    strncpy(errmsg, dsGetErrorText( connection, stat ), 127);
    return;
  }

  /* We got here, so we have a server connection */


  /* Build a list of PV's to use */

  tmpchar = buf;
  totchar = 0;
  for (i=0;i<=npvs;i++) {
    ii=sprintf(tmpchar,"scal:%s:cnt%i,",tmpbl, i);
    tmpchar+=ii;
    totchar+=ii;
  }
  buf[totchar]='\0';

  stat = dsAllocPvs( connection,buf );
  if ( !( stat & 1 ) ) {
    *errcode=stat;
    printf("%s\n",buf);
    strncpy(errmsg, dsGetErrorText( connection, stat ), 127 );
    dsTerminate( connection );
    return;
  }

  return;
}  
/* 
Call this routine to close access to the EPICS scalers
   From Fortran
   CALL FCLOSE_EPICS(ERRNUM, ERRMSG)
   INTEGER ERRNUM - a numerical error code, 0 means no error
   CHARACTER ERRMSG - a string with an error message if errnum=0
*/
void fclose_epics__(int *errcode, char *errmsg)
{
  int stat;


  dsDelay( 1.0 );

  stat = dsFreePvs( connection );
  if ( !( stat & 1 ) ) {
    *errcode=stat;
    strncpy(errmsg, dsGetErrorText( connection, stat ), 127 );
    dsTerminate( connection );
    return;
  }

  dsDelay( 1.0 );

  stat = dsTerminate( connection );
  if ( !( stat & 1 ) ) {
    *errcode=stat;
    strncpy(errmsg, dsGetErrorText( connection, stat ), 127 );
    return;
  }
}
/* 
Call this routine to write to the EPICS scalers
   From Fortran
   CALL FWRITE_EPICS(NVAL, VALUES, ERRNUM, ERRMSG)
   INTEGER NVAL - number of scalers to write (0<NVAL<=10)
   INTEGER VALUES - array of up NVAL values to write
   INTEGER ERRNUM - a numerical error code, 0 means no error
   CHARACTER ERRMSG - a string with an error message if errnum=0
*/
void fwrite_epics__(int *nval, float values[10], int *errcode, char *errmsg)
{
  char buf[256];
  char *tmpchar;
  int totchar;
  int npvs;
  int i, ii, stat;

  /* Validate the input */
  /* Number of PV's must be less than 9*/
  npvs = *nval - 1;
  if ((npvs<0) || (npvs>9)) {
    *errcode=1;
    sprintf(errmsg,"Bad number of PV's = %i",*nval);
    return;
  }
  /* Rewrite the values to character buffer */
  tmpchar = buf;
  totchar = 0;
  for (i=0;i<=npvs;i++) {
    ii=sprintf(tmpchar,"%-f,", values[i]);
    tmpchar+=ii;
    totchar+=ii;
  }
  buf[totchar]='\0';

  // Send the one buffer to the dataServer
  stat = dsWriteAll( connection, buf );
  if ( !( stat & 1 ) ) {
    *errcode=stat;
    strncpy(errmsg, dsGetErrorText( connection, stat ), 127 );
    return;
  }

}
