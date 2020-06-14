/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-2003
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dacq/acq_vme_ctrl.c
*
*    Description:  FORTRAN callable routine for control of front-end
*                  VME acquisition system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/31/93    MCSQ        FORTRAN callable routine adapted from
*                           the standalone program acq_run.c
*
*    4/25/93    MCSQ        Changed the returned status.  Now negative
*                           values are always errors and positive values
*                           mean successful operation.  In the case of
*                           a status request, the status is positive and
*                           nonzero.
*
*    9/25/95    MCSQ       New ethernet library in /usr/users/mcsq/Dlan.
*
*   12/ 3/96    MCSQ       Changed routine for_acq_ctrl to acq_vme_ctrl.
*                          Added routine acq_vme_error to convert error
*                          codes to ASCII messages.
*
*    8/14/02    MCSQ       Ported to Linux
*
*    3/18/03    MCSQ       Changes due to pkt_io.c routines
*
*    1/28/04    MCSQ       Add command to zero the 100Hz VME clock
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pkt_io_udp.h"
#include "acq_ctl.h"

static struct UDP_Packet xbuf;
static struct UDP_Packet rbuf;

#define  ETHER_ERROR -127

/*  Function Prototypes    */
int acq_vme_ctrl__(int* ,char *,int );
int acq_vme_ctrl_(int* ,char *,int );
void acq_vme_error__(int *,char *,int);
void acq_vme_error_(int *,char *,int);

/****************************************************************************
*  FORTRAN callable subroutine:
*
*        CALL ACQ_VME_CTRL(ISTAT,STRING)
*
*  Call:   string = ASCII command string.  Commands are start, stop, init
*                   and status.
*
*  Return  istat = positive means successful operation
*                  negative means error.
*
*                  Error and status codes are defined in the file
*                  /usr/users/mcsq/Dacq/acq_vme_ctrl.for.
*
****************************************************************************/
int acq_vme_ctrl__(int* istat,char *for_cmd,int for_len)
{
  return(acq_vme_ctrl_(istat,for_cmd,for_len));
}
int acq_vme_ctrl_(int* istat,char *for_cmd,int for_len)
{

  static char *cmd[] = {"init","start","stop","status","pacfile","host","zero",NULL};
  static char cmd_code[] = {INIT_ACQ,START_ACQ,STOP_ACQ,STATUS_ACQ,PAC_FILE,HOST,
			    ZERO_CLK,0};
  
  //char *cptr,cmdstr[8];
  char cmdstr[8];
  int i;
  int status;             /* return status from system calls */
  
  /*
   *  make command string lower case
   */
  for(i=0; i < 4; i++)
    {
      cmdstr[i] = tolower(*(for_cmd++));
    }
  /*
   *  Check for valid command
   */
  for(i=0; cmd[i] != NULL; i++)
    {
      if(!strncmp(cmdstr,cmd[i],4)) break;
    }
  if (cmd[i] == NULL)
    {
      /*
       *  Unknown command
       */
      *istat = -1;
      return(-1);
    }
  xbuf.Data[0] = cmd_code[i];
  xbuf.DataSize = 100;
  rbuf.DataSize = 100;

  status = pkt_io(&xbuf,&rbuf,FECNTRL,1);
  if (status != 0)
    {
      *istat=ETHER_ERROR;
      return (ETHER_ERROR);
    }
  if (xbuf.Data[0] == STATUS_ACQ)
    {
      *istat=(char)rbuf.Data[1];
      return ((char)rbuf.Data[0]);
    }
  *istat = (char)rbuf.Data[0];
  return ((char)rbuf.Data[0]);
}
/******************************************************************************
*
*  Returns an ASCII string for an error code.  This one is for a C caller.
*  It returns a NULL terminated string.
*
*******************************************************************************/
void  acq_vme_error(int *error,char *string,int len)
{
   char *s1;

/*
*   First call the FORTRAN callable routine.
*/
   acq_vme_error_(error,string,len);
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
* Call:   CALL ACQ_VME_ERROR(ERROR,STRING)
*
* where:
*           INT*4  ERROR - Error code return by a routine in this package
* return:
*           CHARACTER*(*)  -  ASCII message for this error code.
*******************************************************************************/
void  acq_vme_error__(int *error,char *string,int len)
{
   acq_vme_error_(error,string,len);
}
void  acq_vme_error_(int *error,char *string,int len)
{
  int  i,ierr,j;
  char **cptr;

  static char *status_msg[] = {
    "Acquisition is running",
    "Acquisition is stopped",
    "Acquisition is not initialized",
    "Unknown status code"
};

  static char *vme_err_msg[] = {
    "Unknown command",
    "Invalid Acquisition table index",
    "Start failed. System has not been initialized",
    "Acquisition is already running",
    "Acquisition is already stopped",
    "Initialization error - acquisition is running",
    NULL,
    NULL,
    NULL,

    "Nonexistent crate",
    "Crate off-line",
    "Crate Inhibit stuck",
    "Initialization list CAMAC timeout",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    "FASTBUS module does not respond",
    "Unknown FASTBUS module type",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    "Unknown FERA module type",
    "CAMAC timeout initializing FERA module",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    "No CAMAC interface present",
    "No VME FASTBUS interface present",
    "No FASTBUS 1821 sequencer present",
    NULL,
    "No FERA  interface present",
    "No ORNL trigger module present",
    "No KSC3982 List Sequencer present",
    "Sequencer buffers are too small",
    "Count down list too large",
    "Raw gate list too large",
    "Calculated gate list too large",
    "Special CAMAC readout list too large",
    "Unknown CAMAC module type",
    "Illegal module type in gate specification",

    "Unknown error code"
};

  static char *ether_msg[] = {
      "Ethernet communications error"
};

  for (i=0; i < len; i++) string[i] = ' ';
  if (*error == 0) return;
  ierr = *error;
  if (ierr < 0 && ierr > ETHER_ERROR + 1)
    {
      j = sizeof(vme_err_msg)/sizeof(char *);
      ierr = -ierr;
      if (ierr > j) ierr = j;
      cptr = vme_err_msg + ierr - 1;
      if (*cptr == NULL) cptr = &vme_err_msg[j-1];
    }
  else if (ierr == ETHER_ERROR)
    {
      cptr = ether_msg;
    }
  else
    {
      j = sizeof(status_msg)/sizeof(char *);
      if (ierr <= 0) ierr = j;
      else if (ierr > j) ierr = j;
      cptr = status_msg + ierr - 1;
    }
  if (*error < 0) sprintf(string,"Error = %i: ",*error);
  else  sprintf(string,"Status = %i: ",*error);
  i = strlen(string);;
  len = len - i;
  if (len)
    {
      i = strlen(*cptr);
      if (i >= len) i = len;
      strncat(string,*cptr,(size_t)i);
    }
}
