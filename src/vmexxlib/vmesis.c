/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                              Copyright(C) 2006
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
*    Environment:  VME acquisition system
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmesis.c
*
*    Description:  Read and control SIS3820 VME scaler
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/14/06    MCSQ         
*
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "orph_udp.h"
#include  "vmexx.h"

extern int  vmeio(unsigned char *, int, unsigned char *, int *);
/*
*   Error codes.  Additional error codes in vmexxlib.h and Dvme/vmexx.h
*/
#define  SIS_SCA_ILLEGAL   -35 /* SCA number out of range                */
#define  SIS_SCA_CODE      -36 /* Illegal operation code                 */

/*
*   Function Prototypes
*/

void sis_sca_ctl__(int *,int *,int *,int *);
void sis_sca_ctl_(int *,int *,int *,int *);
void sis_error__(int *,char *,int );
void sis_error_(int *,char *,int );
void sis_error(int *,char *,int );

/*
*   Global data
*/

static struct sis_ctl bufout;
static union eth {
    struct sis_ctl bufin;
      unsigned char dum[1536];
} eth;

/*****************************************************************************
*
* Call:   CALL SIS_SCA_CTL(SCANUM,FUNC,DAT,IERR)
*
*   call:
*          SCANUM - INT*4   SCA number to write (1 thru 2)
*            FUNC - INT*4   Function code. 0 = read, 1= clear, 
*                           2 = enable counting, 3= disable counting
*
* return:
*          DAT(32)- INT*4   Array.  Modified only for FUNC = 0
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void sis_sca_ctl__(int *scanum,int *func,int *dat,int *ierr)
{
  sis_sca_ctl_(scanum,func,dat,ierr);
}
void sis_sca_ctl_(int *scanum,int *func,int *dat,int *ierr)
{
   int   i,rpylen;

   bufout.func = SIS3820MOD;
   bufout.sca_num = *scanum - 1;
   if (*func < 0 || *func > 9)
     {
       *ierr = SIS_SCA_CODE;
       return;
     }
   bufout.code = *func;
   if (*scanum < 1 || *scanum > 2)
     {
       *ierr = SIS_SCA_ILLEGAL;
       return;
     }
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct sis_ctl),
                                             (unsigned char *)&eth,&rpylen);
   if (*func == 0 || *func == 2)
    {
      for (i=0; i < 33; i++) 
       {
         *dat++ = eth.bufin.data[i];
       }
    }
   if (*func == 6 || *func == 7 || *func == 8)
    {
      *dat++ = eth.bufin.data[0];
    }
}
/******************************************************************************
*
* Call:   CALL SIS_ERROR(ERROR,STRING)
*
* where:
*           INT*4  ERROR - Error code return by a routine in this package
* return:
*           CHARACTER*(*)  -  ASCII message for this error code.
*******************************************************************************/
void  sis_error__(int *error,char *string,int len)
{
   sis_error_(error,string,len);
}
void  sis_error_(int *error,char *string,int len)
{
  int  i,ierr,j;
  char **cptr;

  static char *softerr_msg[] = {
    "Ethernet receive timeout",
    "Ethernet transmit error",
    "Ethernet open failure",
    "Illegal SCA specified",
    "Illegal SCA operation code specified",
    "Unknown error code"
};

  static char *vme_err_msg[] = {
    "Unknown VME function request",
    "VME hardware not available",
    "Unknown error code"
};

  for (i=0; i < len; i++) string[i] = ' ';
  if (*error == 0) return;
  ierr = *error;
  if (ierr < 0 && ierr > -32)
    {
      j = sizeof(vme_err_msg)/sizeof(char *);
      ierr = -ierr;
      if (ierr > j) ierr = j;
      cptr = vme_err_msg + ierr - 1;
    }
  else
    {
      j = sizeof(softerr_msg)/sizeof(char *);
      ierr = -ierr -31;
      if (ierr <= 0) ierr = j;
      else if (ierr > j) ierr = j;
      cptr = softerr_msg + ierr - 1;
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
/******************************************************************************
*
*  Returns an ASCII string for an error code.  This one is for a C caller.
*  It returns a NULL terminated string.
*
*******************************************************************************/
void  sis_error(int *error,char *string,int len)
{
   char *s1;

/*
*   First call the FORTRAN callable routine.
*/
   sis_error__(error,string,len);
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
