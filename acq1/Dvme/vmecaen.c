/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                              Copyright(C) 2001
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
*    File:         /usr/users/mcsq/Dlinux/Dvme/vmecaen.c
*
*    Description:  Read/write CAEN ADCs and TDCs
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/16/01    MCSQ         
*
*    4/ 9/03    MCSQ      Ported to Linux
*    7/14/06    MCSQ        Add routine for read and control of the SIS3820
*                           VME scaler module.
*    7/29/2009  RLV       Added Catalin Matei V792 code
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "../Dlan/orph_pf.h"
#include  "../Dvme/vmexx.h"

/*
*   Error codes.  Additional error codes in vmexxlib.h and Dvme/vmexx.h
*/
#define  CAEN_ADC_ILLEGAL  -35 /* ADC number out of range                */
#define  CAEN_TDC_ILLEGAL  -36 /* TDC number out of range                */
#define  CAEN_THRES_INVAL  -37 /* Threshold out of range                 */
#define  CAEN_RANGE_INVAL  -38 /* Range out of range                     */
#define  CAEN_QDC_ILLEGAL  -39 /* QDC number out of range                */
#define  CAEN_IPED_INVAL   -40 /* Iped out of range                      */

/*
*   Function Prototypes
*/

void caen_adc_read__(int *,int *,int *);
void caen_adc_read_(int *,int *,int *);
void caen_adc_write__(int *,int *,int *);
void caen_adc_write_(int *,int *,int *);
void caen_tdc_read__(int *,int *,int *,int *,int *);
void caen_tdc_read_(int *,int *,int *,int *,int *);
void caen_tdc_write__(int *,int *,int *,int *,int *);
void caen_tdc_write_(int *,int *,int *,int *,int *);
void caen_qdc_read__(int *,int *,int *,int *);
void caen_qdc_read_(int *,int *,int *,int *);
void caen_qdc_write__(int *,int *,int *,int *);
void caen_qdc_write_(int *,int *,int *,int *);
void caen_error__(int *,char *,int );
void caen_error_(int *,char *,int );
void caen_error(int *,char *,int );

/*
*   Global data
*/

static struct caen_ctl bufout;
static union eth {
    struct caen_ctl bufin;
      unsigned char dum[1536];
} eth;

/*****************************************************************************
*
* Call:   CALL CAEN_ADC_READ(ADCNUM,DAT,IERR)
*
*   call:
*          ADCNUM - INT*4   ADC number to write (1 thru 12)
*
* return:
*          DAT    - INT*4  Array
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_adc_read__(int *adcnum,int *dat,int *ierr)
{
  caen_adc_read_(adcnum,dat,ierr);
}
void caen_adc_read_(int *adcnum,int *dat,int *ierr)
{
   int   i,rpylen;

   bufout.func = CAEN785;
   bufout.cvt_num = *adcnum - 1;
   bufout.rw = 0;
   if (*adcnum < 1 || *adcnum > 12)
     {
       *ierr = CAEN_ADC_ILLEGAL;
       return;
     }
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
   for (i=0; i < 32; i++) 
     {
       *dat++ = eth.bufin.data[i];
     }
}
/*****************************************************************************
*
* Call:   CALL CAEN_ADC_WRITE(ADCNUM,VAL,IERR)
*
*   call:  
*          ADCNUM - INT*4   ADC number to write (1 thru 12)
*          VAL    - INT*4   Array of values to write to specified ADC
* return:
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_adc_write__(int *adcnum,int *dat,int *ierr)
{
   caen_adc_write_(adcnum,dat,ierr);
}
void caen_adc_write_(int *adcnum,int *dat,int *ierr)
{
   int   rpylen,i;

   bufout.func = CAEN785;
   bufout.cvt_num = *adcnum - 1;
   bufout.rw = 1;
   if (*adcnum < 1 || *adcnum > 12)
     {
       *ierr = CAEN_ADC_ILLEGAL;
       return;
     }
   for (i=0; i < 32; i++)
     {
       if (*dat < 0 || *dat > 255)
         {
           *ierr = CAEN_THRES_INVAL;
           return;
         }
       bufout.data[i] = *dat++;
     }
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
}
/*****************************************************************************
*
* Call:   CALL CAEN_TDC_READ(TDCNUM,DAT,RANGE,MODE,IERR)
*
*   call:
*          TDCNUM - INT*4   TDC number to write (1 thru 12)
*
* return:
*          DAT    - INT*4  Array
*          RANGE  _ INT*4   TDC range
*          MODE   _ INT*4   Mode.  0 means common start
*                                  1 means common stop
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_tdc_read__(int *tdcnum,int *dat,int *range,int *mode,int *ierr)
{
   caen_tdc_read_(tdcnum,dat,range,mode,ierr);
}
void caen_tdc_read_(int *tdcnum,int *dat,int *range,int *mode,int *ierr)
{
   int   i,rpylen;

   bufout.func = CAEN775;
   bufout.rw = 0;
   bufout.cvt_num = *tdcnum - 1;
   if (*tdcnum < 1 || *tdcnum > 12)
     {
       *ierr = CAEN_TDC_ILLEGAL;
       return;
     }
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
   for (i=0; i < 32; i++)
     {
       *dat++ = eth.bufin.data[i];
     }
   *range = eth.bufin.range;
   *range = *range & 0xff;
   *mode = eth.bufin.mode;
}
/*****************************************************************************
*
* Call:   CALL CAEN_TDC_WRITE(TDCNUM,VAL,RANGE,MODE,IERR)
*
*   call: 
*          TDCNUM - INT*4   TDC number to write (1 thru 12)
*          VAL    - INT*4   Array of values to write to specified TDC
*          RANGE  _ INT*4   TDC range
*          MODE   _ INT*4   Mode.  0 means common start
*                                  1 means common stop
* return:
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_tdc_write__(int *tdcnum,int *dat,int *range,int *mode,int *ierr)
{
   caen_tdc_write_(tdcnum,dat,range,mode,ierr);
}
void caen_tdc_write_(int *tdcnum,int *dat,int *range,int *mode,int *ierr)
{
   int   rpylen,i;

   bufout.func = CAEN775;
   bufout.cvt_num = *tdcnum - 1;
   bufout.rw = 1;
   if (*tdcnum < 1 || *tdcnum > 12)
     {
       *ierr = CAEN_TDC_ILLEGAL;
       return;
     }
   for (i=0; i < 32; i++)
     {
       if (*dat < 0 || *dat > 255)
         {
           *ierr = CAEN_THRES_INVAL;
           return;
         }
       bufout.data[i] = *dat++;
     }
   if (*range < 30 || *range > 255)
     {
       *ierr = CAEN_RANGE_INVAL;
       return;
     }
   bufout.range = *range;
   bufout.mode = *mode;
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
}
/*****************************************************************************
*
* Call:   CALL CAEN_QDC_READ(QDCNUM,DAT,IPED,IERR)
*
*   call: 
*          QDCNUM - INT*4   QDC number to write (1 thru 10)
*
* return:
*          DAT    - INT*4   Array
*          IPED   _ INT*4   QDC pedestal
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_qdc_read__(int *qdcnum,int *dat,int *iped,int *ierr)
{
   caen_qdc_read_(qdcnum,dat,iped,ierr);
}  
void caen_qdc_read_(int *qdcnum,int *dat,int *iped,int *ierr)
{
   int   i,rpylen;

   bufout.func = CAEN792;
   bufout.rw = 0;
   bufout.cvt_num = *qdcnum - 1;
   if (*qdcnum < 1 || *qdcnum > 12)
     {
       *ierr = CAEN_QDC_ILLEGAL;
       return;
     }
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
   for (i=0; i < 32; i++)
     {
       *dat++ = eth.bufin.data[i];
     }
   *iped = eth.bufin.range;  //range is the same register in the 775
   *iped = *iped & 0xff;
}
/*****************************************************************************
*
* Call:   CALL CAEN_QDC_WRITE(QDCNUM,VAL,IPED,IERR)
*
*   call: 
*          QDCNUM - INT*4   QDC number to write (1 thru 10)
*          VAL    - INT*4   Array of values to write to specified QDC
*          IPED   _ INT*4   QDC pedestal
* return:
*          IERR   - INT*4 - Returned status of call.  Zero means OK
*                           and nonzero means an error.
*
*****************************************************************************/
void caen_qdc_write__(int *qdcnum,int *dat,int *iped,int *ierr)
{  
   caen_qdc_write_(qdcnum,dat,iped,ierr);
}
void caen_qdc_write_(int *qdcnum,int *dat,int *iped,int *ierr)
{  
   int   rpylen,i; 

   bufout.func = CAEN792;
   bufout.cvt_num = *qdcnum - 1;
   bufout.rw = 1;
   if (*qdcnum < 1 || *qdcnum > 12)
     {
       *ierr = CAEN_QDC_ILLEGAL;
       return;
     }
   for (i=0; i < 32; i++)
     {
       if (*dat < 0 || *dat > 255)
         {
           *ierr = CAEN_THRES_INVAL;
           return;
         }
       bufout.data[i] = *dat++;
     }
   if (*iped < 0 || *iped > 255)
     {
       *ierr = CAEN_IPED_INVAL;
       return;
     }
   bufout.range = *iped;
   *ierr = vmeio((unsigned char *)&bufout,sizeof(struct caen_ctl),
                                             (unsigned char *)&eth,&rpylen);
}
/******************************************************************************
*
* Call:   CALL CAEN_ERROR(ERROR,STRING)
*
* where:
*           INT*4  ERROR - Error code return by a routine in this package
* return:
*           CHARACTER*(*)  -  ASCII message for this error code.
*******************************************************************************/
void  caen_error__(int *error,char *string,int len)
{
   caen_error_(error,string,len);
}
void  caen_error_(int *error,char *string,int len)
{
  int  i,ierr,j;
  char **cptr;

  static char *softerr_msg[] = {
    "Ethernet receive timeout",
    "Ethernet transmit error",
    "Ethernet open failure",
    "Illegal ADC specified",
    "Illegal TDC specified",
    "Invalid Threshold value",
    "Invalid TDC Range value",
    "Illegal QDC specified",
    "Invalid QDC Iped value",
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
void  caen_error(int *error,char *string,int len)
{
   char *s1;

/*
*   First call the FORTRAN callable routine.
*/
   caen_error__(error,string,len);
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
