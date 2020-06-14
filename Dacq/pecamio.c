/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dacq/pecamio.c
*
*    Description:  FORTRAN callable routines for CAMAC I/O.  These routines
*                  have the same names and arguments as similar routines
*                  used on Concurrent machines at ORNL.  These are intended
*                  only to ease the transition to the new Data Acquisition
*                  system.  Both routines call a process in the VME front-end
*                  processor to execute the CAMAC functions.  The routines
*                  are:
*                    cmcbsc  -  Basic CAMAC I/O routine
*                    bhio    -  CAMAC I/O with error output to standard error.
                     camlist -  CAMAC I/O list processor
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/23/92    MCSQ        Original
*
*    6/26/92    MCSQ        The Perkin-Elmer version of BHIO called a routine
*                           IPBERR to report CAMAC errors.  That error reporting
*                           was included in BHIO for the workstation version.
*                           However, some users had a private version of
*                           IPBERR.  This is now implemented here.  Users should
*                           now link with pcam.a which includes pecamio,
*                           fastbusio and ibperr.
*
*    7/20/92    MCSQ        Added routine camlist which executes a list of
*                           CAMAC operations from user arrays of C, N, A, F
*                           and Data.  It returns the status of each CAMAC
*                           operation in the array STAT.  This is much more
*                           efficient than individual calls to CMCBSC or BHIO
*                           since each packet has space for 184 CAMAC commands.
*
*    8/16/92    MCSQ        The new Ethernet library in /usr/acq/lib does not
*                           work with some FORTRAN codes for reasons
*                           unknown!  Hence, I changed the includes to
*                           use old rotines in /usr/users/mcsq/lan.
*
*   12/30/92    MCSQ        Change to new Ethernet library -
*                             /usr/users/mcsq/vme/lan2
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    4/23/93    MCSQ        Added routine camacio_ which is very similar
*                           to bhio_.  The unused argument in bhio_ has
*                           been removed and the error routine is now
*                           named camerr_.  Mostly the new names are
*                           more descriptive.
*
*                           The library for CAMAC and FASTBUS routines
*                           is now /usr/hhirf/vme/vmelib.a
*
*    2/19/95    MCSQ        The VME system now will retry sending packets
*                           up to 3 times at one second intervals.  Here
*                           we impose a minimum receive timeout of 5 sec.
*
*    9/25/95    MCSQ        New ethernet library in /usr/users/mcsq/Dlan.

*    7/20/02    MCSQ        Ported to Linux
*
*    3/27/03    MCSQ        Minor cleanup.
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <unistd.h>
#include  <errno.h>
#include  "../Dacq/pkt_io.h"
#include  "../Dlan/orph_pf.h"
#include  "../Dvme/cnaf.h"

/*    Error Codes returned by cmcbsc  and bhio                          */

#define  ILLCRATE     0xc004    /* Illegal crate number                 */
#define  ILLMOD       0xc005    /* Illegal module number                */
#define  ILLADDR      0xc006    /* Illegal sub-address                  */
#define  ILLFUNC      0xc009    /* Illegal function code                */
#define  CRATEOFF     0xa000    /* Crate off-line or nonexistent        */

/*  Function prototype              */
extern void ipberr_(char *,int *);
extern void camerr_(char *,int *);
void cmcbsc_(int *,int *,int *,int *,int *,int *,int *,void *,int *,int *);
void bhio_(int *,int *,int *,int *,int *,int *,void *,int *,int *);
void camacio_(int *,int *,int *,int *,int *,void *,int *, int *);
void camlist_(int *,int *,int *,int *,int *,void *,int *,int *);

/*   CAMAC data buffers             */
static struct Vmed xbuf,rbuf;

/*****************************************************************************
*    CAMAC Basic I/O routine
*  Call:    branch - Not currently used
*           c      - Crate number
*           n      - Station number
*           a      - Subaddress
*           f      - Function code
*           m      - Data mode: 0 = 16-bit, non-zero 24-bit(integer*4)
*           tmo    - Timeout (seconds)
*           data   - I/O data array.
*           len    - Number of data to transfer.
*           stat   - Returned status (i.e. X, Q etc).
*****************************************************************************/
void cmcbsc_(int *branch,int *c,int *n,int *a,int *f,int *m,int *tmo,void *data,
              int *len,int *stat)
{
   unsigned short *dat16 = data;
            int   *dat24 = data;
            int   i,j,cnt,status;
   struct cnaf_buffer *out_buf = (struct cnaf_buffer *)xbuf.buf;
   struct cnaf_return *ret_buf = (struct cnaf_return *)rbuf.buf;

/*   Check call parameters for valid values    */
   status = 0;
   if (*c < 0 || *c > 17) status = ILLCRATE;
   else if (*c == 8 || *c == 9) status = ILLCRATE;
   else if (*n <= 0 || *n > 31) status = ILLMOD;
   else if (*a < 0 || *a > 15) status = ILLADDR;
   else if (*f < 0 || *f > 31) status = ILLFUNC;
   if (status)
     {
       *stat = status;      /* return error code for illegal paramaters */
       return;
     }

/*  Dispatch on the type of CAMAC operation    */

   xbuf.len = sizeof(struct cnaf_buffer);
   rbuf.len = sizeof(struct cnaf_buffer);
   switch  (*f & 0x18)
    {
/*
*   CAMAC non-data operations - F(8) thru F(15) and F(24) thru F(31)
*/
      case  0x8:
      case  0x18:
        out_buf->cnafs[0].C = *c;
        out_buf->cnafs[0].N = *n;
        out_buf->cnafs[0].A = *a;
        out_buf->cnafs[0].F = *f;
        out_buf->count = 1;
        status = pkt_io(&xbuf,&rbuf,CNAF,*tmo);
        i = 0;
        break;
/*
*   CAMAC read functions - F(0) thru F(7)
*/
      case  0x0:
        cnt = *len;
        i = 0;
        while (cnt > 0)
         {
           out_buf->cnafs[i].C = *c;
           out_buf->cnafs[i].N = *n;
           out_buf->cnafs[i].A = *a;
           out_buf->cnafs[i].F = *f;
           out_buf->cnafs[i].Data = 0;
           i++;
           cnt--;
           if (i == MAX_CNAF || cnt == 0)
             {
               out_buf->count = i;
               status = pkt_io(&xbuf,&rbuf,CNAF,*tmo);
               if (status) break;
               j = 0;
               while (j < i)
                {
                  if (*m) *dat24++ = ret_buf[j].Data;
                  else  *dat16++ = ret_buf[j].Data;
                  j++;
                }
               if (cnt) i = 0;
               else  i--;
             }
         }
        break;
/*
*   CAMAC write operations - F(16) thru F(23)
*/
      case  0x10:
        cnt = *len;
        i = 0;
        while (cnt > 0)
         {
           out_buf->cnafs[i].C = *c;
           out_buf->cnafs[i].N = *n;
           out_buf->cnafs[i].A = *a;
           out_buf->cnafs[i].F = *f;
           if (*m) out_buf->cnafs[i].Data = *dat24++;
           else  out_buf->cnafs[i].Data = *dat16++;
           i++;
           cnt--;
           if (i == MAX_CNAF || cnt == 0)
             {
               out_buf->count = i;
               status = pkt_io(&xbuf,&rbuf,CNAF,*tmo);
               if (status) break;
               if (cnt) i = 0;
               else i--;
             }
         }
        break;
      default:
        break;
    }
/*
*   Build return status word in format expected by Elmer-Perkins routines.
*   If any error from ethernet, return timeout status.
*/
   if (status) *stat = 0x80;
   else if (ret_buf[i].Online == 0) *stat = CRATEOFF;
   else
     {
       if (ret_buf[i].X == 0) status = 2;
       if (ret_buf[i].Q == 0) status ^= 1;
       *stat = status;
     }
   return;
}
/*****************************************************************************
*   CAMAC I/O with error status output
*
*  Call:  mode  - data array size. 0 = 16-bit, nonzero = 24 bit
*         lu    - branch (not currently used)
*         c     - Crate number
*         n     - Station number
*         a     - Subaddress
*         f     - Function code
*         data  - data array
*         nwds  - size of data array
*         istat - Returned status
*****************************************************************************/
void bhio_(int *mode,int *lu,int *c,int *n,int *a,int *f,void *data,int *nwds,
           int *istat)
{
  int tmo = 5;
  char ipb[4];
  int stat;

  cmcbsc_(lu,c,n,a,f,mode,&tmo,data,nwds,istat);
  stat = *istat;
  if(stat != 0) 
    {
      ipb[0] = *c;
      ipb[1] = *n;
      ipb[2] = *a;
      ipb[3] = *f;
      ipberr_(ipb,&stat);
    }
}
/*****************************************************************************
*   This routine is almost the same as BHIO.  Differences are 
*     1) Unused calling argument in bhio_ is removed
*     2) Calls error routine camerr_.  However, the default
*        camerr_ simply calls ipberr_
*
*   Real reason for this is to make function names a little more
*   readable.
*
*  Call:  mode  - data array size. 0 = 16-bit, nonzero = 24 bit
*         c     - Crate number
*         n     - Station number
*         a     - Subaddress
*         f     - Function code
*         data  - data array
*         nwds  - size of data array
*         istat - Returned status
*****************************************************************************/
void camacio_(int *mode,int *c,int *n,int *a,int *f,void *data,
                                                         int *nwds, int *istat)
{
  int tmo = 5;
  char ipb[4];
  int lu,stat;

  cmcbsc_(&lu,c,n,a,f,mode,&tmo,data,nwds,istat);
  stat = *istat;
  if(stat != 0) 
    {
      ipb[0] = *c;
      ipb[1] = *n;
      ipb[2] = *a;
      ipb[3] = *f;
      camerr_(ipb,&stat);
    }
}
/*****************************************************************************
*    CAMAC List Processor
*  Call:    c      - Crate number array
*           n      - Station number array
*           a      - Subaddress array
*           f      - Function code array
*           m      - Data mode: 0 = 16-bit, non-zero 24-bit(integer*4)
*           data   - I/O data array.
*           len    - Number of data to transfer.
*           stat   - Returned status array(i.e. X, Q etc).
*****************************************************************************/
void camlist_(int *c,int *n,int *a,int *f,int *m,void *data,
              int *len,int *stat)
{
   int  tmo = 5;
   unsigned short *dat16,*dat16s;
            int   *dat24,*dat24s;
   int  i,j,cnt,estatus;
   int *cc,*nn,*aa,*ff;
   int *fs;
   int *status,*statusb;
   struct cnaf_buffer *out_buf = (struct cnaf_buffer *)xbuf.buf;
   struct cnaf_return *ret_buf = (struct cnaf_return *)rbuf.buf;

   xbuf.len = sizeof(struct cnaf_buffer);
   rbuf.len = sizeof(struct cnaf_buffer);
   cc = c;
   nn = n;
   aa = a;
   ff = fs = f;
   dat24 = dat24s = data;
   dat16 = dat16s = data;
   status = statusb = stat;
   i = 0;
   cnt = *len;
   while (cnt > 0)
    {
/*   Check call parameters for valid values    */
      *status = 0;
      if (*cc < 0 || *cc > 17) *status = ILLCRATE;
      else if (*cc == 8 || *cc == 9) *status = ILLCRATE;
      else if (*nn <= 0 || *nn > 31) *status = ILLMOD;
      else if (*aa < 0 || *aa > 15) *status = ILLADDR;
      else if (*ff < 0 || *ff > 31) *status = ILLFUNC;
      out_buf->cnafs[i].C = *cc++ & 0xf;
      out_buf->cnafs[i].N = *nn++ & 0x1f;
      out_buf->cnafs[i].A = *aa++ & 0xf;
      out_buf->cnafs[i].F = *ff & 0x1f;
      if ((*ff++ & 0x18) == 0x10)
       {
/*
*   CAMAC write operations - F(16) thru F(23)
*/
         if (*m) out_buf->cnafs[i].Data = *dat24;
         else  out_buf->cnafs[i].Data = *dat16;
       }
     status++;
     dat16++;
     dat24++;
     i++;
     cnt--;
     if (i == MAX_CNAF || cnt == 0)
       {
         out_buf->count = i;
         status = statusb;
         estatus = pkt_io(&xbuf,&rbuf,CNAF,tmo);
         if(estatus)
          {
            while(i > 0) {*status++ = 0x80; i--;}
            while(cnt > 0) {*status++ = 0x80; cnt--;}
            break;
          }
         ff = fs;
         dat16 = dat16s;
         dat24 = dat24s;
         j = 0;
         while (i > 0)
          {
            if ((*ff++ & 0x18) == 0)
             {
/*
*   CAMAC read operations - F(0) thru F(15)
*/
               if(*m) *dat24 = ret_buf[j].Data;
               else  *dat16 = ret_buf[j].Data;
             }
            if (*status == 0)
             {
               if (ret_buf[j].Online == 0) *status = CRATEOFF;
               else
                {
                  if (ret_buf[j].X == 0) *status = 2;
                  if (ret_buf[j].Q == 0) *status ^= 1;
                }
             }
            status++;
            dat16++;
            dat24++;
            j++;
            i--;
          } 
         fs = ff;
         dat16s = dat16;
         dat24s = dat24;
         statusb = status;
       }
    }
   return;
}
