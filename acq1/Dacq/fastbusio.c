/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dacq/fastbusio.c
*
*    Description:  FORTRAN callable routines for FASTBUS. This routine
*                  provides host access to LeCroy 1821 Sequencer registers
*                  for setup of the fastbus system. 
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/22/92    MCSQ         
*
*    7/20/92    MCSQ       Add routine FBLIST which processes a user supplied
*                          list of register reads/writes to the 1821 sequencer.
*                          User must build arrays of register number, A,
*                          Read/Write, F, and Data.  A status for each read/
*                          write operation in the status array.
*
*    8/16/92    MCSQ       The new Ethernet library in /usr/acq/lib does not
*                          work with some FORTRAN codes for reasons
*                          unknown!  Changed includes to use old versions
*                          in /usr/users/mcsq/lan.
*
*   12/30/92    MCSQ       Changed to new Ethernet library -
*                           /usr/users/mcsq/lan2
*
*    1/ 9/93    MCSQ       Change for multiple VME systems.  The VME
*                          ethernet address is by default "vme".  If
*                          the enviroment variable VME is set, that
*                          string is used instead of "vme".
*
*    5/21/93    MCSQ       Change error code returned when fastbus
*                          system not available.  Front-end software
*                          returns a status = -1.  We now return a
*                          CRATEOFF status code to the caller for this
*                          case.    
*
*    9/25/95    MCSQ       New ethernet library in /usr/users/mcsq/Dlan.
*
*    3/27/03    MCSQ       Ported to Linux
*****************************************************************************/
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "../Dacq/pkt_io.h"
#include  "../Dlan/orph_pf.h"
#include  "../Dvme/fastbus.h"

/*    Function prototypes   */


void fastbusio_(int *,int *,unsigned short *,int *,int *);
void fblist_(int *,int *,unsigned short *,int *,int *);

/*    Error Codes returned by  fastbusio and fblist                     */

#define  ILLADDR      0xc006    /* Illegal sub-address                  */
#define  CRATEOFF     0xa000    /* Crate off-line or nonexistent        */

/*   FASTBUS  data buffers             */
static struct Vmed xbuf,rbuf;

/*****************************************************************************
*    FASTBUS I/O routine
*  Call:    a      - Subaddress - 0 <= a <= 7
*           f      - Function code. 0 = read, nonzero = write.
*           data   - I/O data array.
*           len    - number of elements in the I/O array.
*           stat   - Returned status.
*****************************************************************************/
void fastbusio_(int *a,int *f,unsigned short *data,int *len,int *stat)
{
   unsigned short *dat16 = data;
   int  i,j,cnt,status;

   struct fast_buffer *out_buf = (struct fast_buffer *)xbuf.buf;
   struct fast_return *ret_buf = (struct fast_return *)rbuf.buf;

/*   Check call parameters for valid values    */
   status = 0;
   if (*a < 0 || *a > 7)
     {
       *stat = ILLADDR;       /* return error code for illegal parameters */
       return;
     }

   xbuf.len = sizeof(struct fast_buffer);
   rbuf.len = sizeof(struct fast_buffer);
   if (*f == 0)
    {
/*
*   Read function 
*/
      cnt = *len;
      i = 0;
      while (cnt > 0)
       {
         out_buf->fasts[i].A = *a;
         out_buf->fasts[i].F = 1;
         out_buf->fasts[i].Data = 0;
         i++;
         cnt--;
         if (i == MAX_FAST || cnt == 0)
           {
             out_buf->count = i;
             status = pkt_io(&xbuf,&rbuf,FASTBUS,5);
             if (status) break;
             j = 0;
             while (j < i)
              {
                *dat16++ = ret_buf[j].Data;
                j++;
              }
             if (cnt) i = 0;
             else  i--;
           }
       }
    }
   else
    {
/*
*   Write operation
*/
      cnt = *len;
      i = 0;
      while (cnt > 0)
       {
         out_buf->fasts[i].A = *a;
         out_buf->fasts[i].F = 0;
         out_buf->fasts[i].Data = *dat16++;
         i++;
         cnt--;
         if (i == MAX_FAST || cnt == 0)
           {
             out_buf->count = i;
             status = pkt_io(&xbuf,&rbuf,FASTBUS,5);
             if (status) break;
             if (cnt) i = 0;
             else i--;
           }
       }
    }
/*
*   Build return status word in format expected by Elmer-Perkins routines.
*   If any error from ethernet, return timeout status.
*/
   if (status) *stat = 0x80;
   else   *stat = ret_buf[i].Status;
   if (*stat == -1) *stat = CRATEOFF;
   return;
}
/*****************************************************************************
*    FASTBUS List Processor
*  Call:    a      - Register address array
*           f      - Function code array
*           data   - I/O data array.
*           len    - Number of data to transfer.
*           stat   - Returned status array.
*****************************************************************************/
void fblist_(int *a,int *f,unsigned short *data,int *len,int *stat)
{
   unsigned short *dat16,*dat16s;
   int  i,j,cnt,estatus;
   int *aa,*ff,*fs;
   int *status,*statusb;

   struct fast_buffer *out_buf = (struct fast_buffer *)xbuf.buf;
   struct fast_return *ret_buf = (struct fast_return *)rbuf.buf;

   aa = a;
   ff = fs = f;
   dat16 = dat16s = data;
   status = statusb = stat;
   cnt = *len;
   xbuf.len = sizeof(struct fast_buffer);
   rbuf.len = sizeof(struct fast_buffer);
   i = 0;
   while (cnt > 0)
    {
/*   Check call parameters for valid values    */
      *status = 0;
      if (*aa < 0 || *aa > 7) *status = ILLADDR;
      out_buf->fasts[i].A = *aa++ & 0x7;
      out_buf->fasts[i].F = 1;
      if (*ff++ != 0)
       {
/*
*  Write register in 1821 Sequencer
*/
         out_buf->fasts[i].F = 0;
         out_buf->fasts[i].Data = *dat16;
       }
     status++;
     dat16++;
     i++;
     cnt--;
     if (i == MAX_FAST || cnt == 0)
       {
         out_buf->count = i;
         status = statusb;
         estatus = pkt_io(&xbuf,&rbuf,FASTBUS,5);
         if(estatus)
          {
            while(i > 0) {*status++ = 0x80; i--;}
            while(cnt > 0) {*status++ = 0x80; cnt--;}
            break;
          }
         ff = fs;
         dat16 = dat16s;
         j = 0;
         while (i > 0)
          {
            if (*ff++ == 0)
             {
/*
*   Read register in 1821 Sequencer
*/
               *dat16 = ret_buf[j].Data;
             }
            if (*status == 0) *status = ret_buf[j].Status;
            status++;
            dat16++;
            j++;
            i--;
          } 
         fs = ff;
         dat16s = dat16;
         statusb = status;
       }
    }
   return;
}
