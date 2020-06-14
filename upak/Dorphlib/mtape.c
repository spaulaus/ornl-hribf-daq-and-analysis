/*      J.R.B. 1/90

revisions:
 
  12/ 1/93  MCSQ  Catch ioctl failures due to EINTR(Interrupted system
                  call).  Return errno when ioctl fails with an error
                  number other than EINTR.

  12/12/93  MCSQ  Version for use on the ALPHA.  Compatable with
                  the Asynchronous I/O routines in aiolib.  Removed
                  all routines having to do with N-buf.  Removed routines
                  for open, close, read and write.   All that remains
                  are routines for tape motion and EOFs.  NOTE: When
                  you use AIO routines, a logical unit used in aiolib
                  is not the same as tlu in these routines.  The function,
                  MTUNIT(lun) will provide the necessary conversion.  For
                  example, 
                            CALL MT_REW(TLU,IERR)
                  would be changed to
                            CALL MT_REW(MTUNIT(LUN),IERR)

--------------------
SOURCE FILE:  /usr/users/beene/Djblibs/mt_fort.c   
LIBRARY:      /usr/users/beene/lib/jblibc1.a
-----------------------------------------------------------------------
   A group of Fortran callable magtape handling routines
   constructed using standard c calls and hence (I hope) portable.
   In the calls below "tape" is the name of the tape (e.g. /dev/nrmt0h),
   nr is the repeat count for operations (integer), and ierr is an 
   error return (integer). 
*****************IMPORTANT USAGE NOTES*******************************
These routines are designed for ALL arguments to be passed by reference.
This means that arguments defined as char in the various routines
should be called with numeric variables as corresponding FORTRAN actual 
arguments. In general when a CHARACTER variable is more convenient, I do
this by equivalencing a numeric variable to it and then using the
numeric variable as the argument. (Note that for optimization reasons
it's better to avoid floating point numeric types for this purpose). 
You can also use the %REF descriptor, but portability issues are then
involved. For example:
        CHARACTER*100 CNAME
        INTEGER NAME
        INTEGER TERMINATE_STRING
        EQUIVALENCE(NAME,CNAME)
        ...
        CNAME='/dev/nrmt0h'
        JERR = TERMINATE_STRING(CNAME)
        CALL MT_OPENRO(NAME,LU)
        ...
The function TERMINATE_STRING is found in the library utility01. It 
puts a NULL after the last non-blank character in its argument. 

Use INTEGER*4 for ALL arguments and you can't go wrong.
**********************************************************************

**********************************************************************
*GENERAL TAPE HANDLING ROUTINES*
**********************************************************************
   IN ALL FOLLOWING ROUTINES:
   ierr   is an error return (==0 means OK)
   tlu    is an io descriptor ( a tape unit is assumed).
   buf    is an io buffer.
   nb_rd  is a byte count (no. of bytes read)
   nb_wt  is a byte count (no. of bytes written)
   nb_req is a byte count (no. of bytes requested in in IO operation.)
   tape   is a string giving the path to the tape unit being opened. 
          IN THE CALLING PROGRAM THE ACTUAL ARGUMENT CORRESPONDING TO
          TAPE MUST BE A NUMERIC TYPE. Make it an integer.
***********************************************************************
-----------------------------------------------------------------------
    MT_CSE(tlu,ierr)                     Clear "serious exception"
-----------------------------------------------------------------------
    MT_REW(tlu,ierr)                     Rewind tape.
-----------------------------------------------------------------------
    MT_REWUL(tlu,ierr)                   Rewind & unload tape.
-----------------------------------------------------------------------
    MT_FR(tlu,nr,ierr)                   Skip nr records forward.
-----------------------------------------------------------------------
    MT_BR(tlu,nr,ierr)                   "    "   "     backward.
-----------------------------------------------------------------------
    MT_FF(tlu,nr,ierr)                   "    "  files forward.
-----------------------------------------------------------------------
    MT_BF(tlu,nr,ierr)                   "    "    "   backward.
-----------------------------------------------------------------------
    MT_WHERE(tlu,pos)                      pos = bytes from BOT.
-----------------------------------------------------------------------
    MT_RESETPOS(tlu,pos)
***********************************************************************
Utility routines: fortran callable
***********************************************************************
    CSWAB(data,bytes)                    swap bytes in data (not io)
-----------------------------------------------------------------------
*/
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/devio.h>
#include <sys/file.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <memory.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
int errno;
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_REW(tlu,ierr)
   rewind tape.
*/
void mt_rew_( int *tlu, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTREW;
    temp.mt_count = 1;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
    lseek(*tlu,0,0);
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WHERE(tlu,pos)
   determine tape position. (Offset in bytes from pt where tape opened).
*/
void mt_where_( int *tlu, int *pos)
{
/*
      tlu  is file descriptor 
      pos  is offset (position) in bytes.
*/
    *pos=tell(*tlu);
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_RESETPOS(tlu,pos)
   determine tape position. (Offset in bytes from pt where tape opened).
*/
void mt_resetpos_( int *tlu, int *pos)
{
/*
      tlu  is file descriptor 
      pos  is offset (position) in bytes.
*/
    *pos=lseek(*tlu,*pos,0);
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_REWUL(tlu,ierr)
   rewind tape.
*/
void mt_rewul_( int *tlu, int *ierr)
{
/*
      tlu  is file descriptor  
*/
    struct mtop temp;
    temp.mt_op = MTOFFL;
    temp.mt_count = 1;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
    lseek(*tlu,0,0);
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_CSE(tlu,ierr)
   clear "serious" exception. (Stangely enough an
   eof is a serious exception in nbuffered mode!!)
*/
void mt_cse_( int *tlu, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTCSE;
    temp.mt_count = 1;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_FR(tlu,nr,ierr)
   skip *nr records forward 
*/
void mt_fr_( int *tlu, int *nr, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTFSR;
    temp.mt_count = *nr;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_BR(tlu,nr,ierr)
   skip *nr records backward 
*/
void mt_br_( int *tlu, int *nr, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTBSR;
    temp.mt_count = *nr;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_FF(tlu,nf,ierr)
   skip *nf files forward 
*/
void mt_ff_( int *tlu, int *nf, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTFSF;
    temp.mt_count = *nf;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_BF(tlu,nf,ierr)
   skip *nf files backward 
*/
void mt_bf_( int *tlu, int *nf, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTBSF;
    temp.mt_count = *nf;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WEOF(tlu,ierr)
   write end of file
*/
void mt_weof_( int *tlu, int *ierr)
{
/*
      tlu  is file descriptor 
*/
    struct mtop temp;
    temp.mt_op = MTWEOF;
    temp.mt_count = 1;
    while ((*ierr=ioctl(*tlu,MTIOCTOP,&temp)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
