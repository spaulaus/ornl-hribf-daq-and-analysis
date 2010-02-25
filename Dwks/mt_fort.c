/*      J.R.B. 1/90

revisions:
 
  12/ 1/93  MCSQ  Catch ioctl failures due to EINTR(Interrupted system
                  call).  Return errno when ioctl fails with an error
                  number other than EINTR.

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
    MT_OPENRO(tape,tlu)                  Open tape(name), Read Only.
-----------------------------------------------------------------------
    MT_OPENRW(tape,tlu)                  Open tape(name), Read & Write.
-----------------------------------------------------------------------
    MT_CLOSE(tlu)                        Close tape (tlu).
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
    MT_READ(tlu,buf,nby_req,nby_rd,ierr)   Read.  
-----------------------------------------------------------------------
    MT_WRITE(tlu,buf,nby_req,nby_wt,ierr)  Write.
-----------------------------------------------------------------------
    MT_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
-----------------------------------------------------------------------
    DEV_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
-----------------------------------------------------------------------
    MT_WHERE(tlu,pos)                      pos = bytes from BOT.
-----------------------------------------------------------------------
    MT_RESETPOS(tlu,pos)
***********************************************************************
ROUTINES FOR ULTRIX n-buffered (n-b) IO and/or non-blocking (n-d) IO
See manual pages nbuf(4) for more information on what these terms mean.
***********************************************************************
    MT_NB_OFF(tlu,ierr)
Turn off n-b io and wait for completion of any outstanding requests.
-----------------------------------------------------------------------
    MT_NB_ON(tlu,count,ierr)
Turn on n-buffered (n-b) io. count is the number of pending IO's to  
allow on this descriptor .
-----------------------------------------------------------------------
    MT_ND_ON(tlu,ierr)
Turn on non-blocking (n-d) io .
-----------------------------------------------------------------------
    MT_NBND_ON(tlu,count,ierr)
Turn on both n-b & n-d io .
-----------------------------------------------------------------------
    MT_WAIT(tlu,buf,nb_rd,ierr)
Wait for pending n-b IO.
-----------------------------------------------------------------------
    MT_WAITND(tlu,buf,nb_rd,limit,ierr)    
Wait for pending n-b & n-d IO.  limit is timeout in milliseconds.
-----------------------------------------------------------------------
    MT_HOLDNB(tlu,limit,ierr)    
Uses the select system call to wait for an IO to be possible on
descriptor tlu. If only n-b reading is on, and MT_NB_ON was called with 
count=1, then this amounts to a wait for io completion.
ierr = 998 or 997 if the hold times out.
ierr = 0 if io can be done on this descriptor.
Note that the MT_HOLDNB routines do NOT actually complete the io. Their
main function for me is to help me detect hangups. The MT_WAIT routine
should be called after a MT_HOLD type rotine returns to force io
completion.
-----------------------------------------------------------------------
    MT_R_HOLDNB(tlu,limit,ierr)  
Same as MT_HOLDNB but only checks on read operations. 
-----------------------------------------------------------------------
    MT_W_HOLDNB(tlu,limit,ierr)  
Same as MT_HOLDNB but only checks on write operations.
-----------------------------------------------------------------------
    MT_READW(tlu,buf,nby_req,nby_rd,ierr)  
Read(wait). Turns off n-b io first. 
-----------------------------------------------------------------------
    MT_WRITEW(tlu,buf,nby_req,nby_wt,ierr) 
Write(wait).  Turns off n-b io first. 
-----------------------------------------------------------------------
***********************************************************************
Utility routines: fortran callable
***********************************************************************
    CSWAB(data,bytes)                    swap bytes in data (not io)
-----------------------------------------------------------------------
    FCASEFIX(str)                        convert str to lower case 
                                       and terminate it properly.
                                       MAX_LEN = 80 characters.
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
#define WTM_EOF     999
#define IO_TIMEOUT  998
#define IO_WTIMEOUT 997
#define LOOPS_MS    20
int errno;
int casefix(char *);
/***************************************/
/*
   Fortran call sequence:
   CALL MT_OPENRO(tape,tlu)
   open tape for READ only.
*/

void mt_openro_(char *tape, int *tlu)
{
/*    tape is device name
      tlu  is file descriptor returned (error if negative).
*/
    if(casefix(tape) > 0)
      *tlu=open(tape,O_RDONLY,0666);
    else
      *tlu = -1;
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_OPENRW(tape,tlu)
   open tape for READ & WRITE.
*/

void mt_openrw_(char *tape, int *tlu)
{
/*    tape is device name
      tlu  is file descriptor returned (error if negative).
*/
    if(casefix(tape) > 0)
      *tlu=open(tape,O_RDWR,0666);
    else
      *tlu = -1;
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_CLOSE(tlu)
   close tape
*/
void mt_close_(int *tlu)
{
/*
      tlu  is file descriptor
*/
    close(*tlu);
 }
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
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
   get SCSI stautus registers.
*/
void mt_getstatus_( int *tlu, int *t, int *s, int *e, int *r, int *ierr)
{
/*
      tlu  is file descriptor 
      t is device type
      s is status register
      e is error register
      r is residual count
*/
    struct mtget temp2;

    while ((*ierr=ioctl(*tlu,MTIOCGET,&temp2)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }

    *t = temp2.mt_type;
    *s = temp2.mt_dsreg;
    *e = temp2.mt_erreg;
    *r = temp2.mt_resid;
}
/*****************************************/
/*
   Fortran call sequence:
   CALL DEV_GETSTATUS(tlu,type,stat_r,err_r,resid,ierr)
   get status from controlling device driver.
   See devio(4)
*/
void dev_getstatus_( int *tlu, unsigned *s_e, unsigned *h_e, 
         int *st, int *ierr)
{
/*
      tlu  is file descriptor 
      s_e  is soft error count 
      h_e  is hard error count
      st   is driver "generic status mask"  
*/
    struct devget temp2;

    while ((*ierr=ioctl(*tlu,DEVIOCGET,&temp2)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }

    *s_e = temp2.soft_count;
    *h_e = temp2.hard_count;
    *st = temp2.stat;
}

#ifdef __ultrix

/*  Routines for ULTRIX only             */
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_READW(tlu,buf,nby_req,nb_rd,ierr)
   Read and wait for io completion on discriptor tlu.
   Terminates N-buffered read if enabled.
   nby_req = bytes requested (input)
   nby_rd = bytes read (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_readw_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    int count;
    int nbyte;
    count = 0;
    while ((nbyte = ioctl(*tlu,FIONBUF,&count)) == -1)
      {
        if (errno == EINTR) continue;
        break;
      }
    *ierr = 0;
    *nb_rd = read(*tlu,buf,*nby_req);
    if (*nb_rd < 0) {
       *ierr = errno;
       return;
    }
    else if (*nb_rd == 0){
       *ierr = WTM_EOF;
       return;
    }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WRITEW(tlu,buf,nby_req,nb_rd,ierr)
   write and wait for io completion on discriptor tlu.
   Terminates N-buffered io if enabled.
   nby_req = bytes requested (input)
   nby_wt = bytes actually written. =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_writew_(int *tlu, char *buf, int *nby_req, int *nb_wt, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    int count;
    int nbyte;
    count = 0;
    while ((nbyte = ioctl(*tlu,FIONBUF,&count)) == -1)
      {
        if (errno == EINTR) continue;
        break;
      }
    *ierr = 0;
    *nb_wt = write(*tlu,buf,*nby_req);
    if (*nb_wt < 0) {
       *ierr = errno;
       return;
    }
    else if (*nb_wt == 0){
       *ierr = WTM_EOF;
       return;
    }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_READ(tlu,buf,nby_req,nb_rd,ierr)
   Read on discriptor tlu.
   nby_req = bytes requested (input)
   nby_rd = bytes read (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_read_(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    *ierr = 0;
    *nb_rd = read(*tlu,buf,*nby_req);
    if (*nb_rd < 0) {
       *ierr = errno;
    }
    if (*nb_rd == 0) {
       *ierr = WTM_EOF;
    }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WRITE(tlu,buf,nby_req,nb_w,ierr)
   Read on discriptor tlu.
   nby_req = bytes requested (input)
   nby_w = bytes written (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_write_(int *tlu, char *buf, int *nby_req, int *nb_w, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    *ierr = 0;
    *nb_w = write(*tlu, buf, *nby_req);
    if (*nb_w < 0) {
       *ierr = errno;
    }
    else if (*nb_w == 0){
       *ierr = WTM_EOF;
    }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WAIT(tlu,buf,nbyte,ierr)
   Wait for io completion on discriptor tlu.
   nbyte = bytes read (returned). =0 if eof. =-1 if error.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_wait_(int *tlu, char *buf, int *nbyte, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    *ierr = 0;
    while ((*nbyte = ioctl(*tlu,FIONBDONE,&buf)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
    if (*nbyte == 0){
       *ierr = WTM_EOF;
    }
}
/*****************************************/
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_WAITND(tlu,buf,nbyte,limit,ierr)
   Wait for io completion on discriptor tlu.
   nbyte = bytes read (returned). =0 if eof. =-1 if error.
   limit = number of tries before abandoning.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_waitnd_(int *tlu, char *buf, int *nbyte, int *limit, int *ierr)
{
/*    tape is device name
      tlu  is file descriptor 
*/
    int i,nloop;
    *ierr = 0;
    nloop = (*limit) * LOOPS_MS;
    if(nloop <= 10) 
       nloop=100;
    for (i=0; i < nloop; i++) {
       *ierr=0;
       while ((*nbyte = ioctl(*tlu,FIONBDONE,&buf)) == -1)
         {
           if (errno == EINTR) continue;
           break;
         }
       if (*nbyte == -1)
         {
           if((errno != -1)&&(errno != EWOULDBLOCK)&&(errno == EAGAIN)) break;
         }
       else  break;
    }
/*
   printf("NBread: %d   errno: %d  iter: %d\n",*nbyte,errno,i);
*/
    if (*nbyte < 0){
       *ierr = errno;
       if((errno == -1)||(errno == EWOULDBLOCK)||(errno == EAGAIN))
              *ierr = IO_TIMEOUT; 
       }
    else if (*nbyte == 0)
       *ierr = WTM_EOF;
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_NB_OFF(tlu,ierr)
   Terminate N-buffered IO on tlu.
   ierr = 0 if successful, = errno otherwise.
*/
void mt_nb_off_(int *tlu, int *ierr)
{
/*   
      tlu  is file descriptor 
*/
    int count;
    int nbyte;
    *ierr = 0;
    count = 0;
    while ((nbyte = ioctl(*tlu,FIONBUF,&count)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_IOCLR(tlu,ierr)
   Terminate N-buffered IO on tlu.
   ierr = 0 if successful, = errno otherwise.
***This routine is identical to MT_NB_OFF. It is kept for compatibilty***
*/
void mt_ioclr_(int *tlu, int *ierr)
{
/*   
      tlu  is file descriptor 
*/
    int count;
    int nbyte;
    *ierr = 0;
    count = 0;
    while ((nbyte = ioctl(*tlu,FIONBUF,&count)) == -1)
      {
        if (errno == EINTR) continue;
       *ierr = errno;
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_NB_ON(tlu,count,ierr)
   Initiate N-buffered IO on tlu.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_nb_on_(int *tlu, int *count, int *ierr)
{
/*   
      tlu  is file descriptor 
*/
    int nbyte;
    *ierr = 0;
    while ((nbyte = ioctl(*tlu,FIONBUF,count)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        printf("ERROR initiating N-bufferd read: IERR = %d \n",errno);
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_NBND_ON(tlu,count,ierr)
   Initiate N-buffered, non blocking IO on tlu.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_nbnd_on_(int *tlu, int *count, int *ierr)
{
/*   
      tlu  is file descriptor 
*/
    int nbyte;
    *ierr = 0;
    nbyte=fcntl(*tlu,F_SETFL,FNDELAY);
    if (nbyte < 0) {
       *ierr = errno;
       printf("ERROR setting FNDELAY flag: IERR = %d \n",errno);
     }
    while ((nbyte = ioctl(*tlu,FIONBUF,count)) == -1)
      {
        if (errno == EINTR) continue;
        *ierr = errno;
        printf("ERROR initiating N-bufferd read: IERR = %d \n",errno);
        break;
      }
}
/*****************************************/
/*
   Fortran call sequence:
   CALL MT_ND_ON(tlu,ierr)
   Initiate N-buffered, non blocking IO on tlu.
   ierr = 0 if successful, = errno otherwise.
*/

void mt_nd_on_(int *tlu, int *ierr)
{
/*   
      tlu  is file descriptor 
*/
    int nbyte;
    *ierr = 0;
    nbyte=fcntl(*tlu,F_SETFL,FNDELAY);
    if (nbyte < 0) {
       *ierr = errno;
       printf("ERROR setting FNDELAY flag: IERR = %d \n",errno);
     }
}
/*****************************************/
/*
   Fortran call sequence:
   MT_HOLDNB(tlu,limit,ierr)    
   Uses the select system call to wait for an IO to be possible on
   descriptor tlu. If only n-b reading is on, and MT_NB_ON was called with 
   count=1, then this amounts to a wait for io completion.
   ierr = 998 or 997 if the hold times out.
   ierr = 0 if io can be done on this descriptor.
*/
void mt_holdnb_(int *lut, int *limit, int *ierr)
{
   int      timems, times;
   int      nfound, nfds;
   int      readfd, writefd, exceptfd;
   struct   timeval timeout;
      *ierr = 0;
      timems = (*limit) % 1000;
      times = (*limit)/1000;
      nfds = *lut + 1;                       /* no. of descrptrs. */ 
      readfd = 1 << (*lut);                  /* set up masks */
      writefd = 1 << (*lut);                 /* set up masks */
      exceptfd = 0;
      timeout.tv_sec=times;     
      timeout.tv_usec=timems*1000;
      nfound = select(nfds, &readfd, &writefd, &exceptfd, &timeout);
      if (nfound == 0)                       /* 0 implies timeout*/ 
           *ierr = IO_TIMEOUT;
      else if (nfound < 0)
           *ierr = errno;
      else
           *ierr = 0;
}
/*****************************************/
/*
   Fortran call sequence:
   MT_R_HOLDNB(tlu,limit,ierr)    
   Same as MT_HOLDNB but only checks on read operations. 
*/
void mt_r_holdnb_(int *lut, int *limit, int *ierr)
{
   int      timems, times;
   int      nfound, nfds;
   int      readfd, writefd, exceptfd;
   struct   timeval timeout;
      *ierr = 0;
      timems = *limit % 1000;
      times = *limit/1000;
      nfds = *lut + 1;                       /* no. of descrptrs. */ 
      readfd = 1 << (*lut);                  /* set up masks */
      writefd = 0;                           /* set up masks */
      exceptfd = 0;
      timeout.tv_sec=times;     
      timeout.tv_usec=timems*1000;
      nfound = select(nfds, &readfd, &writefd, &exceptfd, &timeout);
      if (nfound == 0)                       /* 0 implies timeout*/ 
           *ierr = IO_TIMEOUT;
      else if (nfound < 0)
           *ierr = errno;
      else
           *ierr = 0;
}
/*****************************************/
/*
   Fortran call sequence:
   MT_W_HOLDNB(tlu,limit,ierr)    
   Same as MT_HOLDNB but only checks on write operations. 
*/
void mt_w_holdnb_(int *lut, int *limit, int *ierr)
{
   int      timems, times;
   int      nfound, nfds;
   int      readfd, writefd, exceptfd;
   struct   timeval timeout;
      *ierr = 0;
      timems = *limit % 1000;
      times = *limit/1000;
      nfds = *lut + 1;                      /* no. of descrptrs. */ 
      writefd = 1 << (*lut);                /* set up masks */
      readfd = 0;                           /* set up masks */
      exceptfd = 0;
      timeout.tv_sec=times;     
      timeout.tv_usec=timems*1000;
      nfound = select(nfds, &readfd, &writefd, &exceptfd, &timeout);
      if (nfound == 0)                       /* 0 implies timeout*/ 
           *ierr = IO_TIMEOUT;
      else if (nfound < 0)
           *ierr = errno;
      else
           *ierr = 0;
}
#endif      /*  end __ultrix    */

/*************UTILITY ROUTINES*****************/
/*   FORTRAN:  SUBROUTINE CSWAB(data, nbytes) */
cswab_(char *data, int *bytes)
{
    int nbyte;
    nbyte = *bytes;
    swab(data,data,nbyte);
}
/*  casefix  */
int casefix( char *text)
{
   int i,j;
   int c = '\040';
   static char sp[] =" ";
   j = strlen(text);
   i = 0;
   if( index(text,c) == NULL){
         if( j > 80){
            text[80] = '\0';
            j = 79;
         }
         while(i<=j){
            text[i] = tolower(text[i]);
            i++;
         }
         return j;
    }
    else{
        while( (memcmp(&text[i],sp,1) != 0)){
           text[i] = tolower(text[i]);
           i++;
        }
        text[i] = '\0';
        return i;
    }
} 
/*   FORTRAN:  SUBROUTINE FCASEFIX(istring)         */
int fcasefix_(char *s)
{
    int i;
    i=casefix(s);
    return i;
}
