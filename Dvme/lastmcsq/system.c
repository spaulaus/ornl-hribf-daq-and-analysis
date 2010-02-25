/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1992-1996
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/system.c
*
*    Description:  This is a version of indepOS.c provided by OASYS/
*                  Green Hills.  It has been customized for use with
*                  a Force Computers CPU40 with VMEPROM.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/31/92    MCSQ         
*
*   11/ 2/92    MCSQ       Heretofore, the terminal read routine used a
*                          single character call to VMEPROM - XGCP.
*                          For C calls it appears better to use a line
*                          input call to VMEPROM - XGLB - since the
*                          VMEPROM will handle things like rubout, backspace
*                          etc.
*
*    2/18/94    MCSQ       Changed _NFILE to FOPEN_MAX for new OASYS stuff.
*
*    4/17/96    MCSQ       Oasys calls a routine __gh_timezone to get the
*                          local time zone.  This routine was _timezone.
*
*    4/17/98    MCSQ       Fix routine gmtime().  Part of my Y2K patches.
*****************************************************************************/
/*
 C Runtime library
 
Copyright 1983,1984,1985,1986,1987,1988,1989,1990 Green Hills Software Inc.

 *  This program is the property of Green Hills Software, Inc,
 *  its contents are proprietary information and no part of it
 *  is to be disclosed to anyone except employees of Green Hills
 *  Software, Inc., or as agreed in writing signed by the President
 *  of Green Hills Software, Inc.
*/
/*
 *
 * If your system is a real UNIX system then nothing needs to be done to
 * this file.
 *
 * This module contains the lowest level I/O functions in the Green Hills
 * C library.  These functions provide a very simple UNIX-like system call
 * interface.
 *
 * Since every system has different memory addresses and I/O devices you
 * must customize these low level functions to work with your system in
 * order to be able to use the Green Hills C library.
 *
 * Please see the Green Hills C Library Documentation for
 * directions as to how to customize this module for your system.
 *
*/

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "system.h"
#include "syram.h"
#include "tcb.h"
#include "vme_sys.h"
#include "vmeprom.h"
#include "lan.h"

extern int errno;

static char isopen[FOPEN_MAX];  /* The corresponding channel is open */
static char isread[FOPEN_MAX];  /* The corresponding channel is open for read */
static char iswrite[FOPEN_MAX]; /* The corresponding channel is open for write*/
static int chan[FOPEN_MAX];     /* An operating system channel number */

static struct LAN_CALL ln_call;

#ifndef MEMSIZE
#define MEMSIZE 2048
#endif
static char _MEMORY[MEMSIZE];
static char *sbrk_ptr;
static char *sbrk_end;

 long  ticks;             /* Elasped clock time  */

Init_IO()
/*
 *  Init_IO is called from START() to initialize the I/O system.
 *
 *  Open file numbers 0, 1, and 2 like the unix shell.
 *
 *  File number 0 is the standard input file.  It is usually the controlling
 *  terminal for the program or a sequential character input file.
 *
 *  File number 1 is the standard output file.  It is usually the controlling
 *  terminal for the program or a sequential character output file.
 *
 *  File number 2 is the standard error file.  It is usually the controlling
 *  terminal for the program.  Error messages are usually written on this
 *  file rather than standard output so that if the standard output is a file,
 *  the error message can still be seen by the person running the program.
 *  In batch jobs, file number 2 is usually a sequential character output file
 *
 *  On non-unix systems "/dev/tty" should be changed to whatever is appropriate
 *  ("SYS$INPUT": on vms, "TTY": on tops-10, etc.).
 *
 *  If these files are already opened by the operating system, or by the unix
 *  shell, delete the calls to open.
 */
{
    open("/dev/tty", 0);
    open("/dev/tty", 1);
    open("/dev/tty", 1);
}

/*
 *  Return the number of a free file number or -1
 */
static find_free()
{
    int fno;

    for (fno = 0; fno < FOPEN_MAX; fno++)
	if (!isopen[fno]) {
	    isopen[fno] = 1;
	    return(fno);
	}
    errno = EMFILE;
    return(-1);
}

open(filename, mode)
char *filename;
/******************************************************************************/
/*  Open the file named filename with the indicated mode.                     */
/*  filename is a null terminated string and mode is one of the following     */
/*                                                                            */
/*      0       => open for reading                                           */
/*      1       => open for writing                                           */
/*      2       => open for reading & writing                                 */
/*                                                                            */
/*  If the open is successful return a small integer file number.             */
/*  On failure return -1, and set errno in accordance with the error.         */
/*                                                                            */
/******************************************************************************/
{
    int fno;

    if ((fno = find_free()) == -1)
	return(-1);
/*
 *  For a complete implementation of the Low Level System Interface,
 *  insert code here that will associate a channel from the operating system
 *  with the file named in filename, you will probably have to deal with mode
 *  too.
 *
 *  chan[fno] = localopen(filename, mode);
 */
    if (!strncmp(filename,"ln",2))
     {
       ln_call.flag = 0;
       ln_call.func = E_OPEN;
       while(!send_ptr_(LAN_MSG_SLOT,(char *)&ln_call)) wait_evts_(-LAN_MSG,0);
       wait_phys_(0x87,(char *)&ln_call,NULL);
       if (ln_call.status < 0) 
	 {
	   errno = -ln_call.status;
	   return (-1);
	 }
       chan[fno] = ln_call.status;
     }
    isread[fno] = (mode&3) != 1;
    iswrite[fno] = (mode&3) != 0;
    return(fno);
}

creat(filename, prot)
char *filename;
/******************************************************************************/
/*  Create and open for writing a file named filename with protection         */
/*  as specified by prot.  filename is a null terminated string.              */
/*  prot is expressed in unix format, it is the logical or of:                */
/*                                                                            */
/*              0400: owner read                                              */
/*              0200: owner write                                             */
/*              0100: owner execute/search directory                          */
/*              0040: group read                                              */
/*              0020: group write                                             */
/*              0010: group execute/search directory                          */
/*              0004: other (world) read                                      */
/*              0002: other (world) write                                     */
/*              0001: other (world) execute/search directory                  */
/*                                                                            */
/*  For things like terminals that cannot be created it should just open the  */
/*  device.  If successful, return a small integer file number.  On failure   */
/*  return -1 and set errno appropriately.                                    */
/******************************************************************************/
{
    int fno,openmode;

    if ((fno = find_free()) == -1)
	return(-1);
/*
 *  For a complete implementation of the Low Level System Interface,
 *  insert code here that will create a file named filename with protection
 *  prot, and associate a channel from the operating system to it.
 *
 *  chan[fno] = localcreat(filename,prot);
 */

    if (!strncmp(filename,"ln",2)) return(-1);
    isread[fno] = 0;
    iswrite[fno] = 1;
    return(fno);
}

close(fno)
/******************************************************************************/
/*  Close the file associated with the file number fno (returned by open()    */
/*  or creat()).  Return 0 if all goes well.  Return -1 if something went     */
/*  wrong and set errno appropriately.                                        */
/******************************************************************************/
{
    if (!isopen[fno]) {
	errno = EBADF;
	return(-1);
    }
    isopen[fno] = 0;
    isread[fno] = 0;
    iswrite[fno] = 0;
/*
 *  For a complete implementation of the Low Level System Interface,
 *  insert code here to close the channel from the operating system.
 *
 *  if (localclose(chan[fno]) successful)
 *      return(0);
 *  else {
 *      errno = reason for error
  *     return(-1);
 *  }
 */
    if (chan[fno])
     {
       ln_call.flag = 0;
       ln_call.func = E_CLOSE;
       ln_call.eno = chan[fno];
       while(!send_ptr_(LAN_MSG_SLOT,(char *)&ln_call)) wait_evts_(-LAN_MSG,0);
       wait_phys_(0x87,(char *)&ln_call,NULL);
       if (ln_call.status < 0) 
	 {
	   errno = -ln_call.status;
	   return (-1);
	 }
       chan[fno] = 0;
     }
    return(0);
}
/****************************************************************************
*
*  Only ethernet ioctl calls are supported.
*
****************************************************************************/
ioctl(int fno, int func, void *buf)
{
    if (!isopen[fno]) {
	errno = EBADF;
	return(-1);
    }
    if (chan[fno])
     {
       ln_call.flag = 0;
       ln_call.func = func;
       ln_call.len = 0;
       ln_call.data = buf;
       ln_call.eno = chan[fno];
       while(!send_ptr_(LAN_MSG_SLOT,(char *)&ln_call)) wait_evts_(-LAN_MSG,0);
       wait_phys_(0x87,(char *)&ln_call,NULL);
       if (ln_call.status < 0) 
	 {
	   errno = -ln_call.status;
	   return (-1);
	 }
     }
    else  
     {
       errno = EINVAL;
       return(-1);
     }
}

read(fno, buf, size)
char *buf;
/******************************************************************************/
/*  Read at most size bytes into buf from the file connected to fno (where    */
/*  fno is one of the file numbers returned by open() or creat()).            */
/*  Return the number of bytes read.  Return 0 at end of file.  Return -1 on  */
/*  error and set errno appropriately.                                        */
/*                                                                            */
/*  The testsuite assumes that (as in unix) on line buffered terminals input  */
/*  carriage returns ('\r') are converted to line feeds ('\n').               */
/******************************************************************************/
{
    int cnt = 0, ch, iseof;
    static int   in_count;
    static char  *lbuf = NULL;

    if (!isread[fno]) {
	errno = EBADF;
	return(-1);
    }
    if (chan[fno])
     {
       ln_call.flag = 0;
       ln_call.func = E_READ;
       ln_call.len = size;
       ln_call.data = buf;
       ln_call.eno = chan[fno];
       while(!send_ptr_(LAN_MSG_SLOT,(char *)&ln_call)) wait_evts_(-LAN_MSG,0);
       wait_phys_(0x87,(char *)&ln_call,NULL);
       if (ln_call.status < 0) 
	 {
	   errno = -ln_call.status;
	   return (-1);
	 }
       cnt = ln_call.status;
     }
    else
     {
       if (lbuf == NULL)
         {
           in_count = get_line_(&lbuf);
           *(lbuf+in_count) = '\n';
           ++in_count;
           OUTPUT('\r');
           OUTPUT('\n');
         }
       while (size-- > 0)
	{
          *buf++ = *lbuf++;
          ++cnt;
          --in_count;
          if (in_count == 0)
            {
              lbuf = NULL;
              break;
            }
	}
     }
    return(cnt);
}

write(fno, buf, size)
char *buf;
/******************************************************************************/
/*  Write at most size bytes into the file connected to fno (where fno is     */
/*  one of the file numbers returned by open() or creat()) into buf.          */
/*  Return the number of bytes written, or -1 to indicate an error and        */
/*  set errno appropriately.  On line buffered terminal output (as in unix)   */
/*  line feeds ('\n') are converted to a carriage return ('\r') and a         */
/*  line feed ('\n').                                                         */
/******************************************************************************/
{

    int cnt = size;
    char ch;

    if (!iswrite[fno]) {
	errno = EBADF;
	return(-1);
    }
    if (chan[fno])
     {
       ln_call.flag = 0;
       ln_call.func = E_WRITE;
       ln_call.len = size;
       ln_call.data = buf;
       ln_call.eno = chan[fno];
       while(!send_ptr_(LAN_MSG_SLOT,(char *)&ln_call)) wait_evts_(-LAN_MSG,0);
       wait_phys_(0x87,(char *)&ln_call,NULL);
       if (ln_call.status < 0) 
	 {
	   errno = -ln_call.status;
	   return (-1);
	 }
       cnt = ln_call.status;
     }
    else
     {
       while (size-- > 0) 
	{
	  ch = *buf++;
	  if (ch == '\n') OUTPUT('\r');
	  OUTPUT(ch);
	}
     }
    return(cnt);
}

lseek(fno, offset, end)
long offset;
/******************************************************************************/
/*  Seek to a new position within the file connected to the file number fno   */
/*  (returned by open() or creat()).  If end is 0, seek to offset bytes from  */
/*  the beginning of the file.  If end is 1, seek to offset bytes from the    */
/*  current position in the file.  If end is 2 seek to offset bytes from      */
/*  the end of the file.  lseek does no I/O.  The next I/O operation to       */
/*  file number fno will begin at the new position in the file.               */
/*                                                                            */
/*  Return the offset from the beginning of the file after the seek takes     */
/*  place.  If an error occurs return -1 and set errno appropriately.         */
/*  If an error occurs the file position is not changed.                      */
/******************************************************************************/
{
#define LOCALBUF 128
    char buf[LOCALBUF];
    int count;
    int i; 

    if (!isopen[fno]) {
	errno = EBADF;
	return(-1);
    }
    if (end < 0 || end > 2) {
	errno = EINVAL;
	return(-1);
    }
    if (isatty(fno)) {
	if (end == 0)
	    return(offset);
	else
	    return(0);
    }
    else if (end == 1 && offset > 0 && isread[fno]) {
/* This case is needed for the Fortran library */
	while (offset > 0) {
	    count = LOCALBUF;
	    if (count > offset)
		count = offset;
	    if ((i = read(fno, buf, count)) <= 0) {
		errno = ESPIPE;
		return(-1);
	    }
	    offset -= i;
	}
    } else {
	errno = ESPIPE;
	return(-1);
    }
}

access(name, prot)
char *name;
/******************************************************************************/
/*  The Green Hills C Library does not use the access function, it is only    */
/*  needed if the Green Hills Fortran Library is being used.                  */
/*                                                                            */
/*  name is the name of a file, prot is the Logical Or of the following flags */
/*              4: readable                                                   */
/*              2: writable                                                   */
/*              1: executable or directory searchable (Unused)                */
/*  If prot is zero then test only for file existence.                        */
/*  Return 0 if all of the specified operations are possible on the file.     */
/*  Return -1 if any of the specified operations are not possible on the file */
/*  and set errno appropriately.                                              */
/******************************************************************************/
{
/*
 *  If no other implementation is provided, return -1 (there are no files)
 */
    return(-1);
}

unlink(name)
char *name;
/******************************************************************************/
/*                                                                            */
/*  name is the name of a file.  The named file is deleted.                   */
/*  Return 0 if the named file is deleted.  Return -1 if there is no such     */
/*  file or if the file cannot be deleted, and set errno appropriately        */
/*                                                                            */
/******************************************************************************/
{
/*
 *  If no other implementation is provided, return -1 (there are no files)
 */
    return(-1);
}

fstat(fno, statptr)
struct stat *statptr;
/******************************************************************************/
/*  The Green Hills C Library does not use the fstat function, it is only     */
/*  needed if the Green Hills Fortran Library is being used.                  */
/*                                                                            */
/*  fno is a file number returned by open() or creat().  In the two fields    */
/*  st_dev and st_ino place values which uniquely identify the file or device */
/*  opened on file number fno.  In unix these are the device number and the   */
/*  inode (file) number on the device.                                        */
/*  The field st_size is set to the size of the file in bytes, -1 if the size */
/*  is unknown.                                                               */
/*                                                                            */
/*  Return 0 if the file status is correctly returned.  Return -1 if no file  */
/*  is opened on file number fno, and set errno appropriately.                */
/******************************************************************************/
{
/*
 *  If no other implementation is provided, return device and inode number 0.
 *  There is only one terminal device.
 */
    statptr->st_dev = 0;
    statptr->st_ino = 0;
    statptr->st_size = -1;              /* Don't know size of file */
    return(0);
}

stat(name, statptr)
char *name;
struct stat *statptr;
/******************************************************************************/
/*  The Green Hills C Library does not use the stat function, it is only      */
/*  needed if the Green Hills Fortran Library is being used.                  */
/*                                                                            */
/*  name is the name of a file.  In the two fields st_dev and st_ino place    */
/*  values which uniquely identify the named file or device.  In unix, these  */
/*  are the device number and the inode (file) number on the device.          */
/*  The field st_size is set to the size of the file in bytes, -1 if the size */
/*  is unknown.                                                               */
/*                                                                            */
/*  Return 0 if the file status is correctly returned.  Return -1 if no file  */
/*  with that name exists, and set errno appropriately.                       */
/******************************************************************************/
{
/*
 *  If no other implementation provided, return device and inode number 0.
 *  There is only one terminal device.
 */
    statptr->st_dev = 0;
    statptr->st_ino = 0;
    statptr->st_size = -1;              /* Don't know size of file */
    return(0);
}

char *sbrk(size)
/******************************************************************************/
/*  Return a pointer to at least size bytes of contiguous read/write memory.  */
/*  The memory returned by sbrk on different calls need not be contiguous     */
/*  with each other (although they should be for compatability with unix).    */
/*  Return -1 on an error and set errno = ENOMEM.                             */
/******************************************************************************/
{
/*
 *  If no other implementation is provided, allocate memory from a static array
 */

    sbrk_ptr += size;
    if (sbrk_ptr > sbrk_end) {
/* [JY] Wed Aug  8 10:03:32 PDT 1990                    cust:  Siemens
    If a request for memory exceeds the heap size, we should not advance the
    pointer past the heap.  Rather, restore the pointer so that subsequent
    smaller requests can be satisfied.
*/
/* new code: */
	sbrk_ptr -= size;
/* end of new code */
	errno = ENOMEM;
	return((char *)-1);
    }
    return(sbrk_ptr - size);
}

getpid()
/******************************************************************************/
/*  getpid returns the current process number.  getpid is used to create      */
/*  filenames which will not conflict with filenames created by another       */
/*  process at the same time.                                                 */
/******************************************************************************/
{
/*
 * If no other implementation is provided, return a random number
 */
    return(task_status_(-1));
}

_exit(code)
/******************************************************************************/
/*  Exit from the program with a status code specified by code.               */
/*  DO NOT RETURN!                                                            */
/******************************************************************************/
{
    int  i = 0;
    int  task;
/*
 *  For a complete implementation of the Low Level System Interface,
 *  insert code here to return to the operating system, monitor, or debugger
*/


/*
 *  Return to the emulator.
 */

    for(;i < FOPEN_MAX; i++) close(i);
    task = (char)getpid();
    while(!send_ptr_(EXIT_MSG_SLOT,NULL)) wait_evts_(-EXIT_MSG,0);
    task_kill_(-task);
}

static mons[]={31,28,31,30,31,30,31,31,30,31,30,31};

times(buffer)
struct tms *buffer;
/******************************************************************************/
/*                                                                            */
/*  Place the process execution time in user mode in buffer->tms_utime        */
/*  Place the process execution time in system mode in buffer->tms_stime      */
/*  All times are measured in clock ticks = 1/CLOCKS_PER_SEC second           */
/*                                                                            */
/******************************************************************************/
{
/*
 *  If no other implementation provided, return time = 0
 */
    struct SYRAM *syram = (struct SYRAM *)SYSRAM;

    buffer->tms_utime = syram->_tics - ticks;
    buffer->tms_stime = 0;
}

time_t time(tptr)
time_t *tptr;
/******************************************************************************/
/*                                                                            */
/*  Return the current time relative to the Epoch (truncated to the most      */
/*  recent second), Midnight 00:00 January 1, 1970 Greenwich Mean Time.       */
/*  If tptr is non-null also store the time into *tptr.                       */
/*                                                                            */
/*  If all you have is local time, not Greenwich Mean Time, then return the   */
/*  local time and return 0 from __gh_timezone() below.                           */
/*                                                                            */
/******************************************************************************/
{
/*
 *  If no other implementation provided, return time = -1
 */
/*    if (tptr!=NULL)
	*tptr= -1;
    return(-1);   */

    struct SYRAM *syram = (struct SYRAM *)SYSRAM;
    int  cy,cm,cd;
    time_t tx;

    cy = syram->_syrs[0];
    cm = syram->_smon - 1;
    cd = syram->_sday - 1;
    tx = (cy-70)*365*24*60*60;          /* seconds since Jan 1, 1970       */
    tx += ((cy-69)/4)*24*60*60;         /* till Jan 1 of this year         */
    if(cy % 4 == 0 && cm > 1) cd++;
    while (cm > 0) cm--, cd += mons[cm];
    tx += cd*24*60*60;
    tx += syram->_shrs*60*60;
    if (syram->_syrs[1] != 0) tx -= 60*60;
    tx += syram->_smin*60;
    tx += syram->_ssec[0];
    tx += __gh_timezone() * 60;
    *tptr = tx;
    return (tx);
}

/******************************************************************************/
/* things in this area represent functions from chapter 3 of the unix manual  */
/*  that is: standard, low level, but not quite system calls.  These guys     */
/*  get generated even if we have unix style system calls.                    */
/******************************************************************************/
int isatty(fno)
int fno;
/******************************************************************************/
/*  There are environments in which we have all the standard unix system      */
/*  calls, but we don't have isatty.  This little hook is provided for such.  */
/*  This really should call ioctl().  But that is grossly machine dependant.  */
/*  Instead I try an lseek which will set errno as the ioctl would.  It fails */
/*  to distinguish between pipes and terminals, but it's better than nothing. */
/******************************************************************************/
{
    errno=0;
    lseek(fno,0l,1);
    if ( errno==ENOTTY )
	return(0);
    return(1);
}

struct tm *gmtime(const time_t *timer)
/******************************************************************************/
/*  gmtime returns a structure containing the current Greenwich Mean Time     */
/*  broken down into tm_year (current year - 1900), tm_mon (current month     */
/*  January=0), tm_mday (current day of month), tm_hour (current hour 24 hour */
/*  time), tm_min (current minute), and tm_sec (current second).              */
/*                                                                            */
/*  seconds is a pointer to a long containing the number of seconds since the */
/*  Epoch (as set by time()).                                                 */
/*                                                                            */
/*  Return 0 if no time of day can be returned                                */
/******************************************************************************/
{
/*
    If no other implementation provided, assume Epoch is 00:00:00 January 1,1970
    as is true in UNIX.
*/
    register time_t time = *timer;
    static struct tm temp;
    register int islpyr, lpcnt;
    register int y, ystart, i, t, yend;

    if (time < 0) return(NULL);
    temp.tm_isdst=0;
    i=time;
    i = (i/(24*60*60))%7-3;
    while (i<0) i += 7;
    temp.tm_wday = i;

/*
*    Approximate the year.  Note that we may be off by + or - one year.
*/
    y = time/(365*24*60*60+6*60*60);        /* a year is about 365.25 days */
    y += 370;

    islpyr = y%4==0 && (y%100!=0 || y%400==0);
    lpcnt = y/4;
    if (islpyr) lpcnt--;
    lpcnt -= y/100;
    lpcnt += y/400;

    lpcnt -= 89;            /* number of leap years in 370 years */
    ystart = (y-370)*(365*24*60*60) + lpcnt*(24*60*60);
    yend = ystart + 365*24*60*60 - 1;
    if (islpyr) yend += 24*60*60;
/*
*    See if our year is correct.  If not, add or subtract one year.
*/
    if (time < ystart || time > yend)
      {
        if (time < ystart) y--;
        else  y++;
        islpyr = y%4==0 && (y%100!=0 || y%400==0);
        lpcnt = y/4;
        if (islpyr) lpcnt--;
        lpcnt -= y/100;
        lpcnt += y/400;
        lpcnt -= 89;
        ystart = (y-370)*(365*24*60*60) + lpcnt*(24*60*60);
      }

    time -= ystart;
    temp.tm_hour = (time/(60*60))%24;
    temp.tm_min = (time/60)%60;
    temp.tm_sec = time%60;
    time /= 24*60*60;
    temp.tm_yday= time;
    ++ time;

    for ( i=0; i<11; ++i ) {
	t = mons[i];
	if ( i==1 && islpyr ) ++t;
	if ( time<=t ) break;
	time -= t;
    }

    temp.tm_year= y-300;
    temp.tm_mon= i;
    temp.tm_mday = time;        /* we incremented the mday above */
    return( &temp );
}

struct tm *localtime(const time_t *timer)
/******************************************************************************/
/*  localtime returns a structure containing the current local time broken    */
/*  down into tm_year (current year - 1900), tm_mon (current month January=0) */
/*  tm_mday (current day of month), tm_hour (current hour 24 hour time),      */
/*  tm_min (current minute), and tm_sec (current second).                     */
/*                                                                            */
/*  seconds is a pointer to a long containing the number of seconds since the */
/*  Epoch (as set by time()).                                                 */
/*                                                                            */
/*  Return 0 if no time of day can be returned.                               */
/******************************************************************************/
{
/*
    If no other implementation provided, assume Epoch is 00:00:00 January 1,1970
    as is true in UNIX.

    There should be code in here to calculate daylight savings time in different
    ways than the one given here.  Which is true for USA as of 1987.

    The rules I follow are:
	before 1987
	    at 2am of the last sunday of april dst starts
	    at 2am of the last sunday of october dst ends
	1987 and after
	    at 2am of the first sunday of april dst starts
	    at 2am of the last sunday of october dst ends
	I shall not concern myself with the vargaries of dst in the 70s
*/
    time_t time = *timer - 60 * __gh_timezone();
    register struct tm *temp;
    register int isdst=0, sn_mday;

    if (*timer == (time_t)-1)
	return(NULL);
    if ((temp = gmtime(&time))==NULL)
	return(NULL);
    if (temp->tm_mon<=2 || temp->tm_mon>=10)/*March or before or Nov or after */
	isdst=0;
    else if ( temp->tm_mon!=3 && temp->tm_mon!=9 )  /* between may and Sept */
	isdst=1;
    else if ( temp->tm_mon==9 ) {                   /* october */
	sn_mday=temp->tm_mday-temp->tm_wday;
	while ( sn_mday+7 <=31 ) sn_mday +=7;
	if ( temp->tm_mday<sn_mday )
	    isdst=1;
	else if ( temp->tm_mday==sn_mday && temp->tm_hour<1 )
	    isdst=1;
    } else if ( temp->tm_year<87 ) {                /* april, before 87 */
	sn_mday=temp->tm_mday-temp->tm_wday;
	while ( sn_mday+7 <=30 ) sn_mday +=7;
	if ( temp->tm_mday>sn_mday )
	    isdst=1;
	else if ( temp->tm_mday==sn_mday && temp->tm_hour>=2 )
	    isdst=1;
    } else {                                        /* april, after 87 */
	sn_mday=temp->tm_mday-temp->tm_wday+7;
	while ( sn_mday-7 > 0 ) sn_mday -=7;
	if ( temp->tm_mday>sn_mday )
	    isdst=1;
	else if ( temp->tm_mday==sn_mday && temp->tm_hour>=2 )
	    isdst=1;
    }

    temp->tm_isdst = isdst;
    if ( isdst ) {
	if ( ++(temp->tm_hour)==24 ) {
	    temp->tm_hour=0;
	    ++(temp->tm_yday);          /* can't overflow dst not on Dec. 31 */
	    if ( ++(temp->tm_wday)==7 ) temp->tm_wday=0;
	    if ( ++(temp->tm_mday)>mons[temp->tm_mon] ) {
		temp->tm_mday=1;
		++(temp->tm_mon);       /* can't overflow dst not on Dec. 31 */
	    }
	}
    }
    return( temp );
}

int __gh_timezone()
/******************************************************************************/
/*  Return the number of minutes west of Greenwich Mean Time of the current   */
/*  time zone.  If the time() functions return the local time rather than     */
/*  Greenwich Mean Time then return 0 from __gh_timezone().                       */
/******************************************************************************/
{
  struct SYRAM *syram = (struct SYRAM *)SYSRAM;

  return (syram->_ssec[1]*60);
}

int rename(const char *old, const char *new)
/******************************************************************************/
/*  Rename the file named "old" to the name "new".                            */
/*  Return 0 if the operation succeeds.  Return -1 if there is no such        */
/*  file or if the file cannot be renamed, and set errno appropriately        */
/******************************************************************************/
{
/*
 *  If no other implementation is provided, return -1 (there are no files)
 */
    return(-1);
}

/* GWW: Thu May 17 16:27:23 EDT 1990: added to avoid dependancy on libansi */
/* Helper routine for truncate */
static char *tmpnam(char *s) {
    static char hexmap[16]="0123456789abcef";
    static int cnt;
    register int chan;
    static char space[L_tmpnam];
    int oldstrlen;
    register int pid;
    register char *pt1, *pt2;

    if ( s==NULL )
	s=space;
#define TMPNAMPREFIX "TMP"
    for( pt1=s, pt2=TMPNAMPREFIX; *pt2 ; *(pt1++)= *(pt2++) );
    pid = getpid();
    *pt1 = hexmap[pid>>12];
    pt1[1] = hexmap[(pid>>8)&0xf];
    pt1[2] = hexmap[(pid>>4)&0xf];
    pt1[3] = hexmap[pid&0xf];
    pt1 += 4;
    do {
	++cnt;
	pt1[0] = hexmap[cnt&0xf];
	pt1[1]='\0'; pt1[2]='\0'; pt1[3]='\0'; pt1[4]='\0'; pt1[5]='\0';
	pt1[6]='\0'; pt1[7]='\0'; pt1[8]='\0';
	if ( cnt&0xfffffff0 )
	    pt1[1] = hexmap[(cnt>>4)&0xf];
	if ( cnt&0xffffff00 )
	    pt1[2] = hexmap[(cnt>>8)&0xf];
	if ( cnt&0xfffff000 )
	    pt1[3] = hexmap[(cnt>>12)&0xf];
	if ( cnt&0xffff0000 )
	    pt1[4] = hexmap[(cnt>>16)&0xf];
	if ( cnt&0xfff00000 )
	    pt1[5] = hexmap[(cnt>>20)&0xf];
	if ( cnt&0xff000000 )
	    pt1[6] = hexmap[(cnt>>24)&0xf];
	if ( cnt&0xf0000000 )
	    pt1[7] = hexmap[(cnt>>28)&0xf];
	chan = open(s,0);
	if ( chan != -1 )
	    close(chan);
    } while ( chan!=-1 );
return(s);
}

truncate(path, length)
char *path;
/******************************************************************************/
/*  The Green Hills C Library does not use the truncate function, it is only  */
/*  needed if the Green Hills Fortran Library is being used.                  */
/*                                                                            */
/*  If the file named "path" is longer than "length" bytes it is truncated    */
/*  to "length" bytes.                                                        */
/*                                                                            */
/*  Return 0 if the named file is truncated or was previously less than       */
/*  length bytes.  Return -1 if the file cannot be truncated, and set         */
/*  errno appropriately.                                                      */
/******************************************************************************/
{
/*
   The following implementation copies the first "length" bytes of the file
   named "path" to a temporary file, then it creates a new file with
   the name "path" and copies the temporary file back into it.  At the end
   the temporary file is deleted.  The prefered implementation is to shorten
   the file without copying, but many operating systems lack such an operation.
*/
    char buf[BUFSIZ];
    int inf, of;
    int count = 0;
    char *tmpname = tmpnam(0);
    unsigned int pid = getpid();

/* NEW handle the two trivial cases efficiently: 
	length is 0 and length is greater than or equal to actual size of file
*/
    struct stat stat1;

    if (length == 0) {
	if ((of = creat(path, 0777)) == -1)
	    return -1;
	close(of);
	return 0;
    }

    if (stat(path, &stat1) < 0)
	return -1;

    if (stat1.st_size != -1 && length >= stat1.st_size)
	return 0;
/* END OF NEW */

    if ((inf = open(path, 0)) == -1 || (of = creat(tmpname, 0777)) == -1) {
	if (inf != -1)
	    close(inf);
	return(-1);
    }
    while (length > 0 && (count = read(inf, buf, BUFSIZ)) > 0) {
	if (count > length)
	    count = length;
	if (count > write(of, buf, count)) {
	    count = -1;
	    break;
	}
	length -= count;
    }
    close(inf);
    close(of);

    if (length == 0 && count >= 0) {
	if ((inf = open(tmpname, 0)) == -1 || (of = creat(path, 0777)) == -1) {
	    if (inf != -1)
		close(inf);
	    return(-1);
	}
	while ((count = read(inf, buf, BUFSIZ)) > 0)
	    if (count > write(of, buf, count)) {
		count = -1;
		break;
	    }
	close(inf);
	close(of);
    }
    unlink(tmpname);
    return(count);
}
/******************************************************************************/
/*  Execute the command pointed to by "string" as if it had been typed in at  */
/*  the terminal.                                                             */
/*  Return the status returned by the process.  A status of 0 is returned if  */
/*  the operation succeeds.  The operation is system implementation dependent */
/*  It may have no effect.                                                    */
/******************************************************************************/
int system(const char *string)
{
    /* If no other implementation provided, minimal compliance */
    return(0);                      /* no system access */
}

#include <signal.h>
typedef void (*sigftype)(int);
static sigftype sigfuncs[_SIGMAX] = {
SIG_DFL,                /*  SIGHUP      */
SIG_DFL,                /*  SIGINT      */
SIG_DFL,                /*  SIGQUIT     */
SIG_DFL,                /*  SIGILL      */
SIG_DFL,                /*  SIGTRAP     */
SIG_DFL,                /*  SIGABRT     */
SIG_DFL,                /*  SIGEMT      */
SIG_DFL,                /*  SIGFPE      */
SIG_DFL,                /*  SIGKILL     */
SIG_DFL,                /*  SIGBUS      */
SIG_DFL,                /*  SIGSEGV     */
SIG_DFL,                /*  SIGSYS      */
SIG_DFL,                /*  SIGPIPE     */
SIG_DFL,                /*  SIGALRM     */
SIG_DFL,                /*  SIGTERM     */
SIG_DFL,                /*  SIGURG      */
SIG_IGN,                /*  SIGSTOP     */
SIG_IGN,                /*  SIGTSTP     */
SIG_IGN,                /*  SIGCONT     */
SIG_IGN,                /*  SIGCHLD     */
SIG_IGN,                /*  SIGTTIN     */
SIG_IGN,                /*  SIGTTOU     */
SIG_IGN,                /*  SIGIO       */
SIG_DFL,                /*  SIGXCPU     */
SIG_DFL,                /*  SIGXFSZ     */
SIG_DFL,                /*  SIGVTALRM   */
SIG_IGN,                /*  SIGPROF     */
SIG_IGN,                /*  SIGWINCH    */
SIG_IGN,                /*  SIGUSR1     */
SIG_IGN                 /*  SIGUSR2     */
};

/******************************************************************************/
/*  Raise the signal, "sig" for the current process according to ANSI C.      */
/*  If sig==SIGKILL then terminate the process with status = 1.               */
/*  Otherwise, execute the function set for the signal, "sig", by the most    */
/*  recent call to signal(), see below.                                       */
/*      If the function specified is SIG_IGN then do nothing.                 */
/*      If the function specified is SIG_DFL then terminate the process with  */
/*              status=1.                                                     */
/*      Otherwise set the function SIG_DFL for "sig", then execute the        */
/*              function with the signal, "sig" passed as an argument.        */
/*  Return -1 if there is no such signal.  Return 0 if the operation succeeds.*/
/******************************************************************************/
int raise(int sig)
{
    /* this is a very basic signal mechanism, it only implements the minimal */
    /* requirements for ANSI C */
    void (*temp)(int);

    if (sig<=0 || sig>_SIGMAX)
	return(-1);
    temp = sigfuncs[sig-1];
    if (sig==SIGKILL || temp==SIG_DFL)
	_exit(EXIT_FAILURE);
    if (temp!=SIG_IGN) {
	sigfuncs[sig-1] = SIG_DFL;
	(*temp)(sig);
    }
    return(0);
}

/******************************************************************************/
/*  Set the function to execute when the signal, "sig" is raised, see raise() */
/*      above.                                                                */
/*  If the function specified is SIG_IGN then raise() will do nothing.        */
/*  If the function specified is SIG_DFL then raise() will terminate the      */
/*      process with status=1.                                                */
/*  Otherwise set the function for signal "sig" to be "func".                 */
/*  Return -1 if there is no such signal.  Return the previous function       */
/*      specified for the signal "sig" if the operation succeeds.             */
/******************************************************************************/
void (*signal(int sig, void (*func)(int)))(int)
{
    /* this is a very basic signal mechanism, it only implements the minimal */
    /* requirements for ANSI C */
    void (*temp)(int);

    if (sig<=0 || sig>_SIGMAX)
	return(SIG_ERR);
    temp=sigfuncs[sig-1];
    sigfuncs[sig-1]=func;
    return(temp);
}

static err(msg)
char *msg;
{
    char *pt = msg;

    while (*pt++ != '\0');
    write(2, msg, pt - msg - 1);
}

/*
 *  Return a null argument list.  Set argc to 1, return a pointer to
 *  an argument list with on empty string argument, and return an
 *  environment with no strings in it.
 */
static char *env[] = {NULL};
char **environ = env;

char **args(argc)
int *argc;
{
    static char *def_argv[]={"", NULL};

    *argc = 1;
    return(def_argv);
}

void heap_init(void)
{
  char *prog,*eom,*last,*sysram;
  struct TCB *tcb_ptr;

  mem_lim_(&prog,&eom,&last,&sysram,(char **)&tcb_ptr);
  if (tcb_ptr->_brk != NULL)
    {
      sbrk_ptr = tcb_ptr->_brk;
      sbrk_end = sbrk_ptr + tcb_ptr->_brk_size;
    }
  else
    {
      sbrk_ptr = _MEMORY;
      sbrk_end = _MEMORY+MEMSIZE;
    }
}
void start(void)
{
  Init_IO();
  heap_init();
  ticks = ((struct SYRAM *)SYSRAM)->_tics;
}
