/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1992
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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/system.h
*
*    Description:  Definitions, includes and prototypes for system.c which
*                  is a customized version of indepOS.c provided with the
*                  OASYS/Green Hills cross development system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/15/92    MCSQ         
*****************************************************************************/
#ifndef  SYSTEM_H_
#define  SYSTEM_H_

#include  <time.h>

/*#include <sys/types.h> */
typedef long unsigned ino_t;
typedef short dev_t;
typedef long off_t;

/*#include <sys/stat.h> */
struct stat {
    dev_t       st_dev;
    ino_t       st_ino;
    off_t       st_size;
    long        dummy[16];
};

/*#include <sys/time.h>*/
/* UNIX Structure representing time since epoch*/
struct timeval {
    long tv_sec;                /* seconds */
    long tv_usec;               /* fractional microseconds */
};
/* Only needed to compile */
struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};

/*#include <sys/times.h>*/
struct tms {
    clock_t tms_utime;          /* user time */
    clock_t tms_stime;          /* system time */
    clock_t tms_cutime;         /* user time of all children */
    clock_t tms_cstime;         /* system time of all children */
};

/*   Prototypes for system calls          */

int    open(char *filename, int mode);
int    creat(char *filename, int prot);
int    close(int fno);
int    ioctl(int fno, int func, void *buf);
int    read(int fno, char *buf, int size);
int    write(int fno, char *buf, int size);
int    lseek(int fno, long offset, int end);
int    access(char *name, int prot);
int    unlink(char *name);
char   *sbrk(int size);
int    getpid(void);
int    _exit(int code);
int    times(struct tms *buffer);
time_t time(time_t *tptr);
int    isatty(int fno);
struct tm *gmtime(const time_t *timer);
struct tm *localtime(const time_t *timer);
int    _timezone(void);
int    rename(const char *old, const char *new);
int    truncate(char *path, int length);
int    system(const char *string);
int    raise(int sig);
void   start(void);

#endif        /* end  SYSTEM_H_   */
