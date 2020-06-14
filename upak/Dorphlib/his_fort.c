/* JRB 1/90
SOURCE FILE:  /usr/users/beene/Djblibs/his_fort.c   
LIBRARY:      /usr/users/beene/lib/jblibc1.a
A set of FORTRAN Callable routines to open, read and write his files. 
------------------------------------------------------------------
Documentation is provided at the begining of each routine. 

A closely related set of routines is provided for manipulating in-memory
histograms with memory allocated at runtime. For now they reside in a
separate library  mem_fort.

*****************IMPORTANT USAGE NOTES*****************************
These routines are designed for ALL arguments to be passed by reference.
This means that even arguments defined as char in the various routines
should be called with numeric variables as corresponding FORTRAN actual
arguments. In general when a CHARACTER variable is more convenient, I do
this by equivalencing a numeric variable to it and then using the
numeric variable as the argument. (Note that for optimization reasons
it's better to avoid floating point numeric types for this purpose). You
can also use the %REF descriptor, but portability issues are then
involved.

Use INTEGER*4 for ALL arguments and you can't go wrong.
********************************************************************

Routines included:
--------------------------------------------------------------------
SUBROUTINE HIS_OPENRO(NAME,LU)
SUBROUTINE SYS_OPENRO(NAME,LU)
--------------------------------------------------------------------
SUBROUTINE HIS_OPENRW(NAME,LU)
SUBROUTINE SYS_OPENRW(NAME,LU)
--------------------------------------------------------------------
SUBROUTINE SYS_CLOSE(LU)
--------------------------------------------------------------------
SUBROUTINE HIS_READ(LUT, BUFFER, IBYT, NB_REQ, NB_GOT, IERR)
--------------------------------------------------------------------
SUBROUTINE HIS_WRITE(LUT, BUFFER, IBYT, NB_REQ, NB_PUT, IERR)
--------------------------------------------------------------------
function ierrno()
function hisfilesize(lut)
--------------------------------------------------------------------

Documentaion is provided at the begining of each routine. Since routines
are very short I don't duplicate it here.

A closely related set of routines is provided for manipulating in memory
histograms with memory allocated at runtime. For now they reside in a
separate library  mem_fort.
*/

#if defined(__APPLE__) || defined(__CYGWIN__)
#include <sys/types.h>
#include <unistd.h>
#endif

#include <sys/stat.h>
#include <sys/file.h>
#include <limits.h>
#define WTM_EOF  999
#include <errno.h>

/* Prototypes for the functions defined here */
void his_openrw__(char *, int *);
void his_openrw_(char *, int *);
void sys_create__(char *, int *);
void sys_create_(char *, int *);
void his_openro__(char *, int *);
void his_openro_(char *, int *);
void his_read__(int *, char *, int *,
		int *, int *, int *);
void his_read_(int *, char *, int *,
	       int *, int *, int *);
void his_write__(int *, char *, int *, 
		 int *, int *, int *);
void his_write_(int *, char *, int *, 
		 int *, int *, int *);


extern int errno;
/*--------------------------------
   Fortran:  value = ierrno()
             returns the latest value of errno
*/
int ierrno_()
{
    return(errno);
} 
/*--------------------------------
   Fortran: value=hisfilesize(lu)
             returns size of the file on lu
*/
int hisfilesize_(int *lut)
{
    struct stat sb;
    int status;

    if (fstat(*lut, &sb) == 0)
       return ((int) sb.st_size);
    else
       return (-1);
}  
/*--------------------------------
   FORTRAN:   CALL HIS_OPENRW(NAME,LU)
              Succesful if LUT returns >= 0 (file descriptor).

*/
void his_openrw__(char name[], int *lut)
{
  his_openrw_(name, lut);
}
void his_openrw_(char name[], int *lut)
{
    errno = 0;
    *lut = open(name,O_RDWR,0);
}
/*--------------------------------
   FORTRAN:   CALL SYS_CREATE(NAME,LU)
              Succesful if LUT returns >= 0 (file descriptor).

*/
void sys_create__(char name[], int *lut)
{
  sys_create_(name, lut);
}
void sys_create_(char name[], int *lut)
{
    int nmask, omask;
    nmask = 022;
    omask = umask(nmask);
    nmask=0664;
    *lut = creat(name,nmask);
    if(*lut > 0){
       close(*lut);
       *lut = open(name, O_RDWR, nmask);
    }
}
/*--------------------------------
   FORTRAN:   CALL HIS_OPENRO(NAME,LU)
              Succesful if LUT returns >= 0 (file descriptor).
*/
void his_openro__(char name[], int *lut)
{
  his_openro_(name, lut);
}
void his_openro_(char name[], int *lut)
{
    errno = 0;
    *lut = open(name,O_RDONLY,0);
}
/*--------------------------------
   FORTRAN:   CALL HIS_READ(LUT, BUFFER, IBYT, NB_REQ, NB_GOT, IERR)
              Tries to read NB_REQ bytes from file described by LUT, 
              begining at byte IBYT from start (IBYT=0) of file.
              NB_GOT bytes actually read. IERR=0 if successful. Set to
              system global errno if not. BUFFER must be big enough..
              I don't check!
*/
void his_read__(int *lut, char *buffer, int *ibyt,
		int *nb_req, int *nb_got, int *ierr)
{
  his_read_(lut, buffer, ibyt,
	    nb_req, nb_got, ierr);
}
void his_read_(int *lut, char *buffer, int *ibyt,
	       int *nb_req, int *nb_got, int *ierr)
{
    int status;
    int pos;
    off_t offset;

    offset = *ibyt;
    pos = lseek(*lut, offset, L_SET); 
    status = read(*lut, buffer, *nb_req);
    if( status < 0) 
      *ierr = errno;
    else if (status == 0)
      *ierr = WTM_EOF;
    else {
      *ierr = 0;
      *nb_got = status;
    }
}
/*--------------------------------
   FORTRAN:   CALL HIS_WRITE(LUT, BUFFER, IBYT, NB_REQ, NB_PUT, IERR)
              Tries to write NB_REQ bytes to file described by LUT,
              beginning IBYT bytes from begining of file (IBYT=0).
              NB_PUT bytes actually written. IERR=0 if successful,
              Set to system global variable errno if an error detected.
*/
void his_write__(int *lut, char *buffer, int *ibyt, 
                          int *nb_req, int *nb_put, int *ierr)
{
  his_write_(lut, buffer, ibyt, 
	     nb_req, nb_put, ierr);
}
void his_write_(int *lut, char *buffer, int *ibyt, 
                          int *nb_req, int *nb_put, int *ierr)
{
    int status;
    int pos;
    off_t offset;

    offset = *ibyt;
    errno = 0;
    pos = lseek(*lut, offset, L_SET); 
/*  printf("pos %d\n",pos);   */
    status = write(*lut, buffer, *nb_req);
    if( status < 0) 
      *ierr = errno;
    else if (status == 0)
      *ierr = WTM_EOF;
    else {
      *ierr = 0;
      *nb_put = status;
    }
}
/*--------------------------------
   FORTRAN:   CALL SYS_OPENRW(NAME,LU)
              Succesful if LUT returns >= 0 (file descriptor).

*/
void sys_openrw_(char name[], int *lut)
{
    errno = 0;
    *lut = open(name,O_RDWR,0);
}
/*--------------------------------
   FORTRAN:   CALL SYS_OPENRO(NAME,LU)
              Succesful if LUT returns >= 0 (file descriptor).
*/
void sys_openro_(char name[], int *lut)
{
    errno = 0;
    *lut = open(name,O_RDONLY,0);
}
/*--------------------------------
   FORTRAN:   CALL SYS_CLOSE(LU)
              Succesful if LUT returns >= 0 (file descriptor).
*/
void sys_close__(int *lut)
{
    errno = 0;
    close(*lut);
 }

