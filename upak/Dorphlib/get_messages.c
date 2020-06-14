/*
     These routines fetch a formatted error message given an ULTRIX
     error number. Both routines are fortran callable.

     Fortran callable routine.
     CALL GET_SYS_ERROR(IERR, ISTRING)
*/
#include <stdio.h>
void get_sys_error_(int *ierr, char *estring)
{
    extern int sys_nerr;
    extern int *sys_errlist[];
    int i, j;
    char blank = ' ';
/* A FORTRAN callable routine to fetch the system error message
   correspondind to input ierrno. The dummy argument corresponding
   to estring should be numeric in the calling program, since it is
   passed by reference.
*/
    memset(estring,blank,77);
    if (*ierr <= 0){
       return;
      }
    else if (*ierr >= sys_nerr) {
       return;
      }
    else {
       sprintf(estring,"SYSERRNO %3d :: %s", *ierr, sys_errlist[*ierr]);
       return;
      } 
}
/*
     Fortran callable routine.
     CALL GET_F77_ERROR(IERR, ISTRING)
*/
void get_f77_error_(int *ierr, char *estring)
{
    static int f77_nerr = 131;
    static char *f77_errlist[] = {
           "error in format",
           "illegal unit number",
           "formatted io not allowed",
           "unformatted io not allowed",
           "direct io not allowed",
           "sequential io not allowed",
           "can't backspace file",
           "not used",
           "can't stat file",
           "unit not connected",
           "off end of record",
           "truncation failed in endfile",
           "incompehensible list input",
           "out of free space",
           "unit not connected",
           "read nexpected character",
           "blank logical input field",
           "not used",
           "not used",
           "not used",
           "not used",
           "not used",
           "not used",
           "not used",
           "not used",
           "not used",
           "new file exists",
           "can't find old file",
           "unknown system error",
           "requires seek ability",
           "illegal argument",
           "not used" 
          };
    static char *eofmess = "eof";
    static char *rtomess = "tape read timeout";
    static char *wtomess = "tape write timeout";
    char blank = ' ';
    int jerr;
/* A FORTRAN callable routine to fetch f77 error message
   corresponding to input ierr. The dummy argument corresponding
   to estring should be numeric in the calling program, since it is
   passed by reference.
*/
    memset(estring,blank,77);
    jerr = *ierr - 100;
    if (*ierr < 100){
       return;
      }
    else if (*ierr >= f77_nerr) {
       if(*ierr == 999) 
          sprintf(estring,"SYSMSG     :: %s", eofmess);
       if(*ierr == 998) 
          sprintf(estring,"SYSMSG     :: %s", rtomess);
       if(*ierr == 997) 
          sprintf(estring,"SYSMSG     :: %s", wtomess);
       return;
      }
    else {
       sprintf(estring,"f77ERRNO %3d :: %s", *ierr, f77_errlist[jerr]);
       return;
      } 
}
