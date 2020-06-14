/* Dummy routines for use on Apple Mac OS X 10.6 and greater */
#include <sys/types.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
int errno;
int casefix(char *);

void mt_openro__(char *tape, int *tlu)
{}

void mt_openrw__(char *tape, int *tlu)
{}

void mt_close__(int *tlu)
{ }

void mt_rew__( int *tlu, int *ierr)
{}

void mt_where_( int *tlu, int *pos)
{}

void mt_resetpos_( int *tlu, int *pos)
{}

void mt_rewul__( int *tlu, int *ierr)
{}

void mt_cse__( int *tlu, int *ierr)
{}

void mt_fr__( int *tlu, int *nr, int *ierr)
{}

void mt_br__( int *tlu, int *nr, int *ierr)
{}

void mt_ff__( int *tlu, int *nf, int *ierr)
{}

void mt_bf__( int *tlu, int *nf, int *ierr)
{}

void mt_weof__( int *tlu, int *ierr)
{}

void mt_getstatus_( int *tlu, int *t, int *s, int *e, int *r, int *ierr)
{}

void dev_getstatus_( int *tlu, unsigned *s_e, unsigned *h_e, 
         int *st, int *ierr)
{}

void mt_readw__(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{}

void mt_writew__(int *tlu, char *buf, int *nby_req, int *nb_wt, int *ierr)
{}

void mt_read__(int *tlu, char *buf, int *nby_req, int *nb_rd, int *ierr)
{}

void mt_write__(int *tlu, char *buf, int *nby_req, int *nb_w, int *ierr)
{}

void mt_wait_(int *tlu, char *buf, int *nbyte, int *ierr)
{}

void mt_waitnd_(int *tlu, char *buf, int *nbyte, int *limit, int *ierr)
{}

void mt_nb_off_(int *tlu, int *ierr)
{}

void mt_ioclr__(int *tlu, int *ierr)
{}

void mt_nb_on_(int *tlu, int *count, int *ierr)
{}

void mt_nbnd_on_(int *tlu, int *count, int *ierr)
{}

void mt_nd_on_(int *tlu, int *ierr)
{}

void mt_holdnb_(int *lut, int *limit, int *ierr)
{}

void mt_r_holdnb_(int *lut, int *limit, int *ierr)
{}

void mt_w_holdnb_(int *lut, int *limit, int *ierr)
{}
/*************UTILITY ROUTINES*****************/
/*
C$PROG CSWAB
*/
/*   FORTRAN:  SUBROUTINE CSWAB(data, nbytes) */
cswab_(char *data, int *bytes)
{
#if defined(__APPLE__) || defined(__CYGWIN__)
     void swab(const void *, void *, ssize_t);
     size_t nbyte;
     nbyte = *bytes;
     swab((const void *)data,(void *)data, nbyte);
#else
     void swab(char *, char *, int);
     int nbyte;
     nbyte = *bytes;
     swab(data,data, nbyte);
#endif
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
/*
C$PROG FCASEFIX
*/
/*   FORTRAN:  SUBROUTINE FCASEFIX(istring)         */
int fcasefix_(char *s)
{
    int i;
    i=casefix(s);
    return i;
}

