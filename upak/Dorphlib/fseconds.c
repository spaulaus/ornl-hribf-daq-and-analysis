/*
  fseconds_.c 
  A Fortran-callable function to return seconds since Jan 1, 1970, which is the
default system clock.  The floating point number it returns has the seconds since
the "epoch" and the fraction of a second down to the resolution of the system
clock, I think.  It calls the system routine "gettimeofday" to read the system
clock and returns the seconds value added to the fraction of a second value.

  Calling:
  In Fortran --------------------
      real*8   fseconds
      external fseconds
  -------------------------------


  Robert Varner is to blame for the errors in this code
*/
#include <sys/time.h>

double fseconds_()
{
  struct timeval tp;
  struct timezone tz;
  double tod;
  
  if (gettimeofday(&tp, &tz) == -1) {
    perror("Error getting timeofday");
    return(0);
  }
 
  tod = (double) tp.tv_sec + (double)tp.tv_usec/(double)1000000.0;

  return (tod);
}
