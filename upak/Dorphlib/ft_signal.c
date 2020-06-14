/***   ft_signal.c   ****

C$PROG FT_SIGNAL

     A Fortran-callable, C-language interface to the "signal" system service.
     To use:
          call signal(signal-number, signal-handler)

     where
          signal-number is an integer representing the signal
                        to be processed by the signal handler

          signal-handler is a subroutine to be called when the
                        signal "signal-number" is received.

     Guilty party: R. Varner   3 November 1999
                  
*********************************************************************/

#include <signal.h>
/*#include <bits/signal.h> */

typedef void (*sighandler_t)(int);

int *signal_(int *signum, void *catch(int))
{
    sighandler_t *valret;

    valret = signal(*signum, (sighandler_t *) catch);

    if (valret == SIG_ERR) {
       perror("Fortran signal");
    }
    return ((int *)valret);
}

