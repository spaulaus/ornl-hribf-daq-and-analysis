/***   ft_signal.c   ****
C$PROG SIGNAL_FT
     A Fortran-callable, C-language interface to the "signal" system service.
     To use:
          call signal(signal-number, signal-handler)

     where
          signal-number is an integer representing the signal
                        to be processed by the signal handler

          signal-handler is a subroutine to be called when the
                        signal "signal-number" is received.

     Guilty party: R. Varner   9 November 1999
                  
*********************************************************************/

#include <signal.h>
#include <errno.h>

#define NULL 0

int signal_(int *signum, void *catch(int))
{
    int valret;
    struct sigaction ftaction;
    struct sigaction oldaction;

#if defined(__APPLE__) || defined(__CYGWIN__)
    ftaction.sa_handler = (void *) catch;
#else
    ftaction.sa_handler = (__sighandler_t) catch;
#endif
    sigemptyset(&ftaction.sa_mask);
    ftaction.sa_flags = (int) NULL;

    valret = sigaction(*signum, &ftaction, &oldaction);

    if (valret == EINVAL) {
       perror("Fortran signal");
    }
    return (valret);
}
