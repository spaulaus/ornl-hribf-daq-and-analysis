/*-----------------------------------------------------------------------------
    Interface for lemo to IPC shared memory buffer.  These are fortran
    callable C routines.

    Robert Varner 28 July 1992
*
*   Revisions:
*
*    11/20/93  MCSQ   Changed for new shared memory buffer scheme.  Function
*                     names are changed.  For example, ipcread is changed to
*                     readipc.  Added functions shmread and shmrelease.
*                      shmread returns a pointer to a data buffer.  shmrelease
*                     releases a buffer.
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

/****************************************************************************
****************************************************************************/
void openipc_(char *direction, char *device, int *bufsize, int *ierr)
{
/*  Dummy routine for offline use - if called give zero and error      */
    *ierr = -1;
    *bufsize=0;
    printf("You tried to openipc, which is not supported on this machine\n");
    return;
}

/****************************************************************************
****************************************************************************/
/*  closeipc - closes an open IPC stream for lemo.  The stream number
*/
void closeipc_()
{
/*  Dummy routine - it does nothing */
    printf("You tried to closeipc, which is not supported on machine\n");
}

/****************************************************************************
****************************************************************************/
/*  readipc - reads a buffer from an IPC input stream.  */
void readipc_( unsigned char *buffer, /* Buffer destination */
              int  *nbuf,   /* Size available     */
              int  *nread,  /* Number bytes read  */
              int  *ierr,   /* Error number       */
              int  *msgf)   /* CTRL C abort flag  */
{
    /* Dummy routine, when called, returns 0 bytes and an error */
    *ierr = -1;
    *nread=  0;
    printf("You tried to readipc, which is not supported on machine\n");
    return;
}
