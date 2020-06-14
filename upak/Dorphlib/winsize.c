/******************************************************************************
*  Routine to return the size of the screen.
*
*  CALL  WINSIZE(COLS,ROWS,IERR)
*
*  Returns:   INT*4  COLS   - Number of character positions on a line
*             INT*4  ROWS   - Number of lines
*             INT*4  IERR   - 0 means OK, -1 means error in getting screen
*                             size.  If IERR = -1, COLS is set to 80 and
*                             ROWS is set to 24.
******************************************************************************/
#include <sys/types.h>
#include <sys/ioctl.h>

#ifdef __CYGWIN__
#include <termios.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#define  STDOUT  1      /* File descriptor for standart output file */

void  winsize_(int *cols, int *rows, int *ierr)
{
   int status;
   struct winsize win;

   status = ioctl(STDOUT,TIOCGWINSZ,&win);
   if (status == -1 || win.ws_col == 0 || win.ws_row == 0)
     {
       *cols = 80;
       *rows = 24;
       *ierr = -1;
     }
   else
     {
       *cols = win.ws_col;
       *rows = win.ws_row;
       *ierr = 0;
     }
}
