/*======================================================================
  en_tmo.c sets the timeout for an ethernet read.  This version 
  is for packetfilter on the DECstation.
  Syntax:
        status=en_tmo(int pfd, int seconds_to_wait)
        status<0 is bad news.

========================================================================*/

extern int en_timeout;

int en_tmo(int pfd, int seconds_to_wait)
{
   en_timeout = (seconds_to_wait == 0) ? 60 : seconds_to_wait;

   return 0;
}
