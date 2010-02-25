/*======================================================================
   en_get_dest.c - gets a destination name.  Uses either
       1) command line "-d" argument,
       2) environment variable "VME", or
       3) default ("vme1" for clients, "public" for servers).
   Syntax:
         destination_name = get_dest(int argc, char *argv, int type)
========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#define SERVER_ENV_NAME "VME"

char *en_get_dest(int argc, char *argv[], int type)
{
   char *sname;

/*
*     Look for "-d" on the command line
*/
   while (--argc>0 && (*++argv)[0] == '-')
      if ((*argv)[1] == 'd') 
        {
          if (*++argv == NULL) break;
          else  return *argv;
        }
      else {
         ++argv;
         --argc;
      }
/*
*     Not found on argument line, try fallback position
*/
   if ((sname=getenv(SERVER_ENV_NAME)) != NULL) return sname;
/*
*     Not found anywhere, choose a likely default
*/
   if (type != 0) return "vme";
   else   return "private";

}
