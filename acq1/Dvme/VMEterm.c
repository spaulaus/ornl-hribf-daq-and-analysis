/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2003 
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /tera/mcsq/Dlinux/Dvme/VMEterm.c
*
*    Description:  
*                 
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/15/03    MCSQ        Original
*
*****************************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <termio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <sys/time.h>
#include <sys/ioctl.h>

/*
*     Function Prototypes
*/
static char *getfield(char *,char *,int);
static void cleanup(int);
static void helpmesg(void);

/*   Global variables       */

 struct termio init_tty,vme_tty;
 char tbuf[128];
 char rbuf[128];
 int  termio,pid;

/****************************************************************************
****************************************************************************/
main()
{
  char *cptr;
  char loadname[80],cmd[16];
  FILE *loadfile;
  int  i,status,len;
  fd_set readfds;
  char bpstr[] = "bp $b01,1\r";
  char lostr[] = "lo <1 ,,\r";
  char initvme[] = "\r";

/*
*   Setup to catch some signals so we can clean up before exit.
*/
   signal(SIGHUP,cleanup);
   signal(SIGINT,cleanup);
   signal(SIGQUIT,cleanup);
   signal(SIGILL,cleanup);
   signal(SIGTRAP,cleanup);
   signal(SIGBUS,cleanup);
   signal(SIGTERM,cleanup);
   signal(SIGUSR1,cleanup);
   signal(SIGUSR2,cleanup);
   signal(SIGCHLD,cleanup);
/*
*   Open serial port 1.  Get port configuration so we can restore on exit.
*/
   termio = open("/dev/ttyS0",O_RDWR);
   if (termio < 3)
     {
       perror("VMEterm");
       printf("Can not open serial line ttyS0\n");
       exit(99);
     }
   if (ioctl (termio,TCGETA,&init_tty) == -1)
     {
       perror("VMEterm");
       printf("Can not get ttyS0 configuration\n");
       close(termio);
       exit(99);
     }
/*
*   Configure ttyS0 for our use.
*/
   vme_tty.c_iflag = IGNCR | IXON;
   vme_tty.c_cflag = B9600 | CS8 | CREAD | CLOCAL;
   vme_tty.c_cc[VMIN] = 1;
   if (ioctl (termio,TCSETA,&vme_tty) == -1)
     {
       perror("VMEterm");
       printf("Can not set ttyS0 configuration\n");
       close(termio);
       exit(99);
     }

   printf("Type ? and a <CR> for local command help\n");

   write(termio,initvme,strlen(initvme));

   if (pid = fork())
    {
/*
*   Parent process
*/
      while(fgets(tbuf,80,stdin) != NULL)
       {
         cptr = getfield(cmd,tbuf,sizeof(cmd));
         if (!strcmp(cmd,"end") || !strcmp(cmd,"END")) cleanup(0);
         if (!strcmp(cmd,"?"))
           {
             helpmesg();
             strcpy(tbuf,initvme);
           }
         if (!strcmp(cmd,"ctrlc") || !strcmp(cmd,"CTRLC"))
           {
             tbuf[0] = 0x03;
             tbuf[1] = 0;
           }
         if (!strcmp(cmd,"ctrld") || !strcmp(cmd,"CTRLD"))
           {
             tbuf[0] = 0x04;
             tbuf[1] = 0;
           }
         if (!strcmp(cmd,"load"))
           {
             cptr = getfield(loadname,cptr,sizeof(loadname));
             strcpy(tbuf,initvme);
             if (cptr == NULL)
               {
                 printf("Need file name\n");
               }
             else
               {
                 printf("Loading File: %s\n",loadname);
                 loadfile = fopen(loadname,"r");
                 if (loadfile == NULL)
                   {
                     perror(loadname);
                   }
                 else
                   {
                     write(termio,bpstr,strlen(bpstr));
                     sleep(2);
                     write(termio,lostr,strlen(lostr));
                     sleep(1);
                     while (fgets(tbuf,sizeof(tbuf)-1,loadfile) != NULL)
                      {
                        tbuf[strlen(tbuf)-1] = '\r';
                        write(termio,tbuf,strlen(tbuf));
                      }
                     fclose(loadfile);
                     strcpy(tbuf,initvme);
                   }
               }
           }
         if (!strcmp(cmd,"break") || !strcmp(cmd,"BREAK"))
           {
             tcsendbreak(termio,1);
             continue;
           }
         len = strlen(tbuf);
         tbuf[len-1] = '\r';
         write(termio,tbuf,len);
       }
    }
   else
    {
/*
*   Child process
*/
      while(1)
       {
         status = read(termio,rbuf,sizeof(rbuf)-1);
         write(1,rbuf,status);
       }
      exit(99);
    }
   return(0);
}
/***************************************************************************
*
*  Extract one field from the source string.  Fields are delimited by
*  space(s), tab(s) and new_line characters.
*
* Call:  s1  -  pointer to destination string
*        s2  -  pointer to source string
*        n   -  number of characters(including the NULL at end of string)
*               to store in destination.  If the field in the source
*               is greater than n-1, the left most n-1 characters are
*               returned.
*
* Return:  Pointer to next character in source sting following this
*          field.
***************************************************************************/
static char *getfield(char *s1,char *s2,int n)
{
   char *cptr;

   *s1 = '\0';
   for (;*s2 == ' ' || *s2 == '\t'; s2++);
   for (cptr = s2; *cptr != ' ' && *cptr != '\0' && *cptr != '\n'; cptr++);
   if (cptr - s2 != 0)
     {
       if (cptr - s2 < n)
         n = cptr - s2;
       else
         n = n -1;
         strncpy(s1,s2,(size_t)n);
         *(s1 + n) = '\0';
         return(cptr);
      }
    else
      return ((char *)NULL);
}
/***************************************************************************
***************************************************************************/
static void cleanup(int code)
{
/*
*   Restore ttyS0 to the state it was when this program started
*/
   if (ioctl (termio,TCSETA,&init_tty) == -1)
     {
       perror("VMEterm");
       printf("Can not set ttyS0 configuration\n");
       close(termio);
       kill(pid,SIGTERM);
       exit(99);
     }
   kill(pid,SIGTERM);
   exit(code);
}
/***************************************************************************
***************************************************************************/
static void helpmesg(void)
{
   printf("\n");
   printf("break         - Send a BREAK to the VME processor\n");
   printf("ctrlc         - Send a control C to the VME processor\n");
   printf("ctrld         - Send a control D to the VME processor\n");
   printf("load filename - Download an S-record file to the VME processor\n");
   printf("                Example: load vme3_boot.run loads the boot\n");
   printf("                         program to processor vme3\n");
   printf("end           - Exit this program\n");
   printf("\n");
   printf("  Enjoy\n");
}

