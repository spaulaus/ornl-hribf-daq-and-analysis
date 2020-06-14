/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /tera/mcsq/Dlinux/Dvme/vmeboot.c
*
*    Description:  Routine to boot the VME processor after powerup. It will
*                  also reboot the system if the running system can respond
*                  to Ethernet interrupts.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/15/92    MCSQ         Original
*
*    7/31/92    MCSQ         After booting the basic system, the CAMAC and
*                            FASTBUS drivers are loaded.
*
*    8/ 4/92    MCSQ         Allocate 128K bytes for loading Data Acquisition
*                            parameters.  This space is allocated just after
*                            the basic system.  This allows(for now at least)
*                            reference by the VME at a fixed address.
*
*   10/30/92    MCSQ         Change error exit code.  Send all error
*                            messages to STDERR.
*
*   12/14/92    MCSQ         Change to new Ethernet library.
*
*   12/29/92    MCSQ         Load and execute the code devchk.c in the VME
*                            processor.  This code checks for the presence
*                            of hardware devices in the VME system.  Flags
*                            set by devchk.c are now used by cnafxx and
*                            fastxx to determine if the hardware is present.
*                            NOTE:  devchk.c must execute before cnafxx and
*                            fastxx.
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    5/ 1/93    MCSQ        VME processors now use a multicast to request
*                           boot.
*
*    6/ 1/94    MCSQ        Output warnings when system software is NOT
*                           loaded from the HHIRF directory.
*
*    1/20/95    MCSQ        Modified for the new VME boot loader.
*
*    2/15/95    MCSQ        Memory allocation routine in the VME processor
*                           has been changed.  The call to allocate memory
*                           for the acquisition parameters now requests that
*                           memory block allocated remain unaltered.
*
*    3/ 6/97    MCSQ        New for CPU-60 processors.  Processors vme1 thru
*                           vme19 are now assumed to be CPU-40s.  Processors
*                           vme20 and greater are CPU-60s.  When there
*                           must be code differences between processors,
*                           filename.run is the CPU-40 version and 
*                           filename60.run is the CPU-60 version.
*
*    7/24/98    MCSQ        Eliminate the default VME processor.
*
*    7/28/02    MCSQ        Ported to Linux
*
*    3/19/03    MCSQ        Changed for new pkt_io.c
*
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <wait.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "mem_mgr.h"

#define  MAXC  257    /* max size of input line from data file */

/*
*   Command strings for loading CAMAC and FASTBUS drivers
*/

/*
*   When you compile for hhirf directory, define HHIRF in this code
*   or use 'makeall hhirf'
#define  HHIRF
*/

#ifdef  HHIRF

#define  RUN      "/usr/acq/vme/run"
#define  CAMAC    "/usr/acq/vme/cnafxx"
#define  FASTBUS  "/usr/acq/vme/fastxx"
#define  DEVCHK   "/usr/acq/vme/devchk"
#define  MEMMGR   "/usr/acq/vme/memmgr"
#define  VMERESET "/usr/acq/vme/vmereset"
#define  BIOS     "/usr/acq/vme/bios"

#else

/*
*   Must define WHO to be top level directory of acquisition system
*   software.
*/

#define WHO        "/tera/mcsq/Dlinux"

#define  RUN      WHO"/Dvme/run"
#define  CAMAC    WHO"/Dvme/cnafxx"
#define  FASTBUS  WHO"/Dvme/fastxx"
#define  DEVCHK   WHO"/Dvme/devchk"
#define  MEMMGR   WHO"/Dvme/memmgr"
#define  VMERESET WHO"/Dvme/vmereset"
#define  BIOS     WHO"/Dvme/bios"

#endif
/*
*     Function Prototypes
*/
void drv_load(void);
void params_alloc(char *);
void set_time(void);
void sr_write(char *inbuf,int size);
void sr_close(void);
void mgr_error(int );
void cpu60_chk(char *,char *);

/*
*    Global variables
*/
char                 server[12] = "vme";
char                 in_line[MAXC],filename[80],*prog;
int                  buflen;
unsigned char       *buf_ptr;
int                  mem_id;

FILE                *infile;

struct Vmed xbuf,rbuf;
union Cmd *inbuf = (union Cmd *)rbuf.buf;
union Cmd *outbuf = (union Cmd *)xbuf.buf;

/***************************************************************************
*
*  Command syntax:
*
*    vmeboot  [file]
*
*      If no 'file' parameter is given, the default VME operating system
*      is loaded(i.e. memmgr.run).  Otherwise, the specified 'file'.run
*      is loaded.
***************************************************************************/
main(int argc, char *argv[])
{
   char *cptr;
   int size,pid,status;
   static char *vmereset[] = {VMERESET,NULL};

   size = strlen(argv[0]);
   prog = argv[0];
/*
*   If no load file name is given, use "memmgr.run".  Else use the
*   file name supplied by the user.  Note that the extension must be ".run".
*/
   if (argc < 2)
     {
       strcpy(filename,MEMMGR);
     }
   else
     {
       strcpy(filename,argv[1]);
       strcat(filename,".run");
     }
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);

   cpu60_chk(filename,server);
   printf("Loading the VME operating system\n");
/*
*   Open the input file
*/
   if ((infile = fopen(filename,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open S record file - %s\n",filename);
       exit(5);
     }
  if (strstr(filename,"/acq") == NULL) printf("**** Loading %s\n",filename);
/*
*  Download code to the VME processor.
*/
   buflen = sizeof(outbuf->load.data);
   buf_ptr = (unsigned char *)outbuf->load.data;
   outbuf->start.func = LOAD_MEM;
   outbuf->start.mid = mem_id;
   xbuf.len = sizeof(union Cmd);
   rbuf.len = 0;
   while (fgets(in_line,MAXC,infile) != NULL)
     {
       size = strlen(in_line);
       sr_write(in_line, size);
       if(!strncmp(in_line,"S9",2)) break;
     }
   sr_close();
   pkt_close();

   rbuf.len = sizeof(union Cmd);
/*
*   Set clock time in VME processor.
*/
   set_time();
/*
*   Allocate space for data acquisition parameters.
*/
   params_alloc("Acq_Params");
/*
*   Load CAMAC and FASTBUS driver routines.
*/
   drv_load();
   printf("VME operating system is loaded.\n");
   return(0);
}
/****************************************************************************
*   Load hardware drivers.
****************************************************************************/
void drv_load(void)
{
  int  err = 0, status;
  static char str1[] = RUN;
  static char str2[80];
  static char *args[] = {str1,str2,NULL};

/*
*   Load and execute devchk.c in the VME processor.  NOTE: This must
*   execute before the CAMAC and FASTBUS drivers execute.
*/
  strcpy(str2,DEVCHK);
  if (strstr(str2,"/acq") == NULL) printf("**** Loading %s\n",str2);
  if(vfork() == 0) {execv(args[0],args); perror("vmeboot"); _exit(99);}
  wait(&status);
  if (status != 0)
    {
      fprintf(stderr,"Cannot load DEVCHK - %s\n",str2);
      err = 50;
    }
  sleep(1);
/*
*   Load the CAMAC driver.
*/
  strcpy(str2,CAMAC);
  if (strstr(str2,"/acq") == NULL) printf("**** Loading %s\n",str2);
  if(vfork() == 0) {execv(args[0],args); perror("vmeboot"); _exit(99);}
  wait(&status);
  if (status != 0)
    {
      fprintf(stderr,"Cannot load CAMAC driver - %s\n",str2);
      err = 50;
    }
/*
*   Load the FASTBUS driver.
*/
  strcpy(str2,FASTBUS);
  if (strstr(str2,"/acq") == NULL) printf("**** Loading %s\n",str2);
  if(vfork() == 0) {execv(args[0],args); perror("vmeboot"); _exit(99);}
  wait(&status);
  if (status != 0)
    {
      fprintf(stderr,"Cannot load FASTBUS driver - %s\n",str2);
      err = 50;
    }
  if (err != 0) exit(err);
  if (getenv("VMECONFIG") != NULL)
    {
/*
*   Enable rx_vmeprom only if VMECONFIG is defined.  Normally this is done
*   in the script BOOTVME.
*/
     printf("Loading BIOS\n");
     strcpy(str2,BIOS);
     if (strstr(str2,"/acq") == NULL) printf("**** Loading %s\n",str2);
     if(vfork() == 0) {execv(args[0],args); perror("vmeboot"); _exit(99);}
     wait(&status);
     if (status != 0)
       {
         fprintf(stderr,"Cannot load BIOS - %s\n",str2);
         err = 50;
       }
     if (err != 0) exit(err);
    }
}
/****************************************************************************
*   Allocate the memory for the Data Acquisition Parameters.
****************************************************************************/
void params_alloc(char *cptr)
{
  int len;
 
  outbuf->alloc.func = ALLOC_MEM;
  outbuf->alloc.size = 128;
  outbuf->alloc.zerof = 0;
  strcpy(outbuf->alloc.name,cptr);
  pkt_io(&xbuf,&rbuf,CODE,5);
  if (inbuf->reply.status != OK)
   {
     mgr_error(inbuf->reply.status);
     exit(50);
   }
  mem_id = inbuf->alloc.mid;
}
/****************************************************************************
*   Set clock time in the VME processor.
****************************************************************************/
void set_time(void)
{
  int  tmo = 400;
  char chour[4];
  time_t timex;
  struct tm *tptr;
  int  dst,lt_hr,utc_hr,size,tzone;

/*
*   The VME processor must have enough time to start the ethernet driver
*   before this message is sent.
*/

  set_rtimer__(&tmo);
  check_rtimer__();
  timex = time(NULL);
  tptr = localtime(&timex);
  outbuf->time.yrs = tptr->tm_year;
  outbuf->time.dst = tptr->tm_isdst;
  outbuf->time.mon = tptr->tm_mon + 1;
  outbuf->time.day = tptr->tm_mday;
  outbuf->time.hrs = tptr->tm_hour;
  outbuf->time.min = tptr->tm_min;
  outbuf->time.sec = tptr->tm_sec;
  dst = tptr->tm_isdst;
  strftime(chour,3,"%H",tptr);
  lt_hr = atoi(chour);
  tptr = gmtime(&timex);
  strftime(chour,3,"%H",tptr);
  utc_hr = atoi(chour);
  tzone = 0;
  while ((lt_hr % 24) != utc_hr) { tzone++; lt_hr++; }
  if (dst != 0) tzone++;
  outbuf->time.tzone = tzone;
  outbuf->time.func = SET_TIME;
  pkt_io(&xbuf,&rbuf,CODE,5);
}
/****************************************************************************
*   Pack the output packet and send it when full.
****************************************************************************/
void sr_write(char *buf,int size)
{
   int status;

   if (size >= buflen)
     {
       *buf_ptr = '\0';
       xbuf.len = sizeof(struct Load)-buflen+1;
       status = pkt_io(&xbuf,&rbuf,CODE,5);
       if (status != 0) {mgr_error(status); exit(50);}
       buf_ptr = (unsigned char *)outbuf->load.data;
       buflen = sizeof(outbuf->load.data);
     }
   strncpy((char *)buf_ptr,buf,(size_t)size);
   buf_ptr += size;
   buflen -= size;
}
/****************************************************************************
*   Send the last packet (if any).
****************************************************************************/
void sr_close(void)
{
   int status;

   *buf_ptr = '\0';
   xbuf.len = sizeof(struct Load)-buflen+1;
   status = pkt_io(&xbuf,&rbuf,CODE,5);
   if (status != 0) {mgr_error(status); exit(50);}
}
/****************************************************************************
*   Report error from remote memory manager
****************************************************************************/
void  mgr_error(int error)
{
   fprintf(stderr," %s error \7 - ",prog);
   switch (-error)
    {
      case  SRADRERR:
        fprintf(stderr,"S record address outside allocated memory\n");
        break;
      case  SRCHKSUM:
        fprintf(stderr,"S record checksum error\n");
        break;
      case  SRILLHEX:
        fprintf(stderr,"S record has nonhex character\n");
        break;
      case  SRCNTERR:
        fprintf(stderr,"S record byte count error\n");
        break;
      case  ILLINDEX:
        fprintf(stderr,"Illegal memory ID - must be in range 3 <= ID <= %d\n",MEM_SEGS);
        break;
      case  KILLERR:
        fprintf(stderr,"Request to kill nonexistent task\n");
        break;
      case  TSKMEM:
        fprintf(stderr,"Insufficient task memory allocated\n");
        break;
      case  ALLOCERR:
        fprintf(stderr,"Memory allocation table is full\n");
        break;
      case  ILLFUNC:
        fprintf(stderr,"Unrecognized command function code\n");
        break;
      case  PERMTSK:
        fprintf(stderr,"Request to kill a permamently resident task\n");
        break;
      case  NOMEMAVAIL:
        fprintf(stderr,"Not enough memory available\n");
        break;
      case  MEMDUPC:
        fprintf(stderr,"Duplicate memory/task name\n");
        break;
      case  ILLFREE:
        fprintf(stderr,"Free memory request error - size <= 0\n");
        break;
      case  -ETHER_RECEIVE:
        printf("Ethernet receive timeout.\n");
        break;
      case  -ETHER_TRANSMIT:
        printf("Ethernet transmit error.\n");
        break;
      case  -ETHER_OPEN:
        printf("Ethernet open failure.\n");
        break;
      default:
        fprintf(stderr,"Unknown error code = %i\n",-error);
        break;
    }
}
/******************************************************************************
*
*   Test for CPU-60 processor.  If it is a CPU-60, check for special
*   version of code for CPU-60 processors.
*
*   CPU-60 processors are vme20 thru vme99.
*
*   If code is special for CPU-60, append '60' to file name.  For example,
*   memmgr.run is for CPU-40s and memmgr60.run is for CPU-60s.
******************************************************************************/
void cpu60_chk(char *file,char *server)
{
     int  cpu60 = 1,len;
    char  *cptr;
   struct stat statbuf;

   len = strlen(file);
   if (strlen(server) <= 4) cpu60 = 0;
   if (!strncmp(server,"vme1",4)) cpu60 = 0;
   else
     {
       strcat(file,"60.run");
       if (stat(file,&statbuf) == -1) cpu60 = 0;
     }
   if (!cpu60)
     {
       *(file+len) = '\0';
       strcat(file,".run");
     }
   return;
}
