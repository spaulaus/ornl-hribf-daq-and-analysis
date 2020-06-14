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
*    File:         /usr/users/mcsq/Dlinux/Dvme/run.c
*
*    Description:  Code to load and start a program in the VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/19/92    MCSQ         Original
*
*    6/13/92    MCSQ         Added routine sr_link which calls the OASYS
*                            Linker when no *.run file exists or the 
*                            load address of the current *.run does not
*                            match the memory allocation.
*
*    7/13/92    MCSQ         Memory segment in the VME processor now uses
*                            just the program name.  Previously, the
*                            segment name included the path for the file
*                            *.run.
*                            
*   10/29/92    MCSQ         Added optional parameters on the command line.
*                            If the program file name includes a path (i.e.
*                            one of your subdirectories or another user's
*                            directory), the output of the OASYS Linker
*                            is a temporary file which is deleted on exit.
*                            CAUTION: If the Linker command file ( *.cmd )
*                            is in another directory, IT MUST specify the
*                            complete path to any object files used!!
*
*                            Changed Error exit codes.
*
*    12/14/92   MCSQ         Delete allocated memory when OASYS linker
*                            terminates with an error.  Changed to the
*                            new Ethernet library.
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    2/18/94    MCSQ        Changed path to OASYS cross tools.
*
*    5/ 1/94    MCSQ        Corrected the default library path.
*
*    2/15/95    MCSQ        Memory allocation routine in the VME processor
*                           now requires a flag in the allocation request
*                           which determines the state of the memory
*                           block granted.  If the flag is zero, the
*                           memory is unaltered.  If the flag is non-zero,
*                           the allocated block is set to zero.
*                           This routine always sets the memory to zero.
*
*    4/13/95   MCSQ         If the .run file is unusable, we first try
*                           a simple loader which can only handle modules
*                           which have no external symbols and no absolute
*                           sections.  If that doesn't work we use the
*                           OASYS linker.
*
*    3/ 6/97   MCSQ         New for the CPU-60 processors.  Processors vme1
*                           thru vme19 are now assumed to be CPU-40s.
*                           Processors vme20 and greater are CPU-60s.
*                           Most codes run on either processor.  However,
*                           if different versions are required, a '60' is
*                           appended to the filename to indicate that it
*                           is for the CPU-60.
*
*    1/27/98   MCSQ         Fix routine sr_link.  The array of pointers,
*                           args, was too small.  Caused problems with
*                           latest OS for ALPHAs.
*
*    7/24/98    MCSQ        Eliminate the default VME processor.
*
*    7/27/02    MCSQ        Adapted for Linux
*
*    3/18/03    MCSQ        Changed for new pkt_io.c
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "vme_sys.h"
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "mem_mgr.h"

#define  MAXC  257

/*
*   Default OASYS Linker command strings
*/
#define  LINKER  "/usr/oasys2/l68"
#define  LIB     "+/usr/oasys2/68040o/libansi,+libind"
/*
*   When you compile for hhirf directory, define HHIRF in this code
*   or use 'makeall hhirf'
#define  HHIRF
*/

#ifdef  HHIRF

#define  VMESYS  ",/usr/acq/vme/gogoc,opsys-r,"
#define  LOADER  "/usr/acq/vme/vmeload"
#else

#define WHO        "/tera/mcsq/Dlinux"

#define  VMESYS  ","WHO"/Dvme/gogoc,opsys-r,"
#define  LOADER  WHO"/Dvme/vmeload"
#endif

/*    Function Prototypes       */
void sr_write(char *inbuf,int size);
void sr_close(void);
void sr_alloc(char *);
void mem_dealloc(void);
void sr_link(char *,int);
void sr_start(void);
void mgr_error(int);
void err_exit(int );
void cpu60_chk(char *,char *);

/*
*                      Global variables
*/
char                 server[12] = "vme";
char                 in_line[MAXC];      /* line buffer for file input       */
char                 filename[80];       /* filename of program              */
char                *prog;               /* pointer to name of this code     */
char                 tmpfilename[L_tmpnam];  /* temporary file name          */
int                  buflen;
int                  path = 0;           /* Path flag. 0 = no path specified */
                                         /* 1 = path specified               */
                                         /* -1 = temporary file used         */
unsigned char       *buf_ptr;
int                  mem_id;             /* memory segment ID in VME         */
int                  memory_size = DEF_MEM_ALLOC;
int                  heap_size = DEF_HEAP;
int                  stack_size = DEF_STACK;
int                  priority = DEF_PRIORITY;
int                  time_slice = DEF_TIME;
int                  port = DEF_PORT;
int                 *params[] = {&memory_size, &heap_size, &stack_size,
                                 &priority, &time_slice, &port};

FILE *infile;

struct Vmed xbuf,rbuf;
union Cmd *outbuf = (union Cmd *)xbuf.buf;
union Cmd *inbuf = (union Cmd *)rbuf.buf;

/****************************************************************************
*
*  Command line syntax:
*
*   run file  [memory_size  heap_size  stack_size  priority  time  port]
*
*  Parameter:   file        - Name of program file to load
*
*  Optional parameters.  They must be in the order shown separated by space(s).
*               memory_size - memory size in Kilobytes
*               heap_size   - heap size to allocate in bytes
*               stack_size  - stack size to allocate in bytes
*               priority    - task priority
*               time        - time slice ( 1 = 10 milliseconds)
*               port        - terminal port number.  
*
*  Example:
*
*   run  acq_sys 256 4096 1024 69 1 1
*
*   Load program 'acq_sys'.  Initially allocate 256 Kbytes of memory.
*   Allocate a heap of 4096 bytes and a user stack of 1024 bytes.
*   Set task priority to 69 and the time slice to 10 milliseconds.
*   Port is 1 which means program can write to port 1.  No terminal
*   reads are possible port 1 input is assigned to Task0.
****************************************************************************/
main(int argc, char *argv[])
{
   char  *cptr;
   int   i,size,temp;

#ifndef HHIRF
   printf("Using VME system files %s\n",VMESYS);
#endif

   xbuf.len = sizeof(union Cmd);
   rbuf.len = sizeof(union Cmd);
   prog = argv[0];
   if (argc < 2) {fprintf(stderr,"Need load file name!\n"); exit(1);}
/*
*  Get optional parameters if any.
*/
   for (i=2; i < argc; i++)
      {
        if (sscanf(argv[i],"%i",&temp) == 1 && i < 8)
          {
            if (temp != 0) *params[i-2] = temp;
          }
        else
          {
            if (i <  8) fprintf(stderr,"Invalid Parameter: %s\n",argv[i]);
            else  fprintf(stderr,"Too many Parameters\n");
            fprintf(stderr,"Usage:  run file [memory_size  heap_size  \
stack_size  priority  time  port]\n");
            exit(2);
          }
      }
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);
/*
*   Allocate memory in the VME processor.  If "file".run exists and address
*   in the .run file matches the memory segment allocated, we just use the
*   existing .run file.  Otherwise, relink the program file.
*/
   strcpy(filename,argv[1]);
   cpu60_chk(filename,server);
   sr_alloc(filename);
/*
*   Open the input file
*/
   strcat(filename,".run");
   if ((infile = fopen(filename,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open S record file - %s\n",filename);
       err_exit(5);
     }
/*
*   Now load the .run file.
*/
   buflen = sizeof(outbuf->load.data);
   buf_ptr = (unsigned char *)outbuf->load.data;
   outbuf->start.func = LOAD_MEM;
   outbuf->start.mid = mem_id;
   while (fgets(in_line,MAXC,infile) != NULL)
     {
       size = strlen(in_line);
       sr_write(in_line, size);
       if(!strncmp(in_line,"S9",2)) break;
     }
   sr_close();
/*
*   Tell the VME processor to start the task just loaded.
*/
   sr_start();
/*
*   If the .run file is a temporary file, delete it.
*/
   if (path < 0) unlink(filename);
   return(0);
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
       status = pkt_io(&xbuf,&rbuf,PROTO_CODE,5);
       if (status != OK)
         {
           mgr_error(status);
           err_exit(50);
         }
       if (inbuf->reply.status != OK)
         {
           mgr_error(inbuf->reply.status);
           err_exit (50);
         }
       buf_ptr = (unsigned char *)outbuf->load.data;
       buflen = sizeof(outbuf->load.data);
     }
   strncpy((char *)buf_ptr,buf,(size_t)size);
   buf_ptr += size;
   buflen -= size;
}
/****************************************************************************
*   Send the last packet.
****************************************************************************/
void sr_close(void)
{
   int  status;

   *buf_ptr = '\0';
   xbuf.len = sizeof(struct Load)-buflen+1;
   status = pkt_io(&xbuf,&rbuf,PROTO_CODE,5);
   if (status != OK)
    {
      mgr_error(status);
      err_exit(50);
    }
   if (inbuf->reply.status != OK)
     {
       mgr_error(inbuf->reply.status);
       err_exit (50);
     }
}
/****************************************************************************
*   Allocate the memory for the task we are going to load.
****************************************************************************/
void sr_alloc(char *cptr)
{
  int i,len,status;
  char *sptr;
 
  outbuf->alloc.func = ALLOC_MEM;
  outbuf->alloc.size = memory_size;    /* memory size to allocate in Kbytes */
  outbuf->alloc.zerof = 1;             /* zero allocated memory block       */
/*
*   The command line file name may contain a path.  If so, extract the
*   program name and set the path present flag.
*/
  i = strlen(cptr);
  sptr = cptr + i -1;
  while(sptr != cptr)
   {
     if(*sptr == '/')
      {
        sptr++;
        path = 1;
        break;
      }
     sptr--;
   }
  i = cptr - sptr + i + 1;
/*
*   Set the VME task name to the program name.
*/
  strncpy((char *)outbuf->alloc.name,sptr,(size_t)i);
  status = pkt_io(&xbuf,&rbuf,PROTO_CODE,5);
  if (status != OK)
   {
     mgr_error(status);
     err_exit(51);
   }
  if (inbuf->reply.status != OK)
   {
     mgr_error(inbuf->reply.status);
     err_exit(51);
   }
/*
*   Link the program if needed.
*/
  mem_id = inbuf->alloc.mid;
  sr_link(cptr,inbuf->alloc.begin + CODE_OFF +2);
}
/****************************************************************************
*   Deallocate memory we didn't use due to an error in the OASYS Linker.
****************************************************************************/
void mem_dealloc(void)
{
  int status;
 
  outbuf->free.func = FREE_MEM;
  outbuf->free.mid = mem_id;
  outbuf->free.begin = 0;
  outbuf->free.end = 0;
  status = pkt_io(&xbuf,&rbuf,PROTO_CODE,5);
  if (status != OK)
    {
      mgr_error(status);
      err_exit(52);
    }
  if (inbuf->reply.status != OK)
   {
     mgr_error(inbuf->reply.status);
     mem_id = 0;
     err_exit(52);
   }
}
/****************************************************************************
*   Relink the *.run file if needed.
*
*   First check for a "file".run which matches the address of the memory
*   segment.  If such exists, use it.
*
*   Otherwise, we must relink the program.  If the command file name has
*   a path specification, use a temporary file for the .run and we will 
*   delete it when finished loading the program.
*
****************************************************************************/
void sr_link(char *file,int addr)
{
   int status;
   char *cptr, file_name[80];
   int  sradr;
   FILE *runfile, *cmdfile;

   static char xcode[] = LINKER;
   static char ycode[] = LOADER;
   static char cmd_sys[] = VMESYS;
   static char cmd_lib[] = LIB;
   static char cmdbuf[256];
   static char saddr[10];
   static char *args[6];

/*
*   Look for an existing "file".run
*/
   strcpy(file_name,file);
   strcat(file_name,".run");
   if((runfile = fopen(file_name,"r")) != NULL)
     {
/*
*   Found it so check the address for a match with the memory segment.
*/
       while (fgets(in_line,MAXC,runfile))
        {
          if(!strncmp(in_line,"S3",2))
            {
              fclose(runfile);
              in_line[12] = '\0';
              sscanf(&in_line[4],"%x",&sradr);
              if (sradr == addr) return;       /* Existing .run file is OK */
              break;
            }
        }
     }
/*
*   If the command line file name had a path specification, create a
*   temporary file for the Linker output.
*/
   if (path == 0) strcpy(cmdbuf,file);
   else
     {
       strcpy(tmpfilename,P_tmpdir);
       strcat(tmpfilename,"/RUNXXXXXX");
       mkstemp(tmpfilename);
       strcpy(cmdbuf,tmpfilename);
       unlink(tmpfilename);
     }
/*
*   First we try vmeload which works for modules having no external references
*   and no absolute sections.  If this doesn't work use the OASYS linker.
*/
   sprintf(saddr,"%x",addr);
   args[0] = ycode;
   args[1] = file;
   args[2] = cmdbuf;
   args[3] = saddr;
   args[4] = NULL;
   if(fork() == 0) 
     {
       execv(args[0],args);
       perror(args[0]);
       _exit(99);
     }
   wait(&status);
   if (status == 0)
     {
/*
*   vmeload worked so we are finished.
*/
       if (path > 0)
         {
           strcpy(file,tmpfilename);
           path = -1;
         }
       return;
     }
/*
*   We must relink the program.  Build the parameters for the OASYS
*   Linker.
*/
   printf("Relink code - %s\n",file);
   args[0] = xcode;
   args[1] = cmdbuf;
   args[2] = NULL;
   sprintf(saddr,"%x",addr);
   strcat(cmdbuf,"-o1:x");
   strcat(cmdbuf,saddr);
   strcat(cmdbuf,"-t:\"gogo\",=");
   strcpy(file_name,file);
   strcat(file_name,".cmd");
/*
*   If a "file".cmd exists,  use the part of the Linker command line
*   to the right of the = .  Otherwise, use the defaults.
*/
   if((cmdfile = fopen(file_name,"r")) != NULL)
     {
       fgets(in_line,MAXC,cmdfile);
       cptr = strstr(in_line,"=");
       if (cptr == NULL)
         {
           fprintf(stderr,"Linker Command file syntax error\n");
           err_exit(53);
         }
       strcat(cmdbuf,++cptr);
       cptr = strstr(cmdbuf,"\n");
       if (cptr != NULL) *cptr = '\0';
       fclose(cmdfile);
     }
   else
     {
/*
*   There is no .cmd file.  Build the default linker command string.
*/
       strcat(cmdbuf,file);
       strcat(cmdbuf,cmd_sys);
       strcat(cmdbuf,cmd_lib);
     }
/*
*   Invoke the OASYS Linker.
*/
   if(fork() == 0) 
     {
       execv(args[0],args);
       perror(args[0]);
       _exit(99);
     }
   wait(&status);
   if (status != 0)
     {
       err_exit(54);
     }
/*
*  If we used a temporary file, return the file name.
*/
   if (path > 0)
     {
       strcpy(file,tmpfilename);
       path = -1;
     }
}
/****************************************************************************
*   Start the task just loaded.
****************************************************************************/
void sr_start(void)
{
  int status;

  outbuf->start.func = START_TASK;
  outbuf->start.mid = mem_id;
  outbuf->start.start = 0;
  outbuf->start.heap = heap_size;
  outbuf->start.stack = stack_size;
  outbuf->start.priority = priority;
  outbuf->start.time_slice = time_slice;
  outbuf->start.port = port;
  status = pkt_io(&xbuf,&rbuf,PROTO_CODE,5);
  if (status != OK)
    {
      mgr_error(status);
      err_exit(55);
    }
  if (inbuf->reply.status != OK)
    {
      mgr_error(inbuf->reply.status);
      err_exit(55);
    }
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
        fprintf(stderr,"Unknown error code\n");
        break;
    }
}
/****************************************************************************
*   Error exit routine.
*
*   If memory has been allocated in the VME processor and the error
*   is not an Ethernet error, release the VME memory.
*
*   If a temporary file was created, delete it.   
****************************************************************************/
void err_exit(int error)
{

   if (mem_id != 0 && (error < 30 || error > 39)) mem_dealloc();
   if (path < 0) unlink(filename);
   exit(error);
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
   char name[80], *cptr;
   struct stat statbuf;

   if (strlen(server) <= 4) return;
   if (!strncmp(server,"vme1",4)) return;
   strcpy(name,file);
   strcat(name,"60.bin");
   if (stat(name,&statbuf) == -1)
     {
       return;
     }
   strcat(file,"60");
   return;
}
