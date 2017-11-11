/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dwks/pacman.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/ 6/93    MCSQ    Original     
*
*
*     2/9/93    MCSQ    Changed the message queue to the TAPEOU process to
*                       a bidirectional queue.  TAPEOU process sends a 
*                       message type = 1 when it is ready for a command.
*                       pacman receives message type = 1 before sending
*                       message type = 2 to TAPEOU as a command.  When
*                       a TAPEOU command is given and no message type = 1
*                       is available, a TAPEOU busy message is issued and
*                       command is ignored.  All this means there is no
*                       typeahead to TAPEOU!.
*
*                       Added commands to recreate DAMM and SCAN windows
*                       if user has deleted them.
*
*                       Added HILI specific command - dmon - which displays
*                       ethernet packets received from VME acquisition system.
*                       This code is independent of pftoipc and is a good
*                       check for data from VME.
*
*                       Fixed command - cd - with no argument so it works
*                       like C shell command.
*
*    3/ 1/93   MCSQ     Added windows for a) SCAD - scaler display, b)
*                       SCANMUS - muon scan program, and c) HODOSCAN -
*                       hodo scan program.
*
*                       Run pftoipc and TAPEOU processes at high priority.
*
*    4/ 3/93   MCSQ     Added routines spawn() and exec_wait().  Added 
*                       a log command to allow user to put comments into
*                       the log file.
*
*    6/12/93   MCSQ     Move command execution from main to a function  to
*                       allow a command to execute some other command.
*                       Revised handling of User windows.
*
*    7/ 5/93   MCSQ     Changed addressing for windows and icons.  Previously,
*                       all addresses were relative to top left of screen.
*                       Now addresses are:
*                         Window        Relative to
*                       logger        bottom right
*                       damm          top left
*                       scan          top right
*                       scanmus       top right
*                       hodoscan      top right
*                       scad          top right
*                       all icons     bottom right
*
*   12/ 2/93   MCSQ     New shared memory buffer scheme.
*
*    2/ 1/94   MCSQ     Many changes to report here.  Heretofore, this code
*                       was customized for the HILI.  It is now generic with
*                       any customizations from a user supplied configuration
*                       file.  Among the things specified in a configuration
*                       file are:
*                       1) The startup banner display.
*                       2) The size of the shared memory buffers(tape buffers
*                          are same size as shared memory).
*                       3) The log time interval( interval at which rate info
*                          is output to the log file).
*                       4) User specified windows such as DAMM and SCAN
*                       5) User commands.  Typically, user programs run from
*                          pacman in a temporary window.
*                       6) Aliases for builtin commands.
*
*                       Normally, the configuration file is in the user's
*                       home directory and the name is pacman.fig.  Also
*                       a config file name may be specified as an argument
*                       on the command line.  In addition, there is a default
*                       config file in /usr/hhirf/wks directory.
*
*                       A new command - userconfig - displays the parameters
*                       and commands from the configuration file.
*
*                       User can execute a sequence of pacman commands from
*                       a command file.  The pacman command
*                          cmdf test
*                       executes the commands in the file test.cmd.
*
*                       The pacman command - hup - stops and restarts the
*                       VME acquisition system.  This useful at very low
*                       data rates since forces VME buffers to be sent to
*                       workstation and hence to any histogramming processes.
*
*    2/ 8/94   MCSQ     Change detection of process completion in exec_wait
*                       and CLDsignal.  The problem was that the child process
*                       created by fork() could complete, generate the CLD
*                       signal and CLDsignal routine executed ALL before
*                       fork() call returned to pacman.  Previously, unixpid
*                       was set nonzero by the fork() and reset to zero
*                       in CLDsignal.  In the above case, unixpid was set to
*                       zero prior to being set nonzero.  Now unixpid set
*                       to the PID of the last process generating a CLD signal.
*                       exec_wait waits till unixpid equals the PID of the
*                       process just forked.
*
*    2/24/94   MCSQ     Added default parameters for things which can
*                       be defined in a configuration file.  Added the
*                       exitmess message on how to exit pacman in case
*                       a user help file does not contain this.
*
*    4/15/94   MCSQ     Added command 'pacfile' which displays the name
*                       of the pac last loaded into the VME processor.
*
*    7/24/94   MCSQ     Commands must always be lower case.  Added code
*                       in function add_cmd to always convert user
*                       defined commands to lower case.
*
*    2/28/95   HQJ      Modified for AXP/OSF. Changed the option flag
*                       [-U] for 'ps' so that the background processes are
*                       also listed. Use getlogin() to feed the command.
*
*                       Added a check-sending flag on TAPEOU commands, mainly
*                       for better executing "no typeahead to TAPEOU!".  If
*                       commands are from a .cmd file, put in a wait status.
*
*    3/21/95   HQJ      Added command 'zbuf' to clear the shm_buffers for
*                       read.  This is intended to do a quick flushing.
*
*    5/29/95   MCSQ     Added check for display type.  The configuration
*                        now allows conditions based on display type.  The
*                       various display have different fonts and now one
*                       include file may be used for all types of display.
*
*                       User defined commnads now pass command line arguments
*                       to the specified code.
*
*    6/11/95   MCSQ     Logger window specs are now determined by the
*                       type of display detected on startup.  Only the
*                       icon position and font are changed depending on
*                       type type of display.
*
*    9/ 8/95   MCSQ     Added commands for W.T. Milner's scaler readout
*                       with output of scaler data to tape.
*
*    5/13/96   MCSQ     Add host name to pacman startup message in the
*                       log file.
*
*    6/26/96   MCSQ     Correct the argument in the call to function
*                       sigblock in routine exec_wait.
*
*    8/10/96   MCSQ     Add commands to enable and disable automatic
*                       file marks for tape.
*
*   12/ 4/96   MCSQ     The signal SIGCLD was being blocked almost
*                       all the time by code in routine exec_wait.
*                       exec_wait needs to block SIGCLD until it
*                       is ready for the signal.  However, it was
*                       leaving SIGCLD blocked on exit.  Fixed that.
*
*                       pacman now makes a file, .pacpid.vmeXX where
*                       vmeXX is the VME processor name, in the users
*                       home directory.  The file is removed when pacman
*                       exits.  Currently this file has the PID of the
*                       TAPEOU process and the ID of the command message
*                       queue for TAPEOU.  Present use is for acq_tape_ctrl
*                       which allows processes other than pacman to control
*                       the tape.
*
*                       Added TON command to TAPEOU.  TON is like TRUN
*                       except it does not start the VME acqusition system.
*
*    1/31/97   MCSQ     RLV found a problem in init() when the workstation
*                       hostname is 25 or more characters long.  The system
*                       routine gethostbyname does not NULL-terminate the
*                       string when the specified return length is less
*                       than the real hostname length.
*
*                       Changed specified length to 36 characters and also
*                       NULL-terminate the string after the call to
*                       gethostbyname call.
*
*   12/ 6/97   MCSQ     The process pftoipc moves Ethernet data packets to
*                       shared memory.  Now we allow the user to specify
*                       a custom process which provides the functions of
*                       pftoipc plus user defined additional processing.
*
*                       Note that any modifications made to the data stream
*                       here effect data stored on tape and data received
*                       by any scan process.
*
*                       To specify a custom pftoipc process, use the
*                       $USER_PFTOIPC statement in the configuration file.
*
*                       Example:   $USER_PFTOIPC /usr/users/mcsq/feinput
*
*                       The code feinput replaces the normal pftoipc.
*
*   12/11/97   MCSQ     Add pftoipc process PID to the file .pacpid.vmeXX.
*                       Allows other processes access to pftoipc.
*
*    4/ 3/98   MCSQ     For the $USER_CMD specification in the pacman.fig
*                       file, allow a $initvme command.  This means execute
*                       initvme AFTER the user command is executed.
*
*    7/24/98   MCSQ     Eliminate the default VME processor.  User MUST
*                       set the environment variable to the name of the
*                       processor being used.
*
*    2/18/99   MCSQ     Allow argument for initvme command.  Add command
*                       setup which run modu_setup to initialize CAMAC
*                       modules.
*
*    4/29/99   MCSQ     We have a new 'tape' process which can write
*                       list data to a disk file.  Add new command
*                       OUF to tape process.
*
*    4/16/03   MCSQ     Since Linux boxes do not have tapes, I changed
*                       TAPEOU to FILEOU in user messages.
*
*   10/20/03   RLV      Add event counter logging to keep the
*                       DOE bean counters happy.
*
*    1/28/04   MCSQ     Add 100Hz VME clock.
*
*    5/19/04   MCSQ     Patched so that TRUN or TON do stopvme followed
*                       by a wait of 1 to 2 seconds and a zbuf. Also
*                       on a TSTOP we do a stopvme before the CTRL/C to
*                       the FILEOU process.
*
*    6/14/04   RLV      Require that the environment variable EXPT is
*                       set.
*
*    6/29/05   RLV      Modify for UDPtoIPC task and new acquisition
****************************************************************************/
/*
*   When you compile for hhirf directory, define HHIRF in this code
*   or use 'makeall hhirf'
#define  HHIRF
*/

/* New acquisition -  define macros for many commands*/
#define  HRIBF

#ifdef  HRIBF

#define  BASE      "/usr/acq2"

#define  LOGGER    BASE"/bin/logger"
#define  LOGGERALT    BASE"/bin/logger >/dev/null"
#define  XTAPE     BASE"/bin/tape"
#define  FEMSG     BASE"/bin/femsg"
#define  UDPTOIPC  BASE"/bin/udptoipc"
//#define  LT        BASE"/bin/lt"
#define  BOOTVME   BASE"/bin/bootvme"
#define  LOADACQ   BASE"/bin/loadacq"
//#define  LOADDSSD  BASE"/bin/loadDSSD"
#define  PACCMD     BASE"/bin/pacor"
#define  STARTVME  BASE"/bin/startvme"
#define  STOPVME   BASE"/bin/stopvme"
#define  STATVME   BASE"/bin/statvme"
#define  TESTVME   BASE"/bin/testvme"
#define  PACFILE   BASE"/bin/pacfile"
#define  INITVME   BASE"/bin/initvme"
#define  SETUP     BASE"/bin/setup"
#define  ZEROCLK   BASE"/bin/zeroclk"
#define  VMEHARDWARE BASE"/bin/vmehardware"
#define  VMEHOST   BASE"/bin/vmehost"
#define  VMECPUS   BASE"/bin/vmecpus"
//#define  KT        BASE"/bin/kt"
#define  PRIORITY  BASE"/etc/acq_priority"
#define  HELPFILE  BASE"/doc/pacman.hep"
#define  DOCFILE   BASE"/doc/pacmanII.doc"
#define  FIGFILE   BASE"/etc/pacman.fig"
#define  PRTDOC    "/usr/hhirf/dodoc pacmanII"

#endif

/* now the usual and unusual include files */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <sys/ioctl.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "orphmsg.h"
#define  ACQ
#include "ipcdefs.h"
#include "acqlib.h"
#include "acqshm.h"

#define STDIN   0    /* descriptor for standard input  */
#define STDOUT  1
#define STDERR  2

/*
*   Name of process which moves Ethernet packets to shared memory buffers
*/
char udptoipcfile[128] = UDPTOIPC;
/*
*    VME processor name
*/
char server[12];

/*
*    Start argument arrays for processes we will always start
*/

char *logger[20] = {"/usr/bin/xterm",
                  "-n",
                  "LOGGER",
                  "-sb",
                  "-sl",
                  "200",
                  "-geometry",
                  "116x10+0-0",
                  NULL };

char *loggeralt[] = { LOGGERALT,
                  server,
                  NULL };
char *tape[] = { XTAPE,
                 server,
                 NULL };
char *femsg[] = { FEMSG,
                 "-d",
                 server,
                 NULL };
char *udptoipc[] = { udptoipcfile,
                   "-d",
                   server,
                    NULL };
/*
*     Default startup banner
*/
char *banner = {"\
****************************************************************************\n\
*                                ORPHAS                                    *\n\
*                                                                          *\n\
*                               pacman II                                  *\n\
*                      Physics Acquisition Manager                         *\n\
*                                                                          *\n\
****************************************************************************\n\
"};

struct usrcmd {
         char *name;  /* Command name                                  */
         int  stop;   /* Stop VME flag. 1 means stop                   */
         int  init;   /* Initvme flag. 1 means do initvme afer command */
         char *dir;   /* Default directory for this command            */
         char **cmd;  /* Command string. Pointer to array of pointers  */
  };

struct usrcmd usrlst[30] = { NULL,0,0,NULL,NULL };

struct wins {
         char *name;
         char **cmdstr;
volatile pid_t pid;
  };

struct wins windows[10] = { NULL,NULL,0  };

struct aliases {
           char *new;
           char *old;
  };

struct aliases aliaslst[100] = {NULL, NULL};

char exitmess[] = {
"HOW TO EXIT PACMAN .....................................................\n\
\n\
KILL ALL  - STOP all acquisition.  This terminates all acquisition\n\
            processes in the DECStation.  To run acquisition again\n\
            you must run pacman again.  If tape output is running,\n\
            two EOFs are written and then the tape is positioned\n\
            between the two EOFs.\n\
\n\
            WARNING:  This command MUST be upper case!\n\
" };

/*
*    Command list
*/

#define  NOLOG   -1     /* means do not send this command string to logger */
#define    LOG   1      /* Send this command string to logger              */

enum Class {EXEC = 1,EXECARG,TAPE,UNIX,EQU,INT,UNKN,NUL,DUP,USER,DIR,
                     WIN,PAC,VMEINIT,TRUN,CMDF,HUP,SCAT,TON};

/*
*     EXEC    - substitute string in table for user string and execute
*     EXECARG - use string in table with arguments from user string
*     TAPE    - send user string to TAPEOU process
*     UNIX    - execute unix system programs(use command line as is)
*     EQU     - alias command.  Table string is an equivalent command
*     INT     - commands which are handled within this program
*     USER    - USER specific commands
*     UNKN    - Unknown command.  Marks end of command table
*     NUL     - No command on input line
*     DUP     - Means command string occurs two or more times in table
*     DIR     - Special for - dir - command
*     WIN     - Recreate user windows
*     PAC     - run PAC compiler
*     VMEINIT - initialize the VME acquisition system
*     TRUN    - Start acquisition to tape
*     CMDF    - Execute from a command file
*     HUP     - Stopvme followed by startvme.  Flushes data buffers.
*     SCAT    - Scaler dump to tape
*/

struct cmdlst {
            char *cmd;         /* Command string                           */
       enum Class class;       /* Command type                             */
            int   log;         /* logger flag                              */
            char *cmdstr;      /* string to substitute for the users cmd   */
};

struct cmdlst kcmd[100] = {
       "prtdoc",    EXECARG, NOLOG, PRTDOC,
       //       "lt",        EXEC, NOLOG, LT,
       "bootvme",   EXEC, LOG,   BOOTVME,
       "loadacq",   EXEC, LOG,   LOADACQ,
       //       "loaddssd",  EXEC, LOG,   LOADDSSD,
       "startvme",  EXEC, LOG,   STARTVME,
       "stopvme",   EXEC, LOG,   STOPVME,
       "initvme",   VMEINIT, LOG,   INITVME,
       "statvme",   EXEC, NOLOG, STATVME,
       "testvme",   EXEC, LOG,   TESTVME,
       "pacfile",   EXEC, NOLOG, PACFILE,
       "setup",     EXECARG, LOG, SETUP,
       "zeroclk",   EXEC, LOG, ZEROCLK,
       "vmehardware", EXEC, NOLOG, VMEHARDWARE,
       "vmehost",     EXEC, NOLOG, VMEHOST,
       "vmecpus",     EXEC, NOLOG, VMECPUS,
       //       "kt",        EXECARG, NOLOG, KT,
       "cmdf",      CMDF,    NOLOG, NULL,
       "cmd",       EQU,  0,     "cmdf",
       "hup",       HUP,     NOLOG, NULL,
       "pac",       PAC,     LOG, PACCMD,
       "pacc",      PAC,     LOG, PACCMD,
       "pacor",     PAC,     LOG, PACCMD,
       "edit",      EXECARG, LOG,   "/usr/bin/xterm -n EDIT_PAC -geometry\
 80x50+10+10 -sb -e vi",
       "vi",        EQU, 0,      "edit",
       "tstop",     INT, LOG,    (char *)1,
       "cd",        INT, LOG,    (char *)2,
       "kill",      INT, NOLOG,  (char *)3,
       "help",      INT, NOLOG,  (char *)4,
       "newf",      INT, NOLOG,  (char *)5,
       "log",       INT, LOG,    (char *)6,
       "userconfig", INT, NOLOG,  (char *)7,
       "exit",      INT, NOLOG,  (char *)8,
       "zbuf",      INT, NOLOG,  (char *)9,
       "911",       EQU, 0,      "help",
       "h",         EQU, 0,      "help",
       "end",       EQU, 0,      "exit",
       "quit",      EQU, 0,      "exit",
       "ps",        UNIX, NOLOG,  NULL,
       "date",      UNIX, NOLOG,  NULL,
       "time",      EQU,  0,      "date",
       "pwd",       EXEC, NOLOG,  "pwd",
       "dir",       DIR,  NOLOG,  "/usr/acq2/bin/xdir",
       "ls",        EQU, 0,       "dir",
       "ipcs",      UNIX, NOLOG,  NULL,
       "ouf",       TAPE, NOLOG,  NULL,
       "fdir",      TAPE, NOLOG,  NULL,
       "clo",       TAPE, NOLOG,  NULL,
       "ulo",       TAPE, NOLOG,  NULL,
       "rdi",       TAPE, NOLOG,  NULL,
       "rdo",       TAPE, NOLOG,  NULL,
       "fro",       TAPE, NOLOG,  NULL,
       "bro",       TAPE, NOLOG,  NULL,
       "ffo",       TAPE, NOLOG,  NULL,
       "bfo",       TAPE, NOLOG,  NULL,
       "rwo",       TAPE, NOLOG,  NULL,
       "bto",       TAPE, NOLOG,  NULL,
       "htit",      TAPE, NOLOG,  NULL,
       "hnum",      TAPE, NOLOG,  NULL,
       "hout",      TAPE, NOLOG,  NULL,
       "eof",       TAPE, NOLOG,  NULL,
       "pev",       TAPE, NOLOG,  NULL,
       "psav",      TAPE, NOLOG,  NULL,
       "dev",       TAPE, NOLOG,  NULL,
       "pz",        TAPE, NOLOG,  NULL,
       "dz",        TAPE, NOLOG,  NULL,
       "pa",        TAPE, NOLOG,  NULL,
       "da",        TAPE, NOLOG,  NULL,
       "pif",       TAPE, NOLOG,  NULL,
       "dif",       TAPE, NOLOG,  NULL,
       "pi",        TAPE, NOLOG,  NULL,
       "di",        TAPE, NOLOG,  NULL,
       "stx",       TAPE, NOLOG,  NULL,
       "trun",      TRUN, NOLOG,  NULL,
       "tcont",     TAPE, NOLOG,  NULL,
       "ton",       TON,  NOLOG,  NULL,
       "scat",      SCAT, NOLOG,  NULL,
       "afon",      TAPE, NOLOG,  NULL,
       "afof",      TAPE, NOLOG,  NULL,
       "afoff",     EQU,  0,     "afof",
       NULL,        UNKN, 0,      NULL};

/*
*     Local Function Prototypes
*/
int  cmd_exec(char *);
char *cmdfind(char *,enum Class *,char **,int *);
char *getfield(char *,char *,int);
char *keysearch(char *,char *);
char *getusr(char *);
int  spawn(char *argstr[]);
void exec_wait(char *argstr[]);
void buildcmd(char *,char *);
int  tape_cmd(char *,int,int);
void loadchk(void);
void init(void);
void kill_all(void);
void cmdlog(void);
void CLDsignal(int);
void helpman(char *);
void ALRMsignal(int);
void TERMsignal(int);
void INTsignal(int);
void docdate(void);
void config(void);
void fig_banner(char *);
int  fig_window(char *);
int  fig_usrcmd(char *);
int  read_fig(FILE *);
int  add_alias(char *,char *);
int  add_cmd(char *,enum Class ,char *,int );
int  fig_alias(char *);
void usrfig(void);
void displaychk(void);
void log_build(void);

/*
*      Global variables
*/
struct fortinmsg message,prompt;

char Display[32];
char priority[] = PRIORITY;
char helpfile[128] = HELPFILE;
char docfile[] = DOCFILE;
char *configfile = NULL;
char figfilename[128];
int  shm_buffer_size = 32768;
int  log_interval = 300;

char minusn[] = "-n";
char *figptr = NULL;
char **strptr = NULL;
char args[2048];
char *uxargs[40];
char cdstring[80], *cd_ptr = NULL;
char in_line[257];
char lastpac[256],lastinit[256];
int  pacloaded = 0,initvme = 0;

char pacpidfile[80];
volatile pid_t  tapepid,loggerpid,femsgpid,udptoipcpid;
volatile pid_t  unixpid;
volatile   int  unixstat,kill_flag = 0,sigintflg,tape_busy = 0;

/***************************************************************************
*
***************************************************************************/
main(int argc,char *argv[])
{
   char  *cptr,line[81];

/*
*  The argument(if present) is the name of a configuration file
*/
   if (argc == 2) configfile = argv[1];
/*
*   Process the configuration file and build logger startup
*/
   config();
   log_build();
/*
*   Get name of VME processor
*/
   if ((cptr = getenv("VME")) != NULL) {
      strcpy(server,cptr);
      printf("\n Servername is: %s\n",server);
   }
   else
     {
       printf("\n\
 You MUST set the environment variable VME to the name of the processor\n\
 you are using.\n\n");
       return;
     }

/*
*   Check for a copy of pacman or other of our processes already loaded.
*   Only one pacman process will be allowed per attached VME system.
*/
   loadchk();

/*
*   Initialize a signal handler to catch the death of any of our children.
*/
   CLDsignal(0);
/*
*   Initialize message queues, load our processes, output the startup 
*   banner and send a startup message to the logger.
*/
   init();
/*
*   Report date of the PACMAN document
*/
   docdate();
/*
*   Read the help file and list it's directory
*/
   helpman(" ");
/*
*   Finally we get to the main loop
*/
   while(1)
    {
/*
*  Put out a user prompt and wait for a command line
*/
      printf("pacman: ");
      sigintflg = 1;
      while (fgets(line,80,stdin) == (char *)NULL)
       {
         if (errno == EINTR && sigintflg == 0) printf("pacman: ");
         if (!sigintflg) sigintflg = 1;
       }
      line[strlen(line)-1] = '\0';
      cmd_exec(line);
    }
}
/*****************************************************************************
*
*   Execute the command
*****************************************************************************/
int cmd_exec(char *cmdstr)
{
   int  Itime = 1;
   int  Iunit = 2;
   int  Ierror = 0;
   int  i,log,status;
   char *subptr, *cptr, *uxptr, *olddir;
   char line[81];
   enum Class class;
   struct usrcmd *usrptr;
   struct wins *winptr;
   static FILE *cmdfile = NULL;

   strcpy(line,cmdstr);
   cptr = cmdfind(line,&class,&subptr,&log);
/*
*  If the command has an alias, put the alias in place of the user command
*  and search the command table again.
*/
   if (class == EQU) 
     {
       strcpy(args,cptr);
       strcpy(line,subptr);
       strcat(line,args);
       cptr = cmdfind(line,&class,&subptr,&log);
     }
   unixstat = 0;
/*
*   Dispatch on the command type
*/
   switch  (class)
     {
     case  TRUN:
       uxptr = getfield(args,cptr,5);
       for(i=0;i<5;i++)            // Lowercase for tests 
	 args[i]=tolower(args[i]);
       
/*       if (uxptr == NULL)
	 {
	   printf(
		  "\n\7TRUN needs argument: BON (beam on) or BOFF (beam off) - %s -\n",line);
	   log = NOLOG;
	   break;
	 }
       else if (strcmp(args,"bon") && strcmp(args,"boff"))
	 {
	   printf(
		  "\n\7Syntax: TRUN BON (beam on) or TRUN BOFF (beam off) - %s -\n",line);
	   log = NOLOG;
	   break;
	 }
*/
       if (pacloaded != 0 && initvme != 0)
	 {
	   strcpy(lastinit,lastpac);
	   strcpy(line,"psav ");
	   strcat(line,lastinit);
	   if (!tape_cmd(line,0,cmdfile!=NULL))
	     {
	       pacloaded = 0;
	       initvme = 0;
	       strcpy(line,"trun ");
	       strcat(line,args);
	       tape_cmd(line,0,1);
	     }
            break;
	 }
     case  TON:
       uxptr = getfield(args,cptr,5);
       for(i=0;i<5;i++)            /* Lowercase for tests */
	 args[i]=tolower(args[i]);
       
/*       if (uxptr == NULL)
          {
            printf(
		   "\n\7TON needs argument: BON (beam on) or BOFF (beam off) - %s -\n",line);
            log = NOLOG;
            break;
          }
       else if (strcmp(args,"bon") && strcmp(args,"boff"))
	 {
	   printf(
		  "\n\7Syntax: TON BON (beam on) or TON BOFF (beam off) - %s -\n",line);
	   log = NOLOG;
	   break;
	 }
*/
     case  SCAT:
       if (cd_ptr != NULL)
	 {
	   if (!tape_cmd(cdstring,0,cmdfile!=NULL))
	     {
	       cd_ptr = NULL;
	       tape_cmd(line,0,1);
	     }
	   break;
          }
       cmd_exec("stopvme");
       wait_(&Itime,&Iunit,&Ierror);
       zbuf_shm();
     case  TAPE:
       tape_cmd(line,0,cmdfile!=NULL);
       break;
       
     case  EXECARG:
     case  PAC:
/*
*  Translate user command to a string from the command table,
*  append arguments from user input and fork a process to
*  execute the function.
*/
        uxptr = getfield(args,cptr,4);
        if (uxptr == NULL)
          {
            printf("\n\7CMD ERROR - argument Needed! - %s -\n",line);
            log = NOLOG;
            break;
          }
        buildcmd(subptr,cptr);
        exec_wait(uxargs);
        if (unixstat) log = NOLOG;
        else if (class == PAC && uxargs[2] != NULL)
          {
            cptr = uxargs[1];
            if (*cptr != '~' && *cptr != '/')
              {
                if (getcwd(lastpac,sizeof(lastpac)) == NULL) break;
                strcat(lastpac,"/");
              }
            else  lastpac[0] = '\0';
            strcat(lastpac,uxargs[1]);
            strcat(lastpac,".pac");
            pacloaded = 1;
            initvme = 0;
            cmdlog();
            log = NOLOG;
            cmd_exec("stopvme");
            cmd_exec("initvme");
            cmd_exec("zbuf");
          }
        break;
      case  VMEINIT:
        uxptr = getfield(args,cptr,4);
        buildcmd(subptr,cptr);
        exec_wait(uxargs);
        if (unixstat) log = NOLOG;
        initvme = 1;
        break;
/*
*  Same as EXECARG except there are no user arguments.
*/
      case  EXEC:
        buildcmd(subptr,NULL);
        exec_wait(uxargs);
        if (unixstat) log = NOLOG;
        else if (class == VMEINIT) initvme = 1;
        break;
/*
*   Special case for - dir.  It may or may not have an argument.
*/
      case  DIR:
        buildcmd(subptr,cptr);
        exec_wait(uxargs);
        break;
      case  INT:
/*
*   Commands which are handled internally
*/
        uxargs[0] = line;
        uxargs[1] = NULL;
        if (subptr == (char *)1)       /* send a CTRL/C to  TAPEOU */
          {
            cmd_exec("stopvme");
            wait_(&Itime,&Iunit,&Ierror);
            status = kill(tapepid,SIGINT);
            if (status == -1)
              {
                perror("pacman - tape CTRL/C");
              }
            if (tape_busy) tape_busy = 0;
          }
        else if (subptr == (char *)2)  /*  change our directory    */
          {
            uxptr = getfield(args,cptr,80);
            if (uxptr == NULL) 
              {
                uxptr = getenv("HOME");
                status = chdir(uxptr);
                strcpy(args,uxptr);
              }
            else
              {
                if (args[0] == '~') 
                  {
                   cptr = getusr(args);
                   if (cptr == NULL) break;
                  }
               status = chdir(args);
              }
            if (status == -1)
              {
                perror("pacman - chdir");
              }
            else
              {
                strcpy(cdstring,"cd  ");
                strcat(cdstring,args);
                cd_ptr = cdstring;
              }
          }
        else if (subptr == (char *)3)  /* kill command             */
          {
            cptr = getfield(args,cptr,80);
            if(!strcmp(args,"ALL")) kill_all();
            else
              {
                printf("To exit, you must give the command  KILL ALL\n");
                printf("WARNING:  This command must be upper case!\n");
              }
          }
        else if (subptr == (char *)4)  /* help command             */
          {
            getfield(line,cptr,sizeof(line));
            helpman(line);
          }
        else if (subptr == (char *)5)   /* Read new help file */
          {
            helpman("newf");
          }
        else if (subptr == (char *)7)
          {
            usrfig();
          }
        else if (subptr == (char *)8)
          {
            printf("%s",exitmess);
          }
        else if (subptr == (char *)9)   /* quick flush shm_buffers */
          {
            status = zbuf_shm();
            if (status != 0)
              {
                acq_error(&status, line, sizeof(line));
                printf("%s\n",line);
              }
          }
        break;
      case  WIN:
         winptr = (struct wins *)subptr;

#ifdef __ultrix
         if (winptr->pid != 0 && getpgrp(winptr->pid, 0) != -1)
#else
         if (winptr->pid != 0 && kill(winptr->pid, 0) != -1)
#endif

           {
             printf("%s window already exists - Command ignored\n",
                                                             winptr->name);
             break;
           }
         winptr->pid = spawn(winptr->cmdstr);
         break;
      case  UNIX:
        buildcmd(line,NULL);
        exec_wait(uxargs);
        if (unixstat) log = NOLOG;
        break;
      case  USER:
/*
*   These are user defined commands
*/
        usrptr = (struct usrcmd *)subptr;
        if (usrptr->stop == 1)
          {
            if (cmd_exec("stopvme") == 0)
                        printf("WARNING:  VME acquisition has been stopped.\n");
          }
        if (usrptr->dir != NULL)
          {
            olddir = getcwd(NULL,256);
            status = chdir(usrptr->dir);
            if (status == -1)
              {
                perror("USER setup directory error");
                break;
              }
          }
        i = 0;
        while (usrptr->cmd[i] != NULL)
          {
            uxargs[i] = usrptr->cmd[i];
            i++;
          }
/*
*   Add any command line arguments
*/
        uxptr = args;
        while ((cptr = getfield(uxptr,cptr,80)) != NULL)
          {
            uxargs[i++] = uxptr;
            uxptr += strlen(uxptr) + 2;
          }
        uxargs[i] = NULL;
        exec_wait(uxargs);
        if (usrptr->dir != NULL)
          {
            chdir(olddir);
            free(olddir);
          }
        if (unixstat) log = NOLOG;
        else if (usrptr->init == 1)
          {
            cmdlog();
            cmd_exec("initvme");
            printf("          VME acquisition has been initialized.\n");
            log = NOLOG;
          }
        break;
      case  CMDF:
/*
*    Process a command file
*/
        if (cmdfile != NULL)
          {
            printf("\n\7Command file cannot be executed in a command file\n");
            break;
          }
        uxptr = getfield(args,cptr,128);
        if (uxptr == NULL)
          {
            printf("\n\7CMD ERROR - Command file name Needed! - %s -\n",line);
            break;
          }
        if (strstr(args,".") == NULL) strcat(args,".cmd");
        if ((cmdfile = fopen(args,"r")) == (FILE *)NULL)
          {
            printf("Can't open command file - %s\n",args);
            break;
          }
        while (fgets(line,80,cmdfile) != NULL)
         {
           cptr = strstr(line,"\n");
           if (cptr != NULL) *cptr = '\0';
           printf("%s\n",line);
           cmd_exec(line);
         }
        fclose(cmdfile);
        cmdfile = NULL;
        break;
      case  HUP:
/*
*   Histogram update.
*/
        cmd_exec("stopvme");
        cmd_exec("startvme");
        break;
      case  DUP:
        log = NOLOG;
        printf("Duplicate commands in command list - %s\n",line);
        break;
      case  NUL:
        log = NOLOG;
        break;
      case  UNKN:
        log = NOLOG;
        printf("\7Unknown Command - %s -  Type HELP for assistance\n",line);
        break;
      default:
        printf("Pacman: should not have gotten here\n");
        break;
    }
   if (log == LOG) cmdlog();
   return (unixstat);
}
/***************************************************************************
*
*   Search the command table for the command on the input line
*
*  Call:   line   -  pointer the command line
*
*  returns:  class     - command type
*            substring - pointer to string to substitute for user command
*            log       - command log flag
*
*  Function returns a pointer to command line.  This can be used in searches
*  for arguments on the command line.
***************************************************************************/
char *cmdfind(char *line,enum Class *class,char **substring,int *log)
{
#define MAX_CMD_LEN  15

   int  i,matches = 0;
   char *cptr;
   char command[MAX_CMD_LEN];
   struct cmdlst *clist =  kcmd;
   struct cmdlst *cfound =  NULL;

   cptr = getfield(command,line,MAX_CMD_LEN);
   for (i=0; i < MAX_CMD_LEN; i++) command[i] = (char)tolower(command[i]);
   if (cptr == NULL)
     {
       *class = NUL;
       return cptr;
     }
   while(clist->class != UNKN)
     {
      if (!strcmp(command,clist->cmd))
        {
          matches++;
          cfound = clist;
        } 
      clist++;
     }
    *log = 0;
    if (matches == 0) *class = UNKN;
    else if (matches > 1) *class = DUP;
    else
      {
        *class = cfound->class;
        *substring = cfound->cmdstr;
        *log = cfound->log;
      }
    return (cptr);
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
char *getfield(char *s1,char *s2,int n)
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
*
***************************************************************************/
char *keysearch(char *line,char *key)
{
   char *cptr,prechar,postchar;

   if ((cptr = strstr(line,key)) == NULL) return NULL;
   postchar = *(cptr + strlen(key));
   if (postchar != ' ' && postchar != '\t' && postchar != '\0'
                                           && postchar != '\n') return NULL;
   if (cptr == line) return cptr;
   prechar = *(cptr-1);
   if (prechar != ' ' && prechar != '\t') return NULL;
   return cptr;
}
/*****************************************************************************
*****************************************************************************/
char *getusr(char *file)
{
   static char   fsav[80];
   char          *cptr;
   struct passwd *pw;

   if (file[0] == '~')
     {
       if ((cptr = strchr(file,'/')) != NULL)
         {
           strcpy(fsav,cptr);
           *cptr = 0;
         }
       else  fsav[0] = 0;
       if ((pw = getpwnam(file+1)) == NULL)
         {
           printf("\7 No such user: %s\n",file+1);
           return (NULL);
         }
       strcpy(file,pw->pw_dir);
       strcat(file,fsav);
       endpwent();
     }
   return(file);
}
/***************************************************************************
*
*   Create a process which runs concurrently with pacman.  The new process
*   is made a process group leader to disassociate it from our control
*   terminal.
***************************************************************************/
int spawn(char *argstr[])
{
   int   pid;

   if ((pid = vfork()) == 0)
     {

#ifdef __ultrix
       setpgrp(0,getpid());
#else
       setpgid(0,getpid());
#endif

       execvp(argstr[0],argstr);
       perror(argstr[0]);
       _exit(0);
     }
   else if (pid == -1)
     {
       printf("pacman - Error forking: %s\n",argstr[0]);
     }
   return(pid);
}
/***************************************************************************
*
*   fork and execute a process.  We wait until the process terminates
*   before return to caller.
***************************************************************************/
void exec_wait(char *argstr[])
{
   int   pid,mask;

   mask = sigblock(sigmask(SIGCLD));
   if ((pid = fork()) == 0)
     {
       execvp(argstr[0],argstr);
       perror(argstr[0]);
       _exit(0);
     }
   else if (pid == -1)
     {
       printf("pacman - Error forking: %s\n",argstr[0]);
     }
   else  while(pid != unixpid) sigpause(mask);
   sigsetmask(mask);
}
/***************************************************************************
*    Build an argument array to be used in forking a new process.  The list
*    can be built from up to two input strings.  Usually the first string
*    is a command substitution string and arguments from the command line.
*
*  call:  s1  - pointer the first string
*         s2  - pointer to second string
*
***************************************************************************/
void  buildcmd(char *s1, char *s2)
{
   int  i = 0, j = 0;
   char *uxptr;

   uxptr = args;
   while (uxptr-args < 900)
     {
       uxargs[i] = uxptr;
       if ((s1 = getfield(uxptr,s1,80)) == NULL)
         {
           if (j == 0 && s2 != NULL)
             {
               j++;
               s1 = s2;
             }
           else
             {
               uxargs[i] = NULL;
               break;
             }
         }
       else
         {
           uxargs[i++] = uxptr;
           uxptr += strlen(uxptr) + 2;
         }
     }
}
/***************************************************************************
*   Put a command into the TAPEOU message queue.
***************************************************************************/
int tape_cmd(char *buf,int nowait,int incmdfile)
{
    int len,status,wmask;
    static int msg_sent=0;

    if (!nowait)
     {
       sigintflg = 1;
/*
*   Get prompt message from TAPEOU process
*/
       wmask = (incmdfile)? 0 : msg_sent;
       do
         {
           status = msgrcv(Ids.tape_msg,&prompt, FTN_SIZE, 1L, wmask);
         }
       while(status == -1 && errno == EINTR && sigintflg != 0);
       if (status == -1)
         {
           printf("\7Tape Command - %s - WAS IGNORED !!\7\n",buf);
           if (msg_sent)
              printf("FILEOU Busy with Command - %s\n",message.text);
           return (-1);
         }
      }
/*
*   Just send input line to TAPEOU
*/
    strcpy(message.text,buf);
    len = strlen(message.text);
    if (len > FTN_SIZE) len = FTN_SIZE;
    message.type = 2;
    msgsnd(Ids.tape_msg,&message,(size_t)len,IPC_NOWAIT);
    msg_sent = IPC_NOWAIT;
    if ( strcmp(message.text,"trun")
               && strcmp(message.text,"tcont")
                    && strcmp(message.text,"ton")) tape_busy = 1;
    else  tape_busy = 0;
    return (0);
}
/***************************************************************************
*   Check what processes are loaded.
***************************************************************************/
void loadchk(void)
{
   int  fd,i,inuse,numpacman = 0,status = 0,pid;
   FILE *infile;
   char *cptr;
   char tmpfilename[L_tmpnam],pidstring[10],ttystr[6],ourtty[6];

   static char *ps[] = {"/bin/ps",
                             "-A",
                              "-o",
                              "pid,tty,cmd",
                              NULL };

/*
*   Run ps -ag on ULTRIX or ps -U on OSF to get processes running
*/
   strcpy(tmpfilename,P_tmpdir);
   strcat(tmpfilename,"/PAC1XXXXXX");
   fd = mkstemp(tmpfilename);

   if ((pid = fork()) == 0)
    {
      freopen(tmpfilename,"w",stdout);
      execvp(ps[0],ps);
      perror(ps[0]);
      _exit(0);
    }
   else if (pid <= 0) {printf("pacman loadchk fork failure \n"); exit(99);}
   while(wait(&status) != pid);
   close(fd);
/*
*   Open the input file
*/
   if ((infile = fopen(tmpfilename,"r")) == (FILE *)NULL)
     {
       fprintf(stdout,"pacman loadchk - Cannot open input file\n");
       exit(2);
     }
/*
*   First we search the list of processes for our pacman process.  Extract
*   the TTY for our process.  This will be used in the check for existence
*   of our Windows.
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {

      cptr = getfield(pidstring,in_line,sizeof(pidstring));
      if (sscanf(pidstring,"%i",&pid) == 0) continue;
      getfield(ttystr,cptr,sizeof(ttystr));
      if (strstr(in_line,"pacman")  != NULL)
        {
          if (pid == getpid()) strcpy(ourtty,ttystr);
          numpacman++;
        }
    }

   rewind(infile);
/*
*   Now check for existence of processes already using the IPC resources
*   we would like to use.  Also make a list of existing Windows associated
*   with our TTY.
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      cptr = getfield(pidstring,in_line,sizeof(pidstring));
      if (sscanf(pidstring,"%i",&pid) == 0) continue;
      getfield(ttystr,cptr,sizeof(ttystr));
      if (strstr(in_line,*udptoipc)  != NULL)
        {
          if (keysearch(in_line,server) != NULL)
            {
              udptoipcpid = pid;
              printf("%s",in_line);
            }
        }
      else if (strstr(in_line,*femsg)  != NULL)
        {
          if (keysearch(in_line,server) != NULL)
            {
              femsgpid = pid;
              printf("%s",in_line);
            }
        }
      else if (strstr(in_line,*tape)  != NULL)
        {
          if (keysearch(in_line,server) != NULL)
            {
              tapepid = pid;
              printf("%s",in_line);
            }
        }
      else if (strstr(in_line,LOGGER)  != NULL)
        {
          if (keysearch(in_line,server) != NULL)
            {
              loggerpid = pid;
              printf("%s",in_line);
            }
        }
      else
        {
          i = 0;
          while(windows[i].name != NULL)
            {
              if (keysearch(in_line,windows[i].name) != NULL)
                {
#ifdef  __ultrix
                  if (numpacman == 1 || !strcmp(ttystr,ourtty))
                                                         windows[i].pid = pid;
#else
                  if (numpacman == 1 || !strcmp(ttystr,ourtty)
                      || !strcmp(ttystr,"??")) windows[i].pid = pid;
#endif
                }
              i++;
            }
        }
    }

/***********
printf("femsg = %i, tape = %i, log = %i\n",femsgpid,tapepid,loggerpid);
printf("udptoipc = %i, scan = %i\n",udptoipcpid,scanpid);
***********/

  pid = tapepid + udptoipcpid + loggerpid + femsgpid;
  if (pid)
    {
      printf("\7\nOne or more of pacman's processes are already running\n");
      fclose(infile);
      unlink(tmpfilename);
      exit(EXIT_FAILURE);
    }
/*
*   Now check for existence of processes which may be using our VME
*   processor or IPC resources assigned to it.
*/
   rewind(infile);
   inuse = 0;
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      cptr = getfield(pidstring,in_line,sizeof(pidstring));
      if (sscanf(pidstring,"%i",&pid) == 0) continue;
      if (keysearch(in_line,server) != NULL)
        {
          inuse = 1;
          printf("%s",in_line);
        }
    }
   if (inuse)
     {
       printf("\7\nWARNING: Above process(s) may be using resources pacman \
needs!!\n");
     }
   fclose(infile);
   unlink(tmpfilename);
}
/***************************************************************************
*   Create message queues for the logger process and for input to the
*   TAPEOU process.  Load the process necessary for data acquisition.
***************************************************************************/
void init(void)
{
    extern struct shm_use *Shm;
    int  i,window,ierr;
    char *cwd;
    static char pid[10];
    static char pri_udptoipc[] = "-15";
    static char pri_tape[] = "-10";
    static char mess[78];
    FILE *pidfile;

/*
*   Open our IPC resources
*/
   create_acq_ipc_(server,&shm_buffer_size,&ierr,sizeof(server));
   if (ierr != 0)
     {
       acq_error(&ierr,mess,sizeof(mess));
       printf("Processor %s:  %s\n",server,mess);
       exit(99);
     }
   open_acq_ipc_(server,&shm_buffer_size,&ierr,sizeof(server));
   if (ierr != 0)
     {
       acq_error(&ierr,mess,sizeof(mess));
       printf("Processor %s:  %s\n",server,mess);
       exit(99);
     }
   Shm->log_interval = log_interval;
/*
*   Setup to ignore most signals but catch a few
*/
   INTsignal(0);
   signal(SIGTERM, TERMsignal);
   signal(SIGTSTP, SIG_IGN);
   signal(SIGCONT, SIG_IGN);
   signal(SIGUSR1, SIG_IGN);
   signal(SIGUSR2, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);
   signal(SIGSTOP, SIG_IGN);
   signal(SIGVTALRM, SIG_IGN);
   signal(SIGKILL, SIG_IGN);
   signal(SIGPIPE, SIG_IGN);
   signal(SIGPROF, SIG_IGN);

   if (banner != NULL) printf("%s",banner);
/*
*   Log startup, current working directory, configuration file and
*   udptoipc process to be used
*/
   uxargs[0] = args;
   uxargs[1] = NULL;
   strcpy(args,"*****  pacman Started on ");
   i = strlen(args);
   gethostname(&args[strlen(args)],36);
   args[i+36] = '\0';
   strcat(args," *****");
   cmdlog();
   if ((cwd = getcwd((char *)NULL,92)) != NULL)
     {
       strcpy(args,"**** Current Directory: ");
       strcat(args,cwd);
       cmdlog();
       free(cwd);
     }
   strcpy(args,"**** Configuration File: ");
   strcat(args,figfilename);
   cmdlog();
   strcpy(args,"**** UDPtoipc Process: ");
   strcat(args,udptoipcfile);
   cmdlog();
/*
*  Start the udptoipc process and the tape process
*/
   udptoipcpid = spawn(udptoipc);
   tapepid = spawn(tape);
/*
*  Check for Display for this process
*/
   if (getenv("DISPLAY") != NULL) window = 1;
   else  window = 0;

/*
*  If we have a display start the regular logger process.  Otherwise,
*  start the alternate logger process.
*/
   if (window == 1) loggerpid = spawn(logger);
   else
     {
       if ((loggerpid = fork()) == 0)
         {
           execvp(loggeralt[0],loggeralt);
           _exit(0);
         }
       else if (loggerpid == -1) perror("fork failure - loggeralt -");
     }
/*
*  Start the front end message receive process
*/
   femsgpid = spawn(femsg);
   sleep(3);
/*
*   Set priority of the udptoipc process and the TAPEOU process
*/
   uxargs[0] = priority;
   uxargs[1] = pid;
   uxargs[2] = pri_udptoipc;
   uxargs[3] = NULL;
   sprintf(pid,"%i",udptoipcpid);
   exec_wait(uxargs);
   uxargs[2] = pri_tape;
   sprintf(pid,"%i",tapepid);
   exec_wait(uxargs);
/*
*   Start any user defined windows provided there is a Display
*/
   i = 0;
   while(windows[i].name != NULL)
     {
       if(windows[i].pid == 0 && window != 0)
                                    windows[i].pid = spawn(windows[i].cmdstr);
       i++;
     }
/*
*   Make a file in the users Home directory which records the tape
*   process ID, the tape message queue ID and the logger message queue ID.
*
*   All of this is so that other processes can control the tape if needed.
*/
   strcpy(pacpidfile,getenv("HOME"));
   strcat(pacpidfile,"/.pacpid.");
   strcat(pacpidfile,server);
   if ((pidfile = fopen(pacpidfile,"w")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open pacpid file - %s\n",pacpidfile);
       return;
     }
   fprintf(pidfile,"Tape_PID  %d\n",tapepid);
   fprintf(pidfile,"Tape_Message_ID  %d\n",Ids.tape_msg);
   fprintf(pidfile,"Log_Message_ID  %d\n",Ids.log_msg);
   fprintf(pidfile,"UDPtoipc_PID  %d\n",udptoipcpid);
   fclose(pidfile);
}
/***************************************************************************
***************************************************************************/
void kill_all(void)
{
   int  pid,status;

   kill_flag = 1;
   signal(SIGCLD, SIG_DFL);
   ALRMsignal(0);
   kill(tapepid,SIGINT);               /* send a CTRL/C to TAPEOU   */
   tape_cmd("end",1,0);                /* Also send it an 'end' cmd */

   if (femsgpid != 0) kill(femsgpid,SIGTERM);
   if (udptoipcpid != 0) kill(udptoipcpid,SIGTERM);
   while(femsgpid+tapepid+udptoipcpid)
    {
      if ((pid = wait(&status)) == -1 && errno != EINTR) break;
      if (pid == femsgpid)
        {
          femsgpid = 0;
          printf("femsg process Terminated\n");
        }
      else if (pid == tapepid)
        {
          tapepid = 0;
          printf("FILEOU process Terminated\n");
        }
      else if (pid == udptoipcpid)
        {
          udptoipcpid = 0;
          printf("%s process Terminated\n",udptoipcfile);
        }
    }
   remove_acq_ipc_(server,&status,sizeof(server));
   unlink(pacpidfile);
   exit(EXIT_SUCCESS);
}
/***************************************************************************
*  Send message to logger queue.  Commands which request logging call
*  this routine.  Startup message is sent by faking a command string.
***************************************************************************/
void  cmdlog(void)
{
   int    i,len;
   time_t tod;
   static struct orphmsg logmess;

   logmess.type = 1;
   strcpy(logmess.text,"pacman    ");

   time(&tod);
   strftime(&logmess.text[MSG_TIME_COL-1],21,"%d-%b-%y %H:%M:%S  ",
                                                              localtime(&tod));

   logmess.text[MSG_USER_COL-1] = '\0';
   i = 0;
   for(; uxargs[i] != NULL; i++)
     {
       len = strlen(&logmess.text[MSG_USER_COL-1]);
       if (ORPHAS_SIZE - MSG_USER_COL - len > strlen(uxargs[i]))
         {
           strcat(&logmess.text[MSG_USER_COL-1],uxargs[i]);
           strcat(&logmess.text[MSG_USER_COL-1]," ");
         }
       else
         {
           len += MSG_SEND_LEN+MSG_TIME_LEN +3 +1;
           msgsnd(Ids.log_msg,&logmess,(size_t)len,IPC_NOWAIT);
           strcpy(&logmess.text[MSG_USER_COL-1],".... ");
           --i;
         }
     }
   len = strlen(&logmess.text[MSG_USER_COL-1]);
   if (len > 0)
     {
       len += MSG_SEND_LEN+MSG_TIME_LEN +3 +1;
       msgsnd(Ids.log_msg,&logmess,(size_t)len,IPC_NOWAIT);
     }
}
/***************************************************************************
*   Routine to catch signal SIGCLD.  Most of these signals are from the
*   death of temporary we forked to do user commands.  However, we
*   also monitor our permanent processes.  If any of these die, warn
*   the users that ACTION on his/her part is REQUIRED!
*
*   Calling this routine with sig equal to zero, initializes the
*   signal handler.
***************************************************************************/
void  CLDsignal(int sig)
{
  int   i,pid,status;
  char *process = NULL;
  static char *prog[3] = {"TAPEOU","LOGGER","femsg"};

  if (sig != 0)
    {
/*
*   If we get here, it is a real signal
*/
      pid = wait(&status);
/**********
     printf("PID %i terminated\n",pid);
**********/
      if (pid == tapepid)
        {
          process = prog[0];
          tapepid = 0;
        }
      else if (pid == loggerpid)
        {
          process = prog[1];
          loggerpid = 0;
        }
      else if (pid == femsgpid)
        {
          process = prog[2];
          femsgpid = 0;
        }
      else if (pid == udptoipcpid)
        {
          process = udptoipcfile;
          udptoipcpid = 0;
        }
      else
        {
          i = 0;
          while(windows[i].name != NULL)
            {
              if(windows[i].pid == pid)
                {
                  windows[i].pid = 0;
                  break;
                }
              i++;
            }
          if (windows[i].name == NULL)
            {
              unixpid = pid;
              unixstat = (status >> 8) & 0xff;
            }
        }
      if (process != NULL)
        {
          printf("\n\7Operator Action required!!\7\n");
          printf("Process - %s - has TERMINATED\n",process);
          printf("\7Stop (KILL ALL) and run pacman again\7\n");
        }
    }
  else
    {
/*
*   Just a call to initialize the signal handler
*/
      unixpid = 0;
      unixstat = 0;
    }
  signal(SIGCLD,CLDsignal);
  return;   
}
/***************************************************************************
*   Help manager.  This routine uses the Milner style help file.  On the
*   first call, the helpfile is read and stored and searched for the 
*   help requested.
*
*   Call:  item  - requested help topic.  If we can't find this topic,
*                  list the directory of topics.
***************************************************************************/
void helpman(char *item)
{
   int  i,j,len,new = 0;
   char *cptr,*end;
   char lookup[16];
   FILE *infile;
   static char *helpdir[50];
   static char helpages[16384];
   static char *page_end = NULL;
   char line[80];

   cptr = helpages;
   len = strlen(item);
   if (len > 4) len = 4;
   for (i=0; i < len; i++) lookup[i] = (char)toupper(item[i]);
   for (; i < 4;  i++) lookup[i] = ' ';
   if (!strncmp(lookup,"NEWF",4))
     {
       page_end = NULL;
       new = 1;
     }
   if (page_end == NULL)
     {
/*
*   Open the input file
*/
      if ((infile = fopen(helpfile,"r")) == (FILE *)NULL)
        {
          fprintf(stderr,"Cannot open help file - %s\n",helpfile);
          return;
        }

      i = 0;
      while (fgets(line,80,infile) != NULL)
       {
         if (!strncmp(line,"$$$$",4))
           {
             cptr++;
             helpdir[i++] = cptr;
             helpdir[i] = NULL;
             for(j=4; j < 8; j++) line[j] = (char)toupper(line[j]);
             strcpy(cptr,&line[4]);
             cptr += strlen(cptr);
           }
         else
           {
             if (line[0] != ' ') cptr++;
             strcpy(cptr,line);
             cptr += strlen(cptr);
           }
       }
      page_end = cptr++;
      fclose(infile);
      if (new) return;
     }
   j = 0;
   for(i=0; helpdir[i] != NULL; i++)
     {
       if (!strncmp(lookup,helpdir[i],4)) break;
     }
   if (helpdir[i] != NULL && strlen(item) <= 4)
     {
       cptr = helpdir[i] + strlen(helpdir[i]) +1;
       end = helpdir[i+1];
       if (end == NULL) end = page_end;
       for (; cptr < end; cptr += strlen(cptr)+1)
         {
           printf("%s",cptr);
           j++;
         }
     }
   else if ((len = strlen(item)) != 0)
     {
       if (len >= sizeof(lookup)) len = sizeof(lookup) - 1;
       for (i=0; i < len; i++) lookup[i] = (char)toupper(item[i]);
       lookup[i] = '\0';
       cptr = helpages;
       end = page_end;
       for(; cptr < end; cptr += strlen(cptr)+1)
         {
           getfield(line,cptr,sizeof(line));
           if (strncmp(line,lookup,len) == 0)
             {
               printf("%s",cptr);
               j++;
             }
         }
      }
    if (j == 0)
      {
        for(i=0; helpdir[i] != NULL; i++)
          {
            printf("Type: H %s",helpdir[i]);
          }
      }
   
}
/****************************************************************************
*
*    Interval timer routine.
***************************************************************************/
void ALRMsignal(int sig)
{
   int status;
   static times = 0;
   static struct itimerval setval;          /* used to set realtime */
   static char exmsg[] = {
"******************************* WARNING *********************************\n\
*  One of more of pacman's processes has not terminated!!\7\7               *\n\
*                                                                       *\n\
*  pacman may refuse to run the next time you try.  If that happens,    *\n\
*  follow the instructions in Section 140 of the PACMAN documentation.  *\n\
*                                                                       *\n\
******************************* WARNING *********************************\n"};

   if (sig)
     {
       if (times > 0)
         {
           printf("%s",exmsg);
           remove_acq_ipc_(server,&status,sizeof(server));
           unlink(pacpidfile);
           exit(EXIT_FAILURE);
         }
       if (times%2 == 0) printf("Waiting for processes to terminate!\n");
       times++;
     }
/*
*   Reset the interval timer and rearm timer signal
*/
   setval.it_value.tv_sec=15;
   setval.it_value.tv_usec=0;
   status=setitimer(ITIMER_REAL, &setval, (struct itimerval *)NULL);
   if (status != 0)
    {
      perror("Setting interval timer");
    }
   signal(SIGALRM,ALRMsignal);
   return;
}
/****************************************************************************
***************************************************************************/
void TERMsignal(int sig)
{
   int  status;

   if (kill_flag == 0) kill_all();
   remove_acq_ipc_(server,&status,sizeof(server));
   unlink(pacpidfile);
   exit(EXIT_FAILURE);
}
/****************************************************************************
***************************************************************************/
void INTsignal(int sig)
{
  if (sig == 0) sigintflg = 1;
  else 
    {
      sigintflg = 0;
      printf("\n");
      if (tape_busy)
        {
          kill(tapepid,SIGINT);
          tape_busy = 0;
        }
    }
  signal(SIGINT,INTsignal);
}
/***************************************************************************
*   Get date of the PACMAN documentation.
***************************************************************************/
void docdate(void)
{
   struct stat statbuf;
   int  infile;
/*
*   Open the input file
*/
   if ((infile = open(docfile,0,O_RDONLY)) <= -1)
     {
       fprintf(stdout,"Cannot open file - %s\n",docfile);
       return;
     }
   fstat(infile,&statbuf);
   printf("\nThe latest PACMAN documentation is dated: %s",
                                                    ctime(&statbuf.st_mtime));
   printf("If your copy is older, discard it and print a new copy using the\n\
command -  prtdoc.\n\n");
   close(infile);
}
/***************************************************************************
*
*  Routine processes the configuration file.  The configuration file name
*  may be specified on the command line.  If the no file name supplied on
*  the command line, look for the file 'pacman.fig' in the users home
*  directory.  If no other file can be used, use the default configuration
*  file - /usr/acq/wks/pacman.fig.
***************************************************************************/
void config(void)
{
   int   accept,i,ierr=0;
   char  *cptr,*fptr;
   FILE *figfile, *figtmp;
   char tmpfilename[L_tmpnam],tmpstr[80];
   static char *keys[] = {"$BANNER","$WINDOW","$USER_CMD","$ALIAS",
                          "$BUFFER_SIZE","$LOG_INTERVAL","$HELP_FILE",
                          "$USER_PFTOIPC",NULL};

/*
*   First choice is file specified on the command line
*/
   if (configfile != NULL)
     {
       strcpy(figfilename,configfile);
       if ((figfile = fopen(figfilename,"r")) == NULL)
         {
           printf("FIG ERROR: Configuration file %s not found!\n",figfilename);
           exit(99);
         }
     }
   else
     {
/*
*   Otherwise, look for pacman.fig in the user's home directory
*/
       strcpy(figfilename,getenv("HOME"));
       strcat(figfilename,"/pacman.fig");
       figfile = fopen(figfilename,"r");
       if (figfile == NULL)
         {
/*
*   When all else fails, use the default configuration file
*/
           strcpy(figfilename,FIGFILE);
           if ((figfile = fopen(figfilename,"r")) == NULL)
             {
               printf("FIG ERROR: No configuration file found!\n");
               exit(99);
             }
         }
     }
   printf("Using configuration file: %s\n",figfilename);
   displaychk();
   strcpy(tmpfilename,P_tmpdir);
   strcat(tmpfilename,"/PAC3XXXXXX");
   close(mkstemp(tmpfilename));

   figtmp = fopen(tmpfilename,"w+");
   while (fgets(in_line,sizeof(in_line),figfile) != NULL)
    {
      cptr = getfield(tmpstr,in_line,sizeof(tmpstr));
      if (strcmp(tmpstr,"#if") == 0)
        {
          accept = 0;
          while ((cptr = getfield(tmpstr,cptr,sizeof(tmpstr))) != NULL)
           {
             if (strcmp(tmpstr,Display) == 0)
               {
                 accept = 1;
                 break;
               }
           }
          while ((fptr = fgets(in_line,sizeof(in_line),figfile)) != NULL)
            {
              getfield(tmpstr,in_line,sizeof(tmpstr));
              if (strcmp(tmpstr,"#endif") == 0) break;
              if (strcmp(tmpstr,"#if") == 0)
                {
                  fptr = NULL;
                  break;
                }
              if (accept != 0) fwrite(in_line,1,strlen(in_line),figtmp);
            }
          if (fptr == NULL)
            {
              printf("Syntax error in configuration file\n");
              fclose(figfile);
              fclose(figtmp);
              unlink(tmpfilename);
              exit(99);
            }
        }
      else  fwrite(in_line,1,strlen(in_line),figtmp);
    }
   fclose(figfile);
   rewind(figtmp);
/*
*   Allocate memory for user specifications
*/
   figptr = malloc(32768);
   strptr = (char **)malloc(1000);
   windows[0].name = NULL;
   windows[0].cmdstr = NULL;
   windows[0].pid = 0;
   usrlst[0].name = NULL;
   usrlst[0].cmd = NULL;
   usrlst[0].dir = NULL;
   usrlst[0].stop = 0;
   usrlst[0].init = 0;
   aliaslst[0].new = NULL;
   aliaslst[0].new = NULL;

/*
*   Read and process the configuration file.
*/
   while (read_fig(figtmp) != 0)
     {
       if ((cptr = getfield(in_line,args,15)) == NULL) continue;
       for (i=0; keys[i] != NULL; i++) if (!strcmp(in_line,keys[i])) break;
       switch (i)
        {
/*
*   Banner specification
*/
          case 0:
           fig_banner(cptr);
           break;
          case 1:
/*
*   User defined windows
*/
           if (fig_window(cptr)) ierr = 1;
           break;
          case 2:
/*
*  User define commands
*/
           if (fig_usrcmd(cptr)) ierr = 1;
           break;
          case 3:
/*
*  Command alias
*/
           if (fig_alias(cptr)) ierr = 1;
           break;
          case 4:
/*
*   Shared memory buffer size
*/
           if (sscanf(cptr,"%i",&shm_buffer_size) != 1)
             {
               printf("FIG ERROR: missing argument\n***%s",args);
               ierr = 1;
             }
           break;
          case 5:
/*
*   Log interval
*/
           if (sscanf(cptr,"%i",&log_interval) != 1)
             {
               printf("FIG ERROR: missing argument\n***%s",args);
               ierr = 1;
             }
           break;
          case 6:
/*
*   Help file specification
*/
            cptr = getfield(args,cptr,sizeof(helpfile));
            if (cptr != NULL) strcpy(helpfile,args);
            else
              {
                printf("FIG ERROR: missing filename\n***%s",args);
                ierr = 1;
              }
           break;
          case 7:
/*
*   User specified "udptoipc" process
*/
            cptr = getfield(args,cptr,sizeof(udptoipcfile));
            if (cptr != NULL) strcpy(udptoipcfile,args);
            else
              {
                printf("FIG ERROR: missing filename\n***%s",args);
                ierr = 1;
              }
           break;
          default:
           printf ("FIG ERROR: Unknown command!\n***%s",args);
           ierr = 1;
        }
      if (ierr != 0) break;
     }
   fclose(figtmp);
   unlink(tmpfilename);
   if (ierr != 0) exit(99);
}
/***************************************************************************
*
*   Handle the $BANNER command in the configuration  file.
***************************************************************************/
void fig_banner(char *s1)
{
   char *cptr;

   cptr = s1;
   while(*cptr == ' ' || *cptr == '\n') cptr++;
   strcpy(figptr,cptr);
   banner = figptr;
   figptr = figptr + strlen(figptr) + 1;
}
/***************************************************************************
*
*  Process the windows specified by $WINDOW commands
***************************************************************************/
int  fig_window(char *s1)
{
   int  field,j;
   char *cptr,*tcptr,*cmdname;
   static int i = 0;

   cptr = s1;
   while((cptr = strstr(cptr,"\n")) != NULL) *cptr = ' ';
   cptr = strstr(s1,"=");
   if (cptr == NULL)
     {
       printf("Syntax error in Window Specification!\n%s\n",args);
       return (-1);
     }
    *cptr = ' ';
    cptr = s1;
    field = 0;
    while ((cptr = getfield(figptr,cptr,128)) != NULL)
      {
        if (field == 0)
          {
            cmdname = figptr;
            figptr += strlen(figptr) + 1;
            windows[i].name = figptr;
            strcpy(figptr,cmdname);
            tcptr = figptr;
            figptr += strlen(figptr) + 1;
            while(*tcptr) *tcptr = toupper(*tcptr), tcptr++;
         }
        else
         {
           *strptr = figptr;
           figptr += strlen(figptr) + 1;
           if (field == 1)
            {
              windows[i].cmdstr = strptr;
              strptr++;
              *strptr = minusn;
              strptr++;
              *strptr = windows[i].name;
            }
           strptr++;
           *strptr = NULL;
         }
        field++;
      }
/*
*    Add command to recreate this window to the pacman command list.
*/
    strptr++;
    if ((j = add_cmd(cmdname,WIN,(char *)&windows[i],NOLOG)) == 0) i++;
    windows[i].name = NULL;
    windows[i].cmdstr = NULL;
    windows[i].pid = 0;
    return j;
}
/***************************************************************************
*
*   Process user defined commands specified the $USER_CMD
***************************************************************************/
int  fig_usrcmd(char *s1)
{
   int  done = 0,j;
   char *cmdname,*cptr,*s2;
   static int  i=0;
   static char *usr_keys[] = {"$cd","$stopvme","$initvme",NULL};

   cptr = s1;
   while ((cptr = strstr(cptr,"\n")) != NULL) *cptr = ' ';
   cptr = s1;
   while ((cptr = strstr(cptr,"=")) != NULL) *cptr = ' ';
   cptr = s1;
   while ((cptr = getfield(figptr,cptr,128)) != NULL)
     {
       if ((s2 = strstr(figptr,";")) != NULL) *s2 = '\0';
       for (j=0; usr_keys[j] != NULL; j++)
                                         if (!strcmp(figptr,usr_keys[j])) break;
       switch (j)
        {
/*
*   $cd - directory
*/
          case  0:
            cptr = getfield(figptr,cptr,128);
            if ((s2 = strstr(figptr,";")) != NULL) *s2 = '\0';
            usrlst[i].dir = figptr;
            figptr = figptr + strlen(figptr) + 1;
            break;
/*
*   $stopvme
*/
          case  1:
            usrlst[i].stop = 1;
            break;
/*
*   $initvme
*/
          case  2:
            usrlst[i].init = 1;
            break;
/*
*   User command
*/
          default:
            cmdname = figptr;
            figptr = figptr + strlen(figptr) + 1;
            usrlst[i].cmd = strptr;
            *strptr = NULL;
            while ((cptr = getfield(figptr,cptr,128)) != NULL)
             {
               if (*figptr == ';') break;
               if ((s2 = strstr(figptr,";")) != NULL) 
                 {
                   *s2 = '\0';
                   done = 1;
                 }
               *strptr = figptr;
               strptr++;
               *strptr = NULL;
               figptr = figptr + strlen(figptr) + 1;
               if (done) break;
             }
        }
      if (cptr == NULL) break;
     }
/*
*   Add new command to the pacman command list.
*/
   strptr++;
   if ((j = add_cmd(cmdname,USER,(char *)&usrlst[i],LOG)) == 0)
    {
      usrlst[i].name = cmdname;
      i++;
    }
   usrlst[i].name = NULL;
   usrlst[i].cmd = NULL;
   usrlst[i].dir = NULL;
   usrlst[i].stop = 0;
   usrlst[i].init = 0;
   return j;
}
/***************************************************************************
*
*   Read one complete command from the configuration file.  If the command
*   uses multiple lines, it must be enclosed in curly braces.
***************************************************************************/
int read_fig(FILE *figfile)
{
   int  i=0,j,maxc = sizeof(args);
   int  curly = 0;
   char *cptr;

   args[0] = '\0';
   while (fgets(in_line,sizeof(in_line),figfile) != NULL)
     {
       while((cptr = strstr(in_line,"{")) != NULL)
         {
           curly++;
           *cptr = ' ';
         }
       while((cptr = strstr(in_line,"}")) != NULL)
         {
           curly--;
           *cptr = ' ';
         }
       j = strlen(in_line);
       maxc -= j;
       if (maxc > 0) strcpy(&args[i],in_line), i += j;
       if (curly == 0) break;
     }
/*
printf("%s",args);
*/
   return i;
}
/***************************************************************************
*
*   Add an alias to the pacman command list
***************************************************************************/
int add_alias(char *new,char *old)
{
    int  j,log;
    char *subptr;
    enum Class class;

    cmdfind(old,&class,&subptr,&log);
    switch  (class)
     {
       case  UNKN:
         printf("ALIAS ERROR: Unknown command - %s -\n",old);
         return (-1);
         break;
       case  EQU:
         old = subptr;
         break;
       default:
         break;
     }
    cmdfind(new,&class,&subptr,&log);
    if (class != UNKN)
      {
        printf("ALIAS ERROR: Duplicate command - %s -\n",new);
        return (-1);
      }
    for (j=0; j < sizeof(kcmd); j++)
     {
       if (kcmd[j].cmd == NULL)
        {
          kcmd[j].cmd = new;
          kcmd[j].class = EQU;
          kcmd[j].log = NOLOG;
          kcmd[j].cmdstr = old;
          j++;
          kcmd[j].cmd = NULL;
          kcmd[j].class = UNKN;
          kcmd[j].cmdstr = NULL;
          break;
        }
     }
   return (0);
}
/***************************************************************************
*
*  Add a new command to the pacman command list.
***************************************************************************/
int add_cmd(char *cmd,enum Class class,char *subptr,int log)
{
   int  j,xlog;
   enum Class xclass;
   char *cptr,*xsubptr;

   cptr = cmd;
   while(*cptr) *cptr = tolower(*cptr), cptr++;
   cmdfind(cmd,&xclass,&xsubptr,&xlog);
   if (xclass == UNKN)
    {
      for (j=0; j < sizeof(kcmd); j++)
       {
         if (kcmd[j].cmd == NULL)
          {
            kcmd[j].cmd = cmd;
            kcmd[j].class = class;
            kcmd[j].log = log;
            kcmd[j].cmdstr = subptr;
            j++;
            kcmd[j].cmd = NULL;
            kcmd[j].class = UNKN;
            kcmd[j].cmdstr = NULL;
            return (0);
          }
       }
    }
   printf("FIG ERROR: Duplicate command - %s -\n",cmd);
   return (-1);
}
/***************************************************************************
*
*   Process $ALIAS command in the configuration file.
***************************************************************************/
int  fig_alias(char *cptr)
{
   int  j;
   static int i = 0;
   char *new,*old,*s1;

   s1 = strstr(cptr,"=");
   if (s1 != NULL) *s1 = ' ';
   cptr = getfield(figptr,cptr,15);
   if (cptr == NULL)
     {
       printf("ALIAS ERROR: - %s -\n",cptr);
       return (-1);
     }
   new = figptr;
   figptr += strlen(figptr) + 1;
   cptr = getfield(figptr,cptr,15);
   if (cptr == NULL)
     {
       printf("ALIAS ERROR: - %s -\n",cptr);
       return (-1);
     }
   old = figptr;
   figptr += strlen(figptr) + 1;
   for(s1=old; *s1 != '\0'; s1++) *s1 = tolower(*s1);
   for(s1=new; *s1 != '\0'; s1++) *s1 = tolower(*s1);
   if ((j = add_alias(new,old)) == 0)
     {
       aliaslst[i].new = new;
       aliaslst[i].old = old;
       i++;
       aliaslst[i].new = NULL;
       aliaslst[i].old = NULL;
     }
   return  j;
}
/***************************************************************************
*
*   Display user defined features.
***************************************************************************/
void usrfig(void)
{
   int  i,j;
   char **sptr;

   printf("***** Windows *****\n");
   for (i=0; windows[i].name != NULL; i++)
     {
       printf("%s  ",windows[i].name);
       sptr = windows[i].cmdstr;
       for (; *sptr != NULL; sptr++) printf("%s ",*sptr);
       printf("\n");
     }
   printf("\n***** Command Aliases *****\n");
   j = 0;
   for (i=0; aliaslst[i].new != NULL; i++)
     {
       printf("%15s = %-15s  ",aliaslst[i].new,aliaslst[i].old);
       j++;
       if (j%2 == 0) printf("\n");
     }
   if (j%2 != 0) printf("\n");
   printf("\n***** User Commands *****\n");
   for (i=0; usrlst[i].cmd != NULL; i++)
     {
       printf("Command: %s\n",usrlst[i].name);
       if (usrlst[i].stop != 0)printf("Stopvme:\n");
       if (usrlst[i].dir != NULL)printf("    Dir: %s\n",usrlst[i].dir);
       printf("       : ");
       sptr = usrlst[i].cmd;
       for (; *sptr != NULL; sptr++) printf("%s ",*sptr);
       if (usrlst[i].init != 0)printf("\nInitvme:");
       printf("\n\n");
     }
   printf("UDPtoipc Process: %s\n",udptoipcfile);
   printf("Help file: %s\n",helpfile);
   printf("Shared Memory buffer size (bytes): %i\n",shm_buffer_size);
   printf("Log Interval (seconds): %i\n",log_interval);
}
/***************************************************************************
*   Get type of display.
***************************************************************************/
void displaychk(void)
{
   int  fd,status = 0,pid;
   FILE *infile;
   char tmpfilename[L_tmpnam];

   static char *xdpy[] = {"/usr/bin/xdpyinfo",
                              NULL };

   strcpy(Display,"unknown");
   if (getenv("DISPLAY") == NULL) return;
/*
*   Run xdpyinfo to get display information
*/

   strcpy(tmpfilename,P_tmpdir);
   strcat(tmpfilename,"/PAC2XXXXXX");
   fd = mkstemp(tmpfilename);

   if ((pid = fork()) == 0)
    {
      freopen(tmpfilename,"w",stdout);
      execvp(xdpy[0],xdpy);
      perror(xdpy[0]);
      _exit(0);
    }
   else if (pid <= 0) {printf("pacman displaychk fork failure \n"); exit(99);}
   while(wait(&status) != pid);
   close(fd);
/*
*   Open the input file
*/
   if ((infile = fopen(tmpfilename,"r")) == (FILE *)NULL)
     {
       fprintf(stdout,"pacman displaychk - Cannot open input file\n");
       exit(2);
     }
/*
*  Determine display type by reading the 'vendor string:'
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      if (strstr(in_line,"vendor string:")  != NULL)
        {
          if (strstr(in_line,"UWS") != NULL)
            {
              strcpy(Display,"decstation");
            }
          else if (strstr(in_line,"Tektronix") != NULL)
            {
              strcpy(Display,"tektronix");
            }
          else if (strstr(in_line,"UNIX V4.0") != NULL)
            {
              strcpy(Display,"alpha");
            }
        }
    }
  fclose(infile);
  unlink(tmpfilename);
}
/***************************************************************************
*   Build the logger startup command
***************************************************************************/
void log_build(void)
{
   char *cptr;
   char **lptr = logger;
   int i = 0;

   while(i < (sizeof(logger)/sizeof(char *)))
     {
       if (*lptr == NULL) break;
       lptr++;
     }
   if (strcmp(Display,"alpha") == 0)
     {
       *lptr++ = figptr;
       cptr = figptr;
       strcpy(figptr,"#+0-24");
       figptr += strlen(cptr) + 1;
       *lptr++ = figptr;
       cptr = figptr;
       strcpy(figptr,"-fn");
       figptr += strlen(cptr) + 1;
       *lptr++ = figptr;
       cptr = figptr;
       strcpy(figptr,"7x13");
       figptr += strlen(cptr) + 1;
     }
   else if (strcmp(Display,"tektronix") == 0)
     {
       *lptr++ = figptr;
       cptr = figptr;
       strcpy(figptr,"#+0-24");
       figptr += strlen(cptr) + 1;
     }
   else
     {
       *lptr++ = figptr;
       cptr = figptr;
       strcpy(figptr,"#+0-24");
       figptr += strlen(cptr) + 1;
     }
   *lptr++ = figptr;
   cptr = figptr;
   strcpy(figptr,"-e");
   figptr += strlen(cptr) + 1;
   *lptr++ = figptr;
   cptr = figptr;
   strcpy(figptr,LOGGER);
   figptr += strlen(cptr) + 1;
   *lptr++ = server;
   *lptr = NULL;
}
