/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1993-2003
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
*    Environment:  ORPHAS Acquisition System
*
*    File:         /usr/users/mcsq/Dlinux/Dwks/acqipc.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/26/93    MCSQ         Original
*
*    2/17/94    MCSQ         Added open_acq_log routine.  This is for routines
*                            which need to access only the logger message queue.
*
*    3/21/95    HQJ          Added routine zbuf_shm to clear buffers for read.
*                            This is for a quick flushing of old data.
*
*    4/14/03    MCSQ         Port to Linux
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <time.h>
#define  ACQ
#include "ipcdefs.h"
#include "orphmsg.h"
#include "acqshm.h"
#include "acqlib.h"

/*
*   device codes
*/
#define  SHM   (0*256)
#define  SEM   (1*256)
#define  MQUE  (2*256)
#define  TQUE  (3*256)
/*
*    More error codes.  These are in addition to ones defined in
*    ipcdefs.h
*/
#define  INVALPTR  10    /* Invalid pointer                                 */
#define  INVALCH   11    /* Invalid channel number                          */
#define  SEMERR    12    /* Semaphore operation error                       */
#define  USRABORT  13    /* User abort                                      */
#define  SEMDELE   14    /* Semaphore set removed                           */
#define  NULLWRPTR 15    /* NULL write buffer pointer                       */
#define  NOCHANNEL 16    /* No read channel available                       */
#define  WRBUSY    17    /* Write channel in use by another process         */
#define  INVALOPEN 18    /* Invalid open flag(must be O_READ or O_WRITE)    */

/*
*    Internal Function Prototypes.  External references are defined in
*    acqlib.h
*/
 int shm_lock(int *);
void shm_unlock(void);
 int shm_read_wait(int,int *,int *);
 int shm_write_wait(int *);
void init_shm(void);

/*
*     Global variables
*/
       struct acq_ids  Ids = {-1,-1,-1,-1};  /*  IPC Ids                     */
       struct shm_use *Shm = NULL;
       int luin = -1;
       int luout = -1;

extern int errno;            /*  global error number                          */
static struct acq_keys Keys; /*  IPC Keys for current ACQ                     */
static char **Resource = NULL; /*  For error report routine                   */
static int *BitMsk;
static unsigned int *RDavail;
static unsigned int *RDuse;
static          int *RDpids;
static          int *WRbuf;
static unsigned int *WRuse;
static          int *WRwait;
static          int *RDwait;
static          int *WRpid;
static struct shm_buf *BufPtr[NUMBUF];

/******************************************************************************
*  Create all IPC Resources required for the specified ACQ device.
*
*  Call:    CALL CREATE_ACQ_IPC(DEVICE,MEMSZ,IERR)
*
*   where:   DEVICE  - CHAR*() - Device name,  vme1, vme2 etc
*            MEMSIZE - INT*4   - Buffer size requested
*            IERR    - INT*4   - Returned status code.  Zero means OK.
*                                Nonzero means an error.
*
******************************************************************************/
void create_acq_ipc__(char *device,int *memsize,int *ierr,int len)
{
  create_acq_ipc_(device,memsize,ierr,len);
}
void create_acq_ipc_(char *device,int *memsize,int *ierr,int len)
{
   int  error,id,size;

/*
*   Release any shared memory segment we are already attached to.
*/
   if (Shm != NULL)
     {
       id = shmdt((void *)(Shm));
       if (id == -1) perror("create_acq_ipc_shmdt1");
       Shm = NULL;
     }
/*
*   Get the Keys for the specified device.
*/
   if (get_acq_key(device,len,&Keys,&Resource) != 0)
     {
       *ierr = NODEVICE;
       return;
     }
   *ierr = 0;
/*
*   Attach to the shared memory segment(if it exists).  The plan here is
*   to see if some other process is using the existing segment.
*/
   if (Keys.dat_shm != 0)
     {
       id = attach_shm(Keys.dat_shm,&size,&error);
       if (id != -1)
         {
           Shm = (struct shm_use *)map_shm(id,&error);
           if (Shm != (struct shm_use *)-1)
             {
               if (Shm->WRpid != getpid())
                 {
                   if (Shm->WRpid != 0 && kill(Shm->WRpid,0) != -1)
                     {
/*
*   The shared memory segment already exists and the write channel is
*   in use by another process.
*/
                       *ierr = SHM + WRBUSY;
                       Shm = NULL;
                       return;
                     }
                 }
               shmdt((void *)(Shm));
               Shm = NULL;
             }
         }
     }
/*
*   Create the shared memory segment.
*/
   if (Keys.dat_shm != 0)
     {
       size = sizeof(struct shm_use) + sizeof(struct shm_buf) * NUMBUF;
       id = create_shm(Keys.dat_shm,size,&error);
       if (id == -1)
         {
           *ierr = SHM + error;
           return;
         }
       Shm = (struct shm_use *)map_shm(id,&error);
       if (Shm == (struct shm_use *)-1)
         {
           *ierr = SHM + error;
           Shm = NULL;
           return;
         }
       init_shm();
/*
*    Set buffer size
*/
       if (memsize != NULL && *memsize >= 2048 && *memsize <= Shm->max_buf_size)
         {
           size = Shm->max_buf_size;
           while (size > 2048)
             {
               if (size <= *memsize) break;
               size = size >> 1;
             }
           Shm->buf_size = size;
         }
     }
/*
*   Create the shared memory access semaphore set
*/
   if (Keys.dat_sem != 0)
     {
       remove_sem(Keys.dat_sem,&error);
       id = create_sem(Keys.dat_sem,NUMSEM,&error);
       if (id == -1)
         {
           *ierr = SEM + error;
           return;
         }
     }
/*
*   Create the message queue
*/
   if (Keys.log_msg != 0)
     {
       remove_msg(Keys.log_msg,&error);
       id = create_msg(Keys.log_msg,&error);
       if (id == -1)
         {
           *ierr = MQUE + error;
           return;
         }
     }
/*
*   Create the tape process queue
*/
   if (Keys.tape_msg != 0)
     {
       remove_msg(Keys.tape_msg,&error);
       id = create_msg(Keys.tape_msg,&error);
       if (id == -1)
         {
           *ierr = TQUE + error;
           return;
         }
     }
}
/******************************************************************************
*
*   Call:   CALL OPEN_ACQ_IPC(DEVICE,MEMSIZE,IERR)
*  
*    where:   DEVICE  -  CHAR*() - ACQ device to open: vme1, vme2 , etc.
*             MEMSIZE -  INT*4   - Size of a shared memory buffer in bytes.
*             IERR    -  INT*4   - Returned status of call.  Zero means OK
*                                  and nonzero means an error.
******************************************************************************/
void open_acq_ipc__(char *device,int *memsize,int *ierr,int len)
{
   open_acq_ipc_(device,memsize,ierr,len);
}
void open_acq_ipc_(char *device,int *memsize,int *ierr,int len)
{
   int  error,i,id,size;
   char *cptr;

/*
*   Release any shared memory segment we are already attached to.
*/
   if (Shm != NULL)
     {
       id = shmdt((void *)(Shm));
       if (id == -1) perror("open_acq_ipc_shmdt");
     }
   Shm = NULL;
/*
*   Get the Keys for the specified device.
*/
   if (get_acq_key(device,len,&Keys,&Resource) != 0)
     {
       *ierr = NODEVICE;
       return;
     }
   *ierr = 0;
/*
*   Attach to the shared memory segment.
*/
   if (Keys.dat_shm != 0)
     {
       id = attach_shm(Keys.dat_shm,&size,&error);
       if (id == -1)
         {
           *ierr = SHM + error;
           return;
         }
     }
   Shm = (struct shm_use *)map_shm(id,&error);
   if (Shm == (struct shm_use *)-1)
     {
       *ierr = SHM + error;
       Shm = NULL;
       return;
     }
   Ids.dat_shm = id;
/*
*   Return buffer size to caller
*/
   *memsize = Shm->buf_size;
/*
*   Initialize some pointers to shared memory variables used to
*   control access to data buffers.
*/
  BitMsk = Shm->BitMsk;
  RDavail = Shm->RDavail;
  RDuse = Shm->RDuse;
  RDpids = Shm->RDpids;
  WRpid = &Shm->WRpid;
  WRbuf = &Shm->WRbuf;
  WRuse = &Shm->WRuse;
  WRwait = &Shm->WRwait;
  RDwait = &Shm->RDwait;
/*
*   Build buffer pointer table.  The first element of the array is
*   a pointer to buffer number 0.
*/
  cptr = (char *)Shm;
  cptr = cptr + sizeof(struct shm_use);
  for(i=0; i < NUMBUF; i+=1)
    {
      BufPtr[i] = (struct shm_buf*)cptr;
      cptr = cptr + sizeof(struct shm_buf);
    }
/*
*   Reset read and write connection flags
*/
  luin = -1;
  luout = -1;
/*
*   Attach to the shared memory access semaphore set.
*/
   if (Keys.dat_sem != 0)
     {
       id = attach_sem(Keys.dat_sem,NUMSEM,&error);
       if (id == -1)
         {
           *ierr = SEM + error;
           return;
         }
       Ids.dat_sem = id;
     }
/*
*   Attach to the message queue.
*/
   if (Keys.log_msg != 0)
     {
       id = attach_msg(Keys.log_msg,&error);
       if (id == -1)
         {
           *ierr = MQUE + error;
           return;
         }
       Ids.log_msg = id;
     }
/*
*   Attach to the tape process queue.
*/
   if (Keys.tape_msg != 0)
     {
       id = attach_msg(Keys.tape_msg,&error);
       if (id == -1)
         {
           *ierr = TQUE + error;
           return;
         }
       Ids.tape_msg = id;
     }
}
/******************************************************************************
*
*   Call:   CALL OPEN_ACQ_LOG(DEVICE,IERR)
*  
*    where:   DEVICE  -  CHAR*() - ACQ device to open: vme1, vme2 , etc.
*             IERR    -  INT*4   - Returned status of call.  Zero means OK
*                                  and nonzero means an error.
******************************************************************************/
void open_acq_log__(char *device,int *ierr,int len)
{
   open_acq_log_(device,ierr,len);
}
void open_acq_log_(char *device,int *ierr,int len)
{
   int  error,id;

   *ierr = 0;
   if (Ids.log_msg != -1) return;
/*
*   Get the Keys for the specified device.
*/
   if (get_acq_key(device,len,&Keys,&Resource) != 0)
     {
       *ierr = NODEVICE;
       return;
     }
/*
*   Attach to the message queue.
*/
   if (Keys.log_msg != 0)
     {
       id = attach_msg(Keys.log_msg,&error);
       if (id == -1)
         {
           *ierr = MQUE + error;
           return;
         }
       Ids.log_msg = id;
     }
   else  *ierr = MQUE + NOEXIST;
}
/******************************************************************************
*
*    Call:    CALL REMOVE_ACQ_IPC(DEVICE,IERR)
*
*    where:   DEVICE - CHAR*() -  ACQ device:  vme1, vme2 etc
*             IERR   - INT*4   -  Returned status code.  Zero means OK
*                                 and nonzero means an error.
******************************************************************************/
void remove_acq_ipc__(char *device,int *ierr,int len)
{
   remove_acq_ipc_(device,ierr,len);
}
void remove_acq_ipc_(char *device,int *ierr,int len)
{
   int  error,id,dum;

/*
*   Release any shared memory segment we are already attached to.
*/
   if (Shm != NULL)
     {
       id = shmdt((void *)(Shm));
       if (id == -1) perror("open_acq_ipc_shmdt");
     }
/*
*   Get Keys for the specified device.
*/
   if (get_acq_key(device,len,&Keys,&Resource) != 0)
     {
       *ierr = NODEVICE;
       return;
     }
   *ierr = 0;
/*
*   Remove the shared memory from the system.
*/
   if (Keys.dat_shm != 0)
     {
       id = attach_shm(Keys.dat_shm,&dum,&error);
       if (id != -1)
         {
           if (shmctl(id,IPC_RMID,NULL) == -1)
             {
               switch (errno)
                {
                  case EINVAL:
                  case EPERM:
                    break;
                  default:
                    perror("remove_acq_ipc_shmctl");
                    exit(99);
                }
             }
         }
     }
   Ids.dat_shm = -1;
   Shm = NULL;
/*
*   Remove the shared memory access semaphore set.
*/
   if (Keys.dat_sem != 0)
     {
       id = attach_sem(Keys.dat_sem,NUMSEM,&error);
       if (id != -1)
         {
           if (semctl(id,0,IPC_RMID,NULL) == -1)
             {
               switch (errno)
                {
                  case EINVAL:
                  case EPERM:
                    break;
                  default:
                    perror("remove_acq_ipc_semctl");
                    exit(99);
                }
             }
         }
     }
   Ids.dat_sem = -1;
/*
*   Remove the message queue.
*/
   if (Keys.log_msg != 0)
     {
       id = attach_msg(Keys.log_msg,&error);
       if (id != -1)
         {
           if (msgctl(id,IPC_RMID,NULL) == -1)
             {
               switch (errno)
                {
                  case EINVAL:
                  case EPERM:
                    break;
                  default:
                    perror("remove_acq_ipc_msgctl");
                    exit(99);
                }
             }
         }
     }
   Ids.log_msg = -1;
/*
*   Remove the tape process queue.
*/
   if (Keys.tape_msg != 0)
     {
       id = attach_msg(Keys.tape_msg,&error);
       if (id != -1)
         {
           if (msgctl(id,IPC_RMID,NULL) == -1)
             {
               switch (errno)
                {
                  case EINVAL:
                  case EPERM:
                    break;
                  default:
                    perror("remove_acq_ipc_msgctl");
                    exit(99);
                }
             }
         }
     }
   Ids.tape_msg = -1;
   Resource = NULL;
}
/*****************************************************************************
*
*  Send an ASCII message from a FORTRAN call to the logger.
*
*  Call:  CALL LOG_MSG(TYPE,SENDER,TEXT,TEXT_LEN,ERR)
*
*   Call arguments:
*        INT*4         TYPE       Mesage type. 1 = inform, 2= warn, 3= panic
*        CHARACTER*8   SENDER     Name of sender program
*        CHARACTER*104 TEXT       Body of message
*        INT*4         TEXT_LEN   Number of bytes in text string
*        INT*4         ERR        Return status
*
*   Return:
*             ERR = 0  means no errors detected.
*****************************************************************************/
void log_msg__(int *type, char *sender, char *text, int *text_len, int *err)
{
   log_msg_(type, sender, text, text_len, err);
}
void log_msg_(int *type, char *sender, char *text, int *text_len, int *err)
{
   struct orphmsg message;
   int  len;
   char *cptr;
   time_t tod;

/*
*   Do we have a log message queue?
*/
   if (Ids.log_msg == -1)
     {
        *err = MQUE + NOEXIST;        /* Return error code */
        return;
     }
/*
*  Build buffer to send to the message queue.  First set message type.
*  Then put in the caller name followed by 1 space.
*/
   message.type = *type;
   strncpy(message.text,sender,MSG_SEND_LEN);
   message.text[MSG_SEND_LEN] = ' ';
/*
*  Next comes the timestamp - 20 characters total, including 2 trailing
*  spaces.
*/
   time(&tod);
//   strftime(&message.text[MSG_TIME_COL-1],21,"%d-%b-%y %H:%M:%S  ",
   strftime(message.text+MSG_TIME_COL-1,21,"%d-%b-%y %H:%M:%S  ",
                                                              localtime(&tod));
/*
*  Next, put in the user's text message - the good stuff.
*  If the user specified length is greater than 104 characters, only
*  the first 104 will be used.
*/
   if(*text_len < MSG_USER_LEN) len = *text_len;
   else  len = MSG_USER_LEN;
/*
*  Suppress trailing spaces
*/
   cptr = text + len - 1;
   while (cptr > text)
     {
       if (*cptr != ' ') break;
       --cptr;
     }
   len = cptr - text + 1;
   for (cptr = text; cptr < text+len; cptr++) if (*cptr == '\0') *cptr = ' ';
   strncpy(&message.text[MSG_USER_COL-1],text,(size_t)len);
   len += MSG_SEND_LEN+MSG_TIME_LEN + 3;
/*
*  Finally, terminate the string with a NULL char and send to the message
*  queue.
*/
   *err = 0;
   message.text[len++] = '\0';
   while (msgsnd(Ids.log_msg,&message,len,0) == -1)
    {
      if (errno != EINTR)
        {
          switch  (errno)
           {
             case EINVAL:
             case EIDRM:
               *err = MQUE + NOEXIST;
               break;
             case EACCES:
               *err = MQUE + NOPERM;
               break;
             default:
               *err = -1;
               perror("log_msg_msgsnd");
           }
          return;
        }
    }
   return;
}
/******************************************************************************
*
*  Returns an ASCII string for an error code.  This one is for a C caller.
*  It returns a NULL terminated string.
*
*******************************************************************************/
void  acq_error(int *error,char *string,int len)
{
   char *s1;

/*
*   First call the FORTRAN callable routine.
*/
   acq_error_(error,string,len);
/*
*  NULL terminate the string returned above.
*/
   s1 = string + len -1;
   while(s1 > string)
    {
      if (*s1 != ' ')
        {
          s1++;
          *s1 = '\0';
          break;
        }
      s1--;
    }
}
/******************************************************************************
*
*   Call:   CALL ACQ_ERROR(ERROR,STRING)
*
*   where: 
*             INT*4  ERROR - Error code return by a routime in this package 
*   return:
*             CHARACTER*(*)  -  ASCII message for this error code.
*******************************************************************************/
void  acq_error__(int *error,char *string,int len)
{
   acq_error_(error,string,len);
}
void  acq_error_(int *error,char *string,int len)
{
  int  i,ierr,j,k;
  static char *msg[] = {
    "Nonexistent ACQ Resource set",
    "No System Table Space for IPC Resource",
    "Insufficient memory available for shared memory segment",
    "Shared memory size exceeds System maximum",
    "No permission for use of IPC Resource",
    "Not Owner of IPC Resource",
    "IPC Resource does not exist",
    "Too many shared memory segments attached",
    "Existing Semaphore set too small",
    "Invalid buffer pointer",
    "Invalid channel number",
    "Semaphore operation error",
    "User aborted read of shared memory buffer",
    "Semaphore set has been removed",
    "NULL write buffer pointer",
    "No read channel available",
    "Write channel in use by another process",
    "Invalid argument for open shared memory request",
    "Unknown error code"
};
  static char uninit[] = "IPC has not been opened";
  static char illegal[] = "Illegal IPC error code";
  static char wtmeof[] = "WTM End-of-File";

  for (i=0; i < len; i++) string[i] = ' ';
  if (*error == 0) return;
  if (*error == 999)
    {
      i = strlen(wtmeof);
      if (i > len) i = len;
      strncpy(string,wtmeof,(size_t)i);
      return;
    }
  if (Resource == NULL)
    {
      i = strlen(uninit);
      if (i > len) i = len;
      strncpy(string,uninit,(size_t)i);
      return;
    }
  ierr = *error & 0xff;
  k = (*error >> 8) & 0xff;
  i = 0;
  while(*(Resource+i) != NULL) i++;
  if (k >= i)
    {
      i = strlen(illegal);
      if (i > len) i = len;
      strncpy(string,illegal,(size_t)i);
      return;
    }
  j = sizeof(msg)/sizeof(char *);
  if (ierr < 1 || ierr > j) ierr = j;
  ierr = ierr -1;
  i = 0;
  if (ierr)
    {
      i = strlen(*(Resource+k));
      if (i >= len) i=len;
      strncpy(string,*(Resource+k),(size_t)i);
      len = len - i;
    }
  if (len)
    {
      string[i] = '\0';
      i = strlen(msg[ierr]);
      if (i >= len) i = len;
      strncat(string,msg[ierr],(size_t)i);
    }
}
/******************************************************************************
*
*  Get a pointer to a shared memory buffer where caller may write a data
*  buffer.
*
*  Call:    buffer_pointer = write_shm(NULL,&error);
*     or    buffer_pointer = write_shm(buffer_pointer,&error);
*
*  Returns:  If call argument is an invalid pointer, a NULL pointer
*            is returned.  Otherwise, the pointer returned points
*            to a write buffer in shared memory.
*
             error - zero means OK. Nonzero is an error code.

*     The first call should always specify a NULL pointer as the argument.
*     After the first call, the pointer argument should be the same pointer
*     returned by the previous call.
*
*     When the pointer is NULL, the only action is to return a pointer to
*     a buffer in shared memory.  The user may attach to multiple buffers
*     by calling multiple times with a NULL pointer.
*
*     When the pointer is not NULL, the buffer pointed to is marked ready
*     to be read.  A new buffer for writing is found and a pointer to
*     this new buffer is returned.  CAUTION: a nonNULL pointer MUST
*     be a pointer returned by a previous call to this routine!
******************************************************************************/
struct shm_buf *write_shm(struct shm_buf *buffer,int *error)
{
           int i,found = -1;
  unsigned int oldmsk,tstmsk;
  volatile unsigned int *ulptr,*ulptr2;
  unsigned int rdmsk,wrmsk,freemsk;
  struct shm_buf **buf,*Shmbuf;
  static unsigned short resetall[NUMSEM];
  static unsigned int  buf_num = 0;

/*
*   Find the bit mask for the buffer pointed to by the argument
*   buffer.  If buffer is not a shared memory pointer, return and error
*   cdoe and a NULL pointer.
*/
  buf = BufPtr;
  oldmsk = 0;
  if (buffer != NULL)
    {
      for (i=0; i < NUMBUF; i++)
        {
          if (*buf == buffer)
           {
             oldmsk = *(BitMsk + i);
             break;
           }
          buf++;
        }
      if (i == NUMBUF)
        {
          *error = INVALPTR;
          return NULL;
        }
    }
  if (shm_lock(error)) return NULL;
/*
*   The following loop executes until a write buffer is available.
*/
  do
   {
/*
*   First we build a bit mask of all read buffers in use and mark available
*   the buffer just written.
*/
     ulptr = RDuse;
     ulptr2 = RDavail;
     rdmsk = 0;
     for(i=0; i < NUMPROC; i++)
       {
         rdmsk = rdmsk | *ulptr++;
         *ulptr2 = *ulptr2 | oldmsk;
         ulptr2++;
       }
/*
*   Get the bit corresponding to the last buffer we picked to write.
*   Then start the search for a new write buffer.
*
*   As we search, keep a mask for all buffers we can't use because they
*   are in use by a read process.  When we find a write buffer, all buffer
*   skipped over by the write search will be marked not available to
*   any read process.
*/
     wrmsk = *WRuse;
     *WRuse = *WRuse & ~oldmsk;
     freemsk = 0;
     tstmsk = *(BitMsk + *WRbuf);
     for (i= *WRbuf; i < NUMBUF; i++)
      {
        if ((rdmsk & tstmsk) == 0)
          {
            if ((wrmsk & tstmsk) == 0)
              {
                found = i;
                break;
              }
          }
        else  freemsk = freemsk | tstmsk;
        tstmsk = tstmsk << 1;
      }
/*
*  Continue the buffer search
*/
     if (found >= 0) break;
     tstmsk = 1;
     for (i=0; i < *WRbuf; i++)
      {
        if ((rdmsk & tstmsk) == 0)
          {
            if ((wrmsk & tstmsk) == 0)
              {
                found = i;
                break;
              }
          }
        else  freemsk = freemsk | tstmsk;
        tstmsk = tstmsk << 1;
      }
     if (found < 0)
       {
/*
*   No buffer is presently available for write.   Set a semaphore
*   and hope that a reader will release a buffer for our use.  The
*   reader should clear the semaphore when he releases a buffer.
*   When that happens we execute this loop again searching for
*   the buffer just released.
*/
         if (shm_write_wait(error)) return NULL;
         continue;
       }
     found = i;
   } while(found < 0);
/*
*   A buffer has been found.  Several things now happen:
*     1) Save the buffer number for our next read or write search
*     2) Set the bit corresponding to this buffer in the write WRuse mask
*     3) Set buffer size in shm_buf struct
*     4) Mark unavailable buffers skipped because they were in use by
*        some read process.  Also mark the buffer just selected as
*        unavailable for read.
*     5) Put the buffer number in the buffer header.
*/
  *WRbuf = found;
  wrmsk =  *(BitMsk + found);
  *WRuse = *WRuse | wrmsk;
  freemsk = freemsk | wrmsk;
  ulptr = RDavail;
  for(i=0; i < NUMPROC; i++)
    {
     *ulptr = *ulptr & ~freemsk;
     *ulptr++;
    }
  Shmbuf = *(BufPtr + found);
  Shmbuf->size = Shm->buf_size;
  buf_num++;
  Shmbuf->buf_num = buf_num;

  if (buffer == NULL || *RDwait == 0) shm_unlock();
  else
    {
/*     Reset semaphores for all read processes and unlock Shm access   */

      *RDwait = 0;
      while (semctl(Ids.dat_sem,0, SETALL, resetall) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("write_shm");
           }
          return NULL;
        }
    }
  return Shmbuf;
}
/******************************************************************************
*
*   Get a pointer to a read buffer in shared memory.
*
*  Call:    buffer_pointer = read_shm(lu,NULL,&ierr,&abort);
*     or    buffer_pointer = read_shm(lu,buffer_pointer,&ierr,&abort);
*
*   where:  lu  -  small positive nonzero integer return by a call to
*                  open_shm(O_READ).  If lu is invalid, a NULL pointer
*                  is returned.
*
*          abort - pointer to a user flag.  When the value stored in
*                  the flag is not 0x20202020, the returned pointer
*                  will be -1
*
*  Returns:  Returns a NULL pointer is lu is invalid or buffer_pointer
*            is invalid.  Returns -1 if user abort flag is not set to
*            0x20202020.
*
*          ierr - 0 means OK, nonzero means error.
*
*  If the call argument pointer is NULL, only a pointer to a new read buffer
*  is returned.  If the argument is a pointer returned by a prior call,
*  the buffer pointed to is released to the writer to be filled again.
*  Multiple calls with NULL pointer will attach to multiple read buffers.
*  These buffers can not be written again until:
*    1) This routine is called with the pointer.
*    2) A call to free_read_buf with the pointer.
******************************************************************************/
struct shm_buf *read_shm(int lu,struct shm_buf *buffer,int *error,int *abort)
{
  int i,mk,found = -1;
  int tstmsk;
  struct shm_buf **buf;
  unsigned int *lavail,*luse;
  static struct sembuf wrwake[2] = { {WRWAKE,-1,0},
                                     {SEMLOCK,-1,0} };

  if (*abort != 0x20202020)
    {
      *error = USRABORT;
      return NULL;
    }
/*
*   lu is used to access the tables associated with read processes.
*   If lu is invalid,  return a NULL pointer.
*/
  if (lu <= 0 || lu > NUMPROC)
    {
      *error = INVALCH;
      return NULL;
    }
  lu--;
  if (shm_lock(error)) return NULL;
  buf = BufPtr;
  lavail = RDavail + lu;
  luse = RDuse + lu;
/*
*   buffer is a pointer to the last buffer read(if any).  That buffer
*   is to be released so it can be written again.  If pointer does not
*   point to one of our buffers, return a NULL pointer and an invalid pointer
*   error code.
*/
  if (buffer != NULL)
    {
      for (i=0; i < NUMBUF; i++)
        {
          if (*buf == buffer)
           {
             *luse = *luse & ~*(BitMsk + i);
             break;
           }
          buf++;
        }
      if (i == NUMBUF)
        {
          *error = INVALPTR;
          return NULL;
        }
     }
  if (buffer != NULL && *WRwait != 0)
    {
/*
*   We have just released a buffer and the write process is waiting for a
*   buffer.  Reset the WRWAKE semaphore and unlock shared memory.
*/
      *WRwait = 0;
      while (semop(Ids.dat_sem,wrwake,2) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("read_shm");
           }
          return NULL;
        }
/*
*   Must lock shared memory again before we continue!!
*/
      if (shm_lock(error)) return NULL;
    }
/*
*   Search for a read buffer available to this process.   We always
*   start the search at the buffer after the current write buffer.
*/
  do
   {
     if ((mk = *lavail) == 0)
       {
/*
*   No buffer is available.  Just go wait till one is available.
*/
         if (shm_read_wait(lu,error,abort)) return NULL;
         continue;
       }
     tstmsk = *(BitMsk + *WRbuf);
     for (i= *WRbuf; i < NUMBUF; i++)
      {
        if ((mk & tstmsk) != 0)
          {
            found = i;
            break;
          }
        tstmsk = tstmsk << 1;
      }
     if (found >= 0) break;
     tstmsk = 1;
     for (i=0; i < *WRbuf; i++)
      {
        if ((mk & tstmsk) != 0) break;
        tstmsk = tstmsk << 1;
      }
     found = i;
   } while(found < 0);
/*
*   We found a read buffer available to this process.  We now do:
*     1)  Mark the buffer we found not available so we won't read it
*         again.
*     2)  Also mark the buffer in use so the write process won't
*         use it till we finish.
*/
  *lavail = *lavail & ~tstmsk;
  *luse = *luse | tstmsk;
  shm_unlock();
  return *(BufPtr + found);
}
/******************************************************************************
*
*   Routine to initialize the control store in shared memory.  This should
*   be called only after the shared memory segment is created.
******************************************************************************/
void init_shm(void)
{
  int  i,j;

  if (Shm == NULL) return;
  Shm->log_interval = LOGINTERVAL;
  Shm->max_buf_size = MAXBUFSIZE;
  Shm->buf_size = BUFSIZE;
  Shm->num_buf = NUMBUF;
  Shm->num_proc = NUMPROC;
  Shm->WRwait = 0;
  Shm->WRbuf = 0;
  Shm->RDwait = 0;
  Shm->WRpid = 0;
  Shm->WRuse = 0;
/*
*   Build the buffer bit mask array.  Each mask has only one bit set.
*   The first element corresponds to buffer number 0 and has a value
*   of 1.  The second element corresponds to buffer number 1 and has
*   value of 0x2.
*/
  j =1;
  for(i=0; i < NUMBUF; i++, j=j<<1) Shm->BitMsk[i]=j;
/*
*   Clear read buffer available array, the read buffer used array,
*   and the array of read process PIDs.
*/
  for(i=0; i < NUMPROC; i+=1)
    {
      Shm->RDavail[i] = 0;
      Shm->RDuse[i] = 0;
      Shm->RDpids[i] = 0;
    }
}
/******************************************************************************
*
*  Open a channel for shared memory access.
*
*  Call:    unit = open_shm(flag,&error)
*
*  Where:   int flag  -  Should be O_WRITE to open the write channel or
*                        O_READ to open a read channel.  Anything else
*                        return an error.
*
*  returns: int unit   -  channel number to be used in future calls to access
*                         shared memory.
*           int *error -  Returned error code.  0 means no error.
******************************************************************************/
int open_shm(int flag,int *error)
{
  int i,*lpids;
  static struct sembuf wrwake[2] = { {WRWAKE,-1,0},
                                     {SEMLOCK,-1,0} };

  if (shm_lock(error)) return -1;
/*
*   Search for read processes which have died.  If there are any,
*   release to buffers assigned to them.
*/
  lpids = RDpids;
  for (i=0; i < NUMPROC; i++)
    {
      if (kill(*lpids,0) == -1 && errno == ESRCH)
        {
          *lpids = 0;
          *(RDuse + i) = 0;
        }
      lpids++;
    }
  lpids = RDpids;
  if (flag == O_READ)
    {
/*
*   Open a read channel
*/
      for (i=0; i < NUMPROC; i++)
        {
          if (*lpids == 0)
            {
              *lpids = getpid();
              *(RDuse + i) = 0;
              *(RDavail + i) = 0;
              break;
            }
          lpids++;
        }
      if (i == NUMPROC) *error = NOCHANNEL;
    }
  else if (flag == O_WRITE)
    {
/*
*   Open the write channel
*/
      if (*WRpid == 0 || (kill(*WRpid,0) == -1 && errno == ESRCH))
        {
          *WRpid = getpid();
          *WRuse = 0;
          i = -1;
        }
      else   *error = WRBUSY;
    }
  else  *error = INVALOPEN;
  if (*error != 0)
    {
      shm_unlock();
      return -1;
    }
  if (*WRwait != 0)
    {
/*
*   When we open a read channel, we clear the available and used flags.
*   If the write process was waiting for a buffer, we need to wake
*   him so he can try to find a buffer.
*/
      *WRwait = 0;
      while (semop(Ids.dat_sem,wrwake,2) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("open_shm");
           }
          return -1;
        }
    }
  else  shm_unlock();
  return (i + 1);
}
/******************************************************************************
*
*  Close channels for access to shared memory.  When a channel is closed,
*  the pid is set to zero and any buffers in use are released.
*  Also, a call to close scans the pid list for nonexistent processes.
*  If any are found, they are removed and their buffers are released.
*
*   Call:      close_shm(lu)
*
*    where    lu - negative means close all read channels opened by this
*                  process.
*                - zero means close the write channel.
*                - positive and nonzero means close only the read channel
*                  specified.
******************************************************************************/
void close_shm(int lu)
{
  int error,i,*lpids,ourpid;
  static struct sembuf wrwake[2] = { {WRWAKE,-1,0},
                                    {SEMLOCK,-1,0} };

  if (RDpids == NULL) return;
  if (shm_lock(&error)) return;
/*
*   Search for read processes which have died.  If there are any,
*   release to buffers assigned to them.
*/
  lpids = RDpids;
  for (i=0; i < NUMPROC; i++)
    {
      if (kill(*lpids,0) == -1 && errno == ESRCH)
        {
          *lpids = 0;
          *(RDuse + i) = 0;
        }
      lpids++;
    }
  ourpid = getpid();
  if (lu < 0)
    {
/*
*   Close all read channels for this process.
*/
      lpids = RDpids;
      for (i=0; i < NUMPROC; i++)
        {
          if (*lpids == ourpid)
            {
              *lpids = 0;
              *(RDuse + i) = 0;
            }
          lpids++;
        }
    }
  else if (lu > 0)
    {
/*
*   Close only the read channel specified by call argument lu.
*/
      if (lu <= NUMPROC)
        {
          lu = lu -1;
          if (*(RDpids + lu) == ourpid)
            {
              *(RDpids + lu) = 0;
              *(RDuse + lu) = 0;
            }
        }
    }
  else
    {
/*
*   Close write channel if it belongs to this process.
*/
      if (*WRpid == ourpid)
        {
          *WRuse = 0;
          *WRpid = 0;
        }
    }
  if (*WRwait != 0)
    {
/*
*  If the write process is waiting for a buffer, clear the WRWAKE
*  so any released buffers can be used.
*/
      *WRwait = 0;
      while (semop(Ids.dat_sem,wrwake,2) == -1)
        {
          if (errno == EINTR) continue;
          return;
        }
    }
  else  shm_unlock();
  return;
}
/******************************************************************************
*
*  Release a read channel buffer.
*
*  Call:   stat = free_read_buf(lu,buffer)
*
*   where:  lu     -  read channel number(1 thru NUMPROC)
*           buffer - pointer to a shared memory buffer   
*
*  Returns:  stat  - 0 means OK. Nonzero means error.
******************************************************************************/
int free_read_buf(int lu,struct shm_buf *buffer)
{
  int i,error;
  struct shm_buf **buf;
  static struct sembuf wrwake[1] = { {WRWAKE,-1,0} };

  if (shm_lock(&error)) return error;
  if (lu > 0 && lu <= NUMPROC)
    {
      buf = BufPtr;
      for (i=0; i < NUMBUF; i++)
        {
          if (*buf == buffer)
            {
              *(RDuse + lu -1) = *(RDuse + lu -1) & ~*(BitMsk + i);
              break;
            }
          buf++;
        }
      if (i == NUMBUF) error = INVALPTR;
    }
  else  error = INVALCH;;
  if (*WRwait != 0)
    {
/*
*   The write process is waiting for a buffer.  Reset his wait semaphore.
*/
      *WRwait = 0;
      while (semop(Ids.dat_sem,wrwake,1) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              error = SEM + NOEXIST;
              break;
             case EIDRM:
              error = SEM +SEMDELE;
              break;
             default:
              error = SEM + SEMERR;
              perror("free_read_buf");
           }
          break ;
        }
    }
  shm_unlock();
  return error;
}
/******************************************************************************
*
*  Release a write channel buffer.
*
*  Call:   stat = free_write_buf(buffer)
*
*   where:   buffer - pointer to a shared memory buffer   
*
*  Returns:  stat  - 0 means OK. Nonzero means error.
******************************************************************************/
int free_write_buf(struct shm_buf *buffer)
{
  int i,error;
  struct shm_buf **buf;

  if (shm_lock(&error)) return error;
  buf = BufPtr;
  for (i=0; i < NUMBUF; i++)
    {
      if (*buf == buffer)
        {
          *WRuse = *WRuse & ~*(BitMsk + i);
          break;
        }
      buf++;
    }
  shm_unlock();
  if (i == NUMBUF) return INVALPTR;
  return 0;
}
/******************************************************************************
*
*  Routine to gain exclusive access to shared memory control tables.
*
*  Call:     stat = shm_lock(&error)
*
*  Returns:  error - error code if stat is nonzero
*            stat  - 0 means OK. Nonzero means error.
******************************************************************************/
int shm_lock(int *error)
{
  static struct sembuf lck[2] = {{SEMLOCK,0,0},
                                 {SEMLOCK,1,0} };

  while (semop(Ids.dat_sem,lck,2) == -1)
    {
      if (errno == EINTR) continue;
      switch (errno)
       {
         case EINVAL:
          *error = SEM + NOEXIST;
          break;
         case EIDRM:
          *error = SEM +SEMDELE;
          break;
         default:
          *error = SEM + SEMERR;
          perror("shm_lock");
       }
     return 1;
    }
  *error = 0;
  return 0;
}
/******************************************************************************
*   Release shared memory for others to use.
*
*  Call:    shm_unlock()
******************************************************************************/
void shm_unlock(void)
{
  static struct sembuf unlck[1] = { {SEMLOCK,-1,0} };

  while (semop(Ids.dat_sem,unlck,1) == -1)
    {
      if (errno != EINTR) return;
    }
  return;
}
/******************************************************************************
*  Wait for a read buffer available or an Abort by the user.
*
*  Call:  stat = shm_read_wait(&error,&abort)
*
*  where:  abort  - Pointer to user abort flag.
*
*  Return:  stat  - 0 means OK.  Nonzero means error or an Abort
*           error - reason code if stat is nonzero.
*
*  CAUTION!  CAUTION!  If stat returns Nonzero,  shared memory will NOT
*                      be locked.  The caller of this routine call shm_lock
*                      if continued access to share memory is required!
******************************************************************************/
int shm_read_wait(int lu,int *error,int *abort)
{
  static struct sembuf set[2] = { {RDWAKE,1,0},
                                 {SEMLOCK,-1,0} };
  static struct sembuf wait[3] = { {RDWAKE,0,0},
                                  {SEMLOCK,0,0},
                                  {SEMLOCK,1,0} };

  *error = 0;
/*
*   Set a flag for the write process to indicate that one or more
*   read processes are waiting for a buffer.  Set the RDWAKE semaphore
*   and unlock shared memory.
*/
  *RDwait += 1;
  set[0].sem_num = RDWAKE + lu;
  wait[0].sem_num = RDWAKE + lu;
  while (semop(Ids.dat_sem,set,2) == -1)
    {
      if (errno == EINTR) continue;
      switch (errno)
       {
         case EINVAL:
          *error = SEM + NOEXIST;
          break;
         case EIDRM:
          *error = SEM +SEMDELE;
          break;
         default:
          *error = SEM + SEMERR;
          perror("shm_read_wait1");
       }
      return *error;
    }
/*
*   Now we wait for the RDWAKE semaphore to be reset by the write process.
*   Also we must again gain exclusive access to shared memory.
*
*   Milner's codes typically use CTRL C to stop the operation in progress.
*   The CTRL C causes a signal which Milner catches and changes an INT*4
*   variable from 0x20202020 to something else.  That signal will also
*   interrupt to semop system call.  Hence, under the assumption that
*   abort is a pointer to the varaible changed by CTRL C, we will exit
*   this routine with an error code after the CTRL C is typed.
*/
  while (semop(Ids.dat_sem,wait,3) == -1)
    {
      if (errno != EINTR)
        {
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("shm_read_wait2");
           }
        }
      if (*abort != 0x20202020) *error = USRABORT;
      if (*error != 0) break;
    }
   return *error;
}
/******************************************************************************
*
*   This routine is used by the write process when no buffer is available.
*   A flag is set in shared memory indicating that the write process is
*   waiting for a buffer.  Readers should check the flag when they release
*   a buffer.  If the flag is set, it is proper to reset the semaphore
*   WRWAKE.
*
*  Call:     stat = shm_write_wait(&error)
*
*  Return:  stat  -  0 means OK.  Nonzero means an error.
******************************************************************************/
int shm_write_wait(int *error)
{
  static int ipass = 0;
  static unsigned short resetall[NUMSEM];
  static struct sembuf wrwake[2] = { {WRWAKE,1,0},
                                    {SEMLOCK,-1,0} };
  static struct sembuf wait[3] = { {WRWAKE,0,0},
                                  {SEMLOCK,0,0},
                                  {SEMLOCK,1,0} };

  if (ipass == 0)
    {
      ipass = 1;
      resetall[SEMLOCK] = 0;
      resetall[WRWAKE] = 1;
    }
  *error = 0;
  *WRwait = 1;
  if (*RDwait != 0)
    {
/*
*   One or more read processes are waiting for a buffer.  Wake all
*   waiting readers, set WRWAKE and unlock shared memory.
*/
      *RDwait = 0;
      while (semctl(Ids.dat_sem,0, SETALL, resetall) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("shm_write_wait1");
           }
          return *error;
        }
    }
  else
    {
/*
*   No one waiting to read a buffer.  Set WRWAKE and unlock shared memory.
*/
      while (semop(Ids.dat_sem,wrwake,2) == -1)
        {
          if (errno == EINTR) continue;
          switch (errno)
           {
             case EINVAL:
              *error = SEM + NOEXIST;
              break;
             case EIDRM:
              *error = SEM +SEMDELE;
              break;
             default:
              *error = SEM + SEMERR;
              perror("shm_write_wait2");
           }
          return *error;
        }
    }
/*
*   Wait for the semaphore WRWAKE to be zero, lock shared memory and
*   return.
*/
  while (semop(Ids.dat_sem,wait,3) == -1)
    {
      if (errno == EINTR) continue;
      switch (errno)
       {
         case EINVAL:
          *error = SEM + NOEXIST;
          break;
         case EIDRM:
          *error = SEM +SEMDELE;
          break;
         default:
          *error = SEM + SEMERR;
          perror("shm_write_wait3");
       }
    }
   return *error;
}
/******************************************************************************
*
*  Call:   CALL  READ_SHM(BUF,NBUF,NREAD,EVTNUM,EVENTS,BUFNUM,IERR,ABORT)
*
* where:  BUF   - Integer array
*         NBUF  - Size of array BUF in bytes
*         ABORT - Milner's CTRL C variable
*
* Returns:
*         NREAD  - Number of bytes stored in array BUF
*         EVTNUM - Event number from VME system
*         EVENTS - Number of events stored in array BUF
*         BUFNUM - Buffer number
*         IERR   - 0 means OK.  Nonzero means an error
******************************************************************************/
void read_shm__(unsigned char *buffer,int *nbuf,int *nread,
               unsigned int *event_num,int *events,unsigned int *bufs,
                                                    int *ierr,int *msgf)
{
  read_shm_(buffer,nbuf,nread,event_num,events,bufs,ierr,msgf);
}
void read_shm_(unsigned char *buffer,int *nbuf,int *nread,
               unsigned int *event_num,int *events,unsigned int *bufs,
                                                    int *ierr,int *msgf)
{
   static struct shm_buf *ibuf = NULL;

   *ierr = 0;
   if (luin == -1)
     {
       ibuf = NULL;
       luin = open_shm(O_READ,ierr);
       if (*ierr != 0) return;
     }
   ibuf = read_shm(luin,ibuf,ierr,msgf);
   if (*ierr != 0) return;
   *event_num = ibuf->event_num;
   *events = ibuf->events;
   *bufs = ibuf->buf_num;
   if ((*nread = ibuf->size) > *nbuf) *nread = *nbuf;
   memcpy(buffer,ibuf->data,*nread);
   free_read_buf(luin,ibuf);
   ibuf = NULL;
/******
   *ierr = 0;
   if (*nread == 0) *ierr = 999;
******/
}
/******************************************************************************
*
*  Call:   CALL WRITE_SHM(BUF,NBUF,NWRITE,IERR)
*
* where:  BUF  - Integer array where data to be written is stored
*         NBUF - Number of data bytes stored in array BUF
*
* Returns:
*         NWRITE - Number of data bytes written
*         IERR   - 0 means OK.  Nonzero means an error.
******************************************************************************/
void write_shm__(unsigned char *buffer,int *nbuf,int *nwrite,int *ierr)
{
   write_shm_(buffer,nbuf,nwrite,ierr);
}
void write_shm_(unsigned char *buffer,int *nbuf,int *nwrite,int *ierr)
{
   static int ecount = 0;
   static struct shm_buf *obuf = NULL;

   *ierr = 0;
   if (luout == -1)
     {
       obuf = NULL;
       luout = open_shm(O_WRITE,ierr);
       if (*ierr != 0) return;
       obuf = write_shm(obuf,ierr);
       if (*ierr != 0) return;
     }
   if (obuf == NULL)
     {
       *ierr = NULLWRPTR;
       return;
     }
   obuf->event_num = ecount++;
   obuf->buf_num = ecount;
   obuf->events = 1;
   if (*nbuf <= Shm->buf_size) *nwrite = *nbuf;
   else  *nwrite = Shm->buf_size;
   obuf->size = *nwrite;
   memcpy(obuf->data,buffer,*nwrite);
   obuf = write_shm(obuf,ierr);
}
/******************************************************************************
*
*   Close a channel opened by the FORTRAN callable routines READ_SHM or
*   WRITE_SHM.
*
*  Call:    CALL  CLOSE_SHM(FLAG)
*
*  where:   INT*4  FLAG - O_READ means close read channel and O_WRITE
*                         means close the write channel.
******************************************************************************/
void close_shm__(int *flag)
{
   close_shm_(flag);
}
void close_shm_(int *flag)
{
   if (*flag == O_READ && luin >= 0 && luin < NUMPROC)
     {
       close_shm(luin);
       luin = -1;
     }
   else if (*flag == O_WRITE && luout >= 0 && luin < NUMPROC)
     {
       close_shm(0);
       luout = -1;
     }
}
/******************************************************************************
*
*   Display shared memory status.  FORTRAN callable.
*
******************************************************************************/
void acq_status__(void)
{
   acq_status_();
}
void acq_status_(void)
{
   acq_shm_spy();
}
/******************************************************************************
*
*   Display shared memory status.
*
******************************************************************************/
void acq_shm_spy(void)
{
   int  error,i;
   static unsigned short readall[NUMSEM];
   static struct shm_use shm_spy;
   struct shm_use *spy = &shm_spy;

   if (Shm == NULL) return;
   if (shm_lock(&error)) return;
   shm_spy = *Shm;
   if (semctl(Ids.dat_sem,0,GETALL,readall) == -1) perror("spy");
   shm_unlock();
   printf("            log = %i\n",spy->log_interval);
   printf("       max size = %i\n",spy->max_buf_size);
   printf("           size = %i\n",spy->buf_size);
   printf("           bufs = %i\n",spy->num_buf);
   printf("          procs = %i\n",spy->num_proc);
   printf("          WRpid = %i\n",spy->WRpid);
   printf("          WRuse = %x\n",spy->WRuse);
   printf("write wait flag = %i\n",spy->WRwait);
   printf(" read wait flag = %i\n",spy->RDwait);
   printf("\nProc   PID     Avail     Used\n");
   for (i=0; i < NUMPROC; i++)
     {
       printf("%3i %6i  %8x %8x\n",i,spy->RDpids[i],spy->RDavail[i],
                                     spy->RDuse[i]);
     }
   printf("\n      Lock Semaphore: %hi\n",readall[SEMLOCK]);
   printf("Write Wait Semaphore: %hi\n",readall[WRWAKE]);
   printf("Read Wait Semaphore Set:  ");
   for (i=0; i < NUMPROC; i++) printf(" %hi",readall[RDWAKE+i]);
   printf("\n");
}
/******************************************************************************
*
*   Clear RDavail mask for all procs.  This is equivalent to clear all the
*   buffers for read.  Designed for use with the pacman command 'ZBUF'.
*
* Returns:  0 ok, else error
*
******************************************************************************/
int zbuf_shm(void)
{
  int i, error;
  volatile unsigned int *lavail;

  if (shm_lock(&error)) return error;

  lavail = RDavail;
  for (i= 0; i < NUMPROC; i++)          /* doesn't hurt to clear the ones */
    {
     *lavail ^= *lavail;              /*  that already cleared */
     *lavail++;
    }

  shm_unlock();
  return 0;
}
