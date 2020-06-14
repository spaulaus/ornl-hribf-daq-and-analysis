/*-----------------------------------------------------------------------------
    ipclemo.c 
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

#define  ACQ
#include  "../Dshm/ipcdefs.h"
#include  "acqshm.h"
#include  "acqlib.h"

extern struct  {
        double first_evt;
        double num_evts;
        double buf_num; } orphas_;
static char name[20],mess[78];
static int opendir = O_READ;
extern int luin;

/****************************************************************************
****************************************************************************/
void openipc_(char *direction, char *device, int *bufsize, int *ierr)
{
/*  Map to the IPC region and information      */
    if (*direction == 'w' || *direction == 'W')
     {
       create_acq_ipc_(device,bufsize,ierr,20);
       strncpy(name,device,sizeof(name));
       if (*ierr != 0)
         {
           acq_error(ierr,mess,sizeof(mess));
           printf("%s\n",mess);
           return;
         }
       else opendir = O_WRITE;
     }
    open_acq_ipc_(device,bufsize,ierr,20);
    if (*ierr != 0)
      {
        acq_error(ierr,mess,sizeof(mess));
        printf("%s\n",mess);
        return;
      }
    *ierr = 0;
    return;
}

/****************************************************************************
****************************************************************************/
/*  ipcclose - closes an open IPC stream for lemo.  The stream number
*/
void closeipc_()
{
   int ierr;

   if (opendir == O_READ) close_shm(luin);
   else remove_acq_ipc_(name,&ierr,sizeof(name));
}

/****************************************************************************
****************************************************************************/
/*  ipcread - reads a buffer from an IPC input stream.  */
void readipc_( unsigned char *buffer, /* Buffer destination */
              int  *nbuf,   /* Size available     */
              int  *nread,  /* Number bytes read  */
              int  *ierr,   /* Error number       */
              int  *msgf)   /* CTRL C abort flag  */
{
    unsigned int itotal,ibuf;
             int iseen;

    /* Go ahead and read */
    
    read_shm_(buffer, nbuf, nread,&itotal,&iseen,&ibuf,ierr,msgf);
    if (*ierr != 0)
      {
        acq_error(ierr,mess,sizeof(mess));
        printf("%s\n",mess);
        *nread = 0;
      }
    if (*nread == 0) *ierr = 999;
    if (*ierr == 0)
      {
        orphas_.first_evt = itotal;
        orphas_.num_evts = iseen;
        orphas_.buf_num = ibuf;
      }
    else  orphas_.num_evts = 0.0;

    return;
}

/****************************************************************************
****************************************************************************/
/*  ipcwrite - writes a buffer to an IPC output stream.  */
void writeipc_( unsigned char *buffer, /* Buffer destination */
               int  *nbuf,     /* Size available     */
               int  *nwrite,   /* Number bytes written  */
               int  *ierr)     /* Error number       */
{

    /* Go ahead and write */
    write_shm_(buffer, nbuf, nwrite, ierr);
    if (*ierr != 0)
      {
        acq_error(ierr,mess,sizeof(mess));
        printf("%s\n",mess);
        *nwrite = -1;
      }

    return;
}
/****************************************************************************
*
*  Get a pointer to a shared memory buffer.
****************************************************************************/
void shmread_(char **ptr,int *nbytes,int *ierr,int *abort)
{
   char *uptr;
   struct shm_buf *bufptr;

   if (luin == -1)
     {
       luin = open_shm(O_READ,ierr);
       if (*ierr != 0)
         {
            acq_error(ierr,mess,sizeof(mess));
            printf("%s\n",mess);
            *ptr = NULL;
            *nbytes = 0;
            return;
         }
     }
   uptr = *ptr;
   if (uptr != NULL) uptr -= offsetof(struct shm_buf,data[0]);  
   bufptr = read_shm(luin,(struct shm_buf *)uptr,ierr,abort);
   if (bufptr == NULL)
     {
       acq_error(ierr,mess,sizeof(mess));
       printf("shmread: %s\n",mess);
       *nbytes = 0;
       orphas_.num_evts = 0.0;
       return;
     }
   orphas_.first_evt = bufptr->event_num;
   orphas_.num_evts = bufptr->events;
   orphas_.buf_num = bufptr->buf_num;
   *nbytes = bufptr->size;
   uptr = (char *)bufptr;
   uptr += offsetof(struct shm_buf,data[0]);
   *ptr = uptr;
}
/****************************************************************************
*
*  Release buffers in shared memory.
****************************************************************************/
void shmrelease_(int *numbuf, char **ptr)
{
   int  i,num;
   char *uptr;

   num = *numbuf;
   for (i=0; i < num; i++)
     {
       if (*ptr != NULL)
         {
           uptr = *ptr;
           uptr -= offsetof(struct shm_buf,data[0]); 
           i = free_read_buf(luin,(struct shm_buf *)uptr);
           if (i != 0)
             {
               acq_error(&i,mess,sizeof(mess));
               printf("shmread: %s\n",mess);
             }
           *ptr = NULL;
         }
       ptr++;
     }
}
