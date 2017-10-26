/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  
*
*    File:         /usr/users/mcsq/Dshm/create_shm.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 6/93    MCSQ         Original
*****************************************************************************/

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "ipcdefs.h"

#ifdef __ultrix
/*
*    Function Prototypes for ULTRIX
*/
/*
*  Include ipc function prototypes here since the calling
*  argument types are not elsewhere defined.
*/

extern int shmget(key_t,int,int);
extern int shmctl(int,int,struct shmid_ds *);
extern char *shmat(int,void *,int);
extern int shmdt(void *);
#endif

/******************************************************************************
******************************************************************************/
int create_shm(key_t key,int size,int *error)
{
               int  shmid;
   struct shmid_ds  shmstat;

   *error = 0;
/*
*   First we try to create an exclusive shared-memory segment.  If that
*   is successful, we are done.
*/
   shmid = shmget(key,size,IPC_CREAT | IPC_EXCL | 0660);
   if (shmid != -1)
     {
       return shmid;      /* Created a new shared-memory segment */
     }
   else
     {
       if (errno != EEXIST)
         {
/*
*   A shared memory segment with the key did not exist but shmget
*   failed.  Test why it failed and return error code.
*/
           switch  (errno)
            {
              case EINVAL:
                *error = TOOLARGE;
                break;
              case ENOSPC:
                *error = NOTBLSPACE;
                break;
              case ENOMEM:
                *error = NOMEMORY;
                break;
              default:
                perror("create_shm__shmgeta");
                *error = -1;
            }
           return (-1);
        }
     }
/*
*   We know that a segment with our key exists.  Check to see if
*   we can use it.
*/
   shmid = shmget(key,0,0660);
   if (shmid == -1)
     {
       switch (errno)
        {
          case EPERM:
            *error = NOPERM;
            break;
          case ENOENT:
            shmid = create_shm(key,size,error);
            return shmid;
          default:
            *error = -1;
            perror("create_shm_shmgetb");
            return -1;
        }
     }
/*
*   There's still hope.  Get status of the shared memory segment.
*/
   if (shmctl(shmid,IPC_STAT,&shmstat) == -1)
     {
       perror("create_shm_shmctla");
       exit(99);
     }
/*
*   Check for ownership.  
*/
   if (shmstat.shm_perm.uid != geteuid())
     {
/*
*   We don't own this segment but we can use it if the size is correct.
*/
       *error = NOTOWNER;
/*
       if (size == shmstat.shm_segsz) return shmid;
*/
       return -1;
     }
/*
*   We own this memory segment.  If it is the correct size, just return.
*   Otherwise, remove the segment and create a new one.
*/
   if (size == shmstat.shm_segsz) return shmid;
   if (shmctl(shmid,IPC_RMID,NULL) == -1)
     {
       perror("create_shm_shmctlb");
       exit(99);
     }
   shmid = create_shm(key,size,error);
   return shmid;
}
