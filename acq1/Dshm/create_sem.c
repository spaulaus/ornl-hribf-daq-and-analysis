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
*    File:         /usr/users/mcsq/Dshm/create_sem.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/12/93    MCSQ         Original
*****************************************************************************/

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "ipcdefs.h"

#ifdef __ultrix
/*
*    Function Prototypes for ULTRIX
*/
/*
*  Include ipc function prototypes here since the calling
*  argument types are not elsewhere defined.
*/
extern int semget(key_t,int,int);
extern int semctl(int,int,int,struct semid_ds *);

#endif

/******************************************************************************
******************************************************************************/
int create_sem(key_t key,int count,int *error)
{
               int  semid,sizerr = 0;
   struct semid_ds  semstat;

   *error = 0;
/*
*   First we try to create an exclusive semaphore set.  If that
*   is successful, we are done.
*/
   semid = semget(key,count,IPC_CREAT | IPC_EXCL | 0660);
   if (semid != -1)
     {
       return semid;      /* Created a new semaphore set */
     }
   else
     {
       if (errno != EEXIST)
         {
/*
*   A semaphore set with the key did not exist but semget
*   failed.  Test why it failed and return error code.
*/
           switch  (errno)
            {
              case ENOSPC:
                *error = NOTBLSPACE;
                break;
              default:
                perror("create_sem_semgeta");
                *error = -1;
            }
           return (-1);
        }
     }
/*
*   We know that a semaphore with our key exists.  Check to see if
*   we can use it.
*/
   semid = semget(key,count,0660);
   if (semid == -1)
     {
       switch (errno)
        {
          case EPERM:
            *error = NOPERM;
            break;
          case ENOENT:
            semid = create_sem(key,count,error);
            return semid;
          case EINVAL:
            sizerr = 1;
            break;
          default:
            *error = -1;
            perror("create_sem_semgetb");
            return -1;
        }
     }
/*
*   There's still hope.  Get status of the message queue.
*/
   if (semctl(semid,0,IPC_STAT,&semstat) == -1)
     {
       perror("create_sem_semctla");
       exit(99);
     }
/*
*   Check for ownership.
*/
   if (semstat.sem_perm.uid != geteuid())
     {
/*
*   We don't own this message queue.
*/
       *error = NOTOWNER;
       return -1;
     }
   if (sizerr == 0) return semid;
   if (semctl(semid,0,IPC_RMID,NULL) == -1)
     {
       perror("create_sem_semctlb");
       exit(99);
     }
   semid = create_sem(key,count,error);
   return semid;
}
