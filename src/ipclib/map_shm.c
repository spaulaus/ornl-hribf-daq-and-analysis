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
*    File:         /usr/users/mcsq/Dshm/map_shm.c
*
*    Description:  
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 7/93    MCSQ         Original
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
void *map_shm(int shmid,int *error)
{
   void *ptr;

   *error = 0;
   ptr = shmat(shmid,NULL,0);
   if (ptr == (void *)-1)
     {
       switch  (errno)
        {
          case EACCES:
           *error = NOPERM;
           break;
          case EINVAL:
           *error = NOEXIST;
           break;
          case ENOMEM:
           *error = NOMEMORY;
           break;
          case EMFILE:
           *error = TOOMANY;
           break;
          default:
           perror("attach_shm_shmget");
        }
       return (void *)-1;
     }
   return ptr;
}
