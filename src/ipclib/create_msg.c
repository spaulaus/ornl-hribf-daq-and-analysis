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
*    File:         /usr/users/mcsq/Dshm/create_msg.c
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
#include <sys/msg.h>
#include "ipcdefs.h"

#ifdef __ultrix
/*
*    Function Prototypes for ULTRIX
*/
/*
*  Include ipc function prototypes here since the calling
*  argument types are not elsewhere defined.
*/

extern int msgget(key_t,int);
extern int msgctl(int,int,struct msqid_ds *);
extern int msgrcv(int,void *,int,int,int);
extern int msgsnd(int,void *,int,int);
#endif

/******************************************************************************
******************************************************************************/
int create_msg(key_t key,int *error)
{
               int  msgid;
   struct msqid_ds  msgstat;

   *error = 0;
/*
*   First we try to create an exclusive message queue.  If that
*   is successful, we are done.
*/
   msgid = msgget(key,IPC_CREAT | IPC_EXCL | 0660);
   if (msgid != -1)
     {
       return msgid;      /* Created a new message queue */
     }
   else
     {
       if (errno != EEXIST)
         {
/*
*   A message queue with the key did not exist but msgget
*   failed.  Test why it failed and return error code.
*/
           switch  (errno)
            {
              case ENOSPC:
                *error = NOTBLSPACE;
                break;
              default:
                perror("create_msg__msggeta");
                *error = -1;
            }
           return (-1);
        }
     }
/*
*   We know that a message queue with our key exists.  Check to see if
*   we can use it.
*/
   msgid = msgget(key,0660);
   if (msgid == -1)
     {
       switch (errno)
        {
          case EPERM:
            *error = NOPERM;
            break;
          case ENOENT:
            msgid = create_msg(key,error);
            return msgid;
          default:
            *error = -1;
            perror("create_msg_msggetb");
            return -1;
        }
     }
/*
*   There's still hope.  Get status of the message queue.
*/
   if (msgctl(msgid,IPC_STAT,&msgstat) == -1)
     {
       perror("create_msg_msgctla");
       exit(99);
     }
/*
*   Check for ownership.
*/
   if (msgstat.msg_perm.uid != geteuid())
     {
/*
*   We don't own this message queue.
*/
       *error = NOTOWNER;
       return -1;
     }
   return msgid;
}
