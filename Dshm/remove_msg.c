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
*    File:         /usr/users/mcsq/Dshm/remove_msg.c
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
int remove_msg(key_t key,int *error)
{
   int  msgid,stat;

   msgid = attach_msg(key,error);
   if (msgid == -1) return (-1);
   stat = msgctl(msgid,IPC_RMID,NULL);
   if (stat == -1)
     {
       switch  (errno)
        {
          case EPERM:
           *error = NOTOWNER;
           break;
          default:
           perror("remove_msg_msgget");
        }
       return -1;
     }
   return msgid;
}
