/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1992-1994
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dwks/tape/fortinput.c
*
*    Description:  FORTRAN callable routine to get messages from the
*                  message queue system.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 4/92    MCSQ         
*
*    2/ 8/93    MCSQ       Convert FORTRAN message queue to bidirectional
*                          queue.  A call to this routine sends a message
*                          type 1 to prompt pacman for a command.  pacman
*                          sends commands a message type 2.
*****************************************************************************/
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#define  ACQ
#include "ipcdefs.h"
#include "orphmsg.h"

static struct msqid_ds statbuf;

static struct fortinmsg rcvmsg,sndmsg;

extern struct acq_ids Ids;

void frcvmsg_(char *buf,int *size,int *ierr)
{
    int msglen,status;
    char *xbuf = buf;

    if (Ids.tape_msg <= 0)
     {
       *ierr = 1;
       return;
     }
/*
*   Send a message requesting command input from pacman
*/
    sndmsg.type = 1;
    do
      {
        status=msgsnd(Ids.tape_msg, &sndmsg, 1, 0);
      }
    while (status == -1 && errno == EINTR);
    if (status == -1)
      {
        *ierr = 2;
        msgctl(Ids.tape_msg,IPC_RMID,&statbuf);
        return;
      }
/*
*    Wait for a command message.
*/
    do
      {
        msglen=msgrcv(Ids.tape_msg, &rcvmsg, FTN_SIZE, 2, 0);
      }
    while (msglen == -1 && errno == EINTR);
    if (msglen == -1)
      {
        *ierr = 2;
        msgctl(Ids.tape_msg,IPC_RMID,&statbuf);
        return;
      }
    if (msglen >= *size)
      {
        strncpy(buf,rcvmsg.text,*size);
      }
    else
      {
        strncpy(buf,rcvmsg.text,msglen);
        xbuf += msglen;
        while((msglen++) <= *size) *xbuf++ = ' ';
      }
    *ierr = 0;
    return;
} 
