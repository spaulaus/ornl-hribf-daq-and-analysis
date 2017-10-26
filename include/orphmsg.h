#ifndef ORPHMSG_H
#define ORPHMSG_H
#include <inttypes.h>
/******************************************************************************
*
*   Include file for message output to the logger process.
*
******************************************************************************/

/*  Structure of buffer sent to the system message queue facility  */
struct orphmsg {
         long  type;       /* message type. Must be > 0 */
         char  text[136];  /* ASCII message             */
};

#define ORPHAS_SIZE  (sizeof(struct orphmsg) - sizeof(long))

/*   Message types   */

#define  MSG_INFORM  1
#define  MSG_WARN    2
#define  MSG_PANIC   3

/*
*   The array text[] can really be anything you want.  However, others
*   have adpoted a common format.  The Format is as follows:
*
*   Sender name  -  8 characters.  Usually the name of program that sent
*                   the message.
*   1 space
*
*   Time stamp   -  18 characters.  'dd-Mon-yy hh:mm:ss'
*
*   2 spaces
*
*   User text    -  Upto 104 characters.
*
*   NULL char
*
*
*   The following defines may assist you in getting messages into this
*   format.
*/
#define  MSG_SEND_COL  1
#define  MSG_SEND_LEN  8
#define  MSG_TIME_COL  10
#define  MSG_TIME_LEN  18
#define  MSG_USER_COL  30
#define  MSG_USER_LEN  104

/*****************************************************************************
*
*   Structure of message buffers received from the VME processor.
*
*****************************************************************************/
struct VMEmsg {
              int  type;       /* Message type: 1 = inform, 2 = warn,   */
                               /*  3 = panic                            */
              char sender[9];  /* Sender program name - NULL terminated */
              char text[105];  /* User text message - NULL terminated   */
};

/*****************************************************************************
*
*    FORTRAN input  message queue definitions
*
*****************************************************************************/


#define   FTN_SIZE  81
struct fortinmsg {
           long  type;
           char  text[FTN_SIZE];
};

#endif    /*  end ORPHMSH_H  */
