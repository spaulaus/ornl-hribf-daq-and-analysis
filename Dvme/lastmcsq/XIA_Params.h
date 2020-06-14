/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1999
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
*    Environment:  VME based Data Acquisition System for XIA modules
*
*    File:         /usr/users/mcsq/Dvme3/DSSD_Params.h
*
*    Description:  Definitions of control tables for the VME based
*                  acquisition system.  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/14/99       MCSQ  
*
*****************************************************************************/

#ifndef  ACQ_PARAMS_H_
#define  ACQ_PARAMS_H_

#ifndef  ORPH_H_
#include "orph.h"
#endif

/*
*    Structure of memory segment used for intertask communication.
*/
struct acq_share {
       unsigned int  event_number;
                int  avg_param_size;
                char Host_Ether_Adr[6];
               short FB_enabled;
               short FB_error;
               short KSC3982_enabled;
               short acqrun;
                int  spare[250];
               short testrun;
  };


#define ACQ_SHARED_RAM 0x20000      /* Start of a structure used for passing
                                       data between tasks                 */

#define ACQ_RAM        0x21002      /* Start address of parameter tables  */
#define ACQ_MAX_RAM    ((131072-4098)/4)

#define NUM_XIA  19        /* Maximum number of XIA modules */

/*
*    Data buffer structure and parameters
*/
#define  AVG_PARAMS       1
#define  MAX_PKT_DATA     (MAX_ORPH_DATA - 8)
#define  PKTS_PER_BUF     40
#define  MAX_PARAMS       700
#define  MAX_DATA_PARAMS  (8200 * NUM_XIA)

/*
*    Data buffer descriptor
*/
struct acq_buf_hdr {
    unsigned short  *str_buf;   /* pointer to event data start in buffer    */
    unsigned short  *end_buf;   /* pointer to end of event data in buffer   */
    unsigned short  *last_event;   /* pointer to previous event data        */
               int  totalevents;   /* event number                          */
               int  events;     /* number of events in this buffer          */
               int  ack;        /* buffer acknowledge - zero = acked        */
               int  busy;       /* buffer busy flag - negative = Busy       */
} ;

struct data_buf {
     struct acq_buf_hdr  Bufhdr;        /* information on buffer content    */
    struct Packet_Header Head;          /* Ether packet header              */
                    int  TotalEvents;   /* event number of first pkt event  */
         unsigned short  Events;        /* number of events this packet     */
         unsigned short  Cont;          /* Continuation flag for large evts */
         unsigned short  Data[MAX_DATA_PARAMS];   /* event data             */
};

/*
*   Command messages to the Acquisition task
*/
struct cntl_msg {
             char  flag;
             char  msg;
             char  reply;
};

#endif          /* end  ACQ_PARAMS_H_   */
