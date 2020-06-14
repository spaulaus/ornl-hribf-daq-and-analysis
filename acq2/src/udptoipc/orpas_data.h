#ifndef  ORPAS_DATA_H
#define  ORPAS_DATA_H

/* orpas_data.h
 *  Structure of Ethernet DATA messages from front-end to workstation.
 *  Used by any client sending orpas events on the network 
 */

/*  The TotalEvents is global information for deadtime/efficiency
    calculations and should include the number of events in the current
    buffer.  NumberEvents is the number of events in the current
    data buffer.  Continuation is normally 0. The Buffer is just an
    address for the rest of the data packet, which should be event buffers.
    Events should never span packet boundaries, except when one event is
    larger than the maximum packet size.  In this case, Continuation
    should be 1 and NumberEvents 0.  The next packet should be the 
    continuation of this one.  It should have Continuation set to 2.  
    Continuation should be incremented for each subsequent packet.  
    The last continuation packet will have FFFF at the end of the data,
    no more events in that packet, and NumberEvents set to 1.

    If the acquisition system stops, it should send a packet with NumberEvents
    set to 0, and an FFFF for the first and only word.
*/


#define DATAHDRSZ 8         /* Bytes of header in data packets */
struct orpas_data {
     int TotalEvents;             /* Total number of events sent */
     unsigned short NumberEvents;  /* Number of bytes in this buffer */
     unsigned short Continuation; /* Flag for event spanning many buffers */
     unsigned short Buffer;       /* start of data buffer */
} ;

#endif   /*  ORPAS_DATA_H  */
