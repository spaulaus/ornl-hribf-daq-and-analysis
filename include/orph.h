#ifndef  ORPH_H_
#define  ORPH_H_
/*
*  Adapted from /usr/users/mcsq/Dlan/orph_pf.h for use with VME processor
*  codes.
*/
/*   orph.h  -  header file of definitions for VME to DECStation
     Ethernet protocol. 
     Robert Varner Feb 1992                                            */
/* version 1.1 12 August 1992 */

/*    Some sizes and limits */
#define MAX_ORPH_DATA 1492    /* byte length of packet - header 	*/
#define MIN_ORPH_DATA   44
#define ORPH_HDR_LEN    20    /* byte length of address+protocol 	*/
#define HW_ADDR_LEN      6    /* Length of hardware address in bytes 	*/
#define PROT_SIZE        2    /* bytes in protocol  			*/
#define MAX_PACKET    1512    /* max packet size                        */
#define MIN_PACKET      64    /* min packet size                        */
/*    Parameters of acknowledgement protocol */
#define MAX_RETRIES      5    /* number of times to try during Reliable */
#define ACK_TMO          2    /* seconds to wait for ACK		*/
/*    Possible values for Ack field in packet header */
#define NAKPKT      2    /* Negative ACK to PLEASEACK	 		*/
#define ACKPKT      1    /* Packet acknowledges receipt of message 	*/
#define ACK        -1    /* Request needs to be acknowledged 		*/
#define NOACK       0    /* Requires unreliable transport 		*/
/*    Return states from functions  */
#define SUCCESS          1
#define FAILURE         -1
#define TIMEOUT          0
#define RETRY           -2

/*   Define our Ethernet packet and headers          */
     /*    The order of this structure is IMPORTANT. 
           Request_number MUST be last               */
struct Packet_Header {
   unsigned char Destination[HW_ADDR_LEN]; /* Packet destination */
   unsigned char Source[HW_ADDR_LEN];	/* Packet source      */
   unsigned char Protocol[PROT_SIZE];	/* Protocol number    */
   char Ack;      /* Reliable flag: 0->no ack, 1->is Ack, -1->needs Ack */
   unsigned char Order;                 /* Order in sequence of packets */
   unsigned int  Request_Number;        /* Requestor ID for reply */
};

struct Ether_Packet {
   unsigned char Destination[HW_ADDR_LEN]; /* Packet destination */
   unsigned char Source[HW_ADDR_LEN];   /* Packet source      */
   unsigned char Protocol[PROT_SIZE];   /* Protocol number    */
   char Ack;                   /* Reliable Acknowledgement flag */
   unsigned char Order;                 /* As in Packet_Header */ 
   unsigned int  Request_Number;        /* Requestor ID for reply */

   unsigned char Data[MAX_ORPH_DATA];	/* the data to be transmitted */
};

/*    Definitions for protocols, messages and states  */

/*     Ethernet Protocols */
#define PROTO_PREFIX   0x4f	/* 1st octet of protocol same for all */

#define PROTO_DATA	0x50	/* Event Data */
#define PROTO_REQUEST_BOOT	0x51	/* VME requests to be booted */
#define PROTO_FORCE_BOOT	0x52	/* Force VME to reboot  */
#define PROTO_CODE	0x53	/* Code download to VME		*/
#define PROTO_FASTBUS	0x54	/* Exec FastBus command 	*/
#define PROTO_CNAF	0x55	/* Exec CNAF for CAMAC 		*/
#define PROTO_FECNTRL	0x56	/* Control messages from WS 	*/
#define PROTO_FEMSG	0x57	/* FrontEnd Messages to WS	*/
#define PROTO_TEST      0x58    /* Protocol for testing         */
#define PROTO_SOFT	0x59	/* Message to/from software in 
                                   front-end                    */
#define PROTO_VMEIO     0x5d    /* Control of VME acq modules   */
#define PROTO_LNFILL    0x5e    /* LN filling system            */
#define PROTO_RMSSIO    0x5f    /* RMS/DRS control systems      */

#endif      /* end  ORPH_H_   */
