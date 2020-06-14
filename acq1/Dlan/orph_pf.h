#ifndef  ORPH_PF_H_
#define  ORPH_PF_H_
/*   orph.h  -  header file of definitions for VME to DECStation
     Ethernet protocol. 
     Robert Varner Feb 1992                                            */
/* version 1.1 12 August 1992 */
/* Linux version  3/11/03     */

/*    Some sizes and limits */
#define MAX_ORPH_DATA 1480    /* byte length of packet - header 	*/
#define MIN_ORPH_DATA   44
#define ORPH_HDR_LEN    20    /* byte length of address+protocol 	*/
#define HW_ADDR_LEN      6    /* Length of hardware address in bytes 	*/
#define PROT_SIZE        2    /* bytes in protocol  			*/
#define MAX_PACKET    1500    /* max packet size                        */
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
/*    Values passed used to set requestor identification */
#define SERVER           0    /* Server requestor identification */
#define CLIENT    getpid()    /* Clients ident from process id   */
#define RECI_PKT         1
#define XMIT_PKT         2

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

struct ReadQueue {
                  int  len;
   struct Ether_Packet *pkt;
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
#define PROTO_TEST	0x58	/* Protocol for testing         */
#define PROTO_SOFT	0x59	/* Message to/from software in 
                                   front-end                    */
#define PROTO_VMEIO     0x5d    /* Control of VME acq modules   */
#define PROTO_LNFILL    0x5e    /* LN filling system            */
#define PROTO_RMSSIO    0x5f    /* RMS/DRS control systems      */


/*    Enumerated type of servers.  Makes some program logic simple */
 enum server_proto {DATA=0x50, REQUEST_BOOT, FORCE_BOOT, CODE, FASTBUS,
             CNAF, FECNTRL, FEMSG, TEST, SOFT ,SPARE1, SPARE2, SPARE3,
             VMEIO, LNFILL, RMSSIO};

/*    Prototypes                                                    */
int  en_read(int, struct Packet_Header *, unsigned char *);
int  en_write(int, struct Packet_Header *, unsigned char *, int);
int  en_open(char *, struct Packet_Header *, enum server_proto, unsigned int);
void en_close(int);
int  en_tmo(int,int);
int  en_snd_acknak(int, struct Packet_Header *, int);
void en_InitRecord(void);
int  en_FindPacket(struct Ether_Packet *);
void en_dump_records_(void);
void en_InitHis(void);
void en_AddHis(struct Ether_Packet *, int);
void en_dump_his_(void);
char *en_get_dest(int , char *[], int );
unsigned char *en_name_to_addr(char *);
unsigned char *en_name_to_interf(char *);

/*
*   Multicast address used by VME processors which are ready for boot
*/

#define  MULTICAST   {0x03,0x6d,0x63,0x73,0x71,0x00}

#endif      /* end  ORPH_PF_H_   */
