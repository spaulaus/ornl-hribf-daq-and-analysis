/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    Environment:  VME based Data Acquisition System.
*
*    File:         /usr/users/mcsq/Dvme3/lan.h
*
*    Description:  Ethernet hardware and software parameters.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/15/92    MCSQ         
*
*    2/18/95    MCSQ       Added  ethernet ioctl function EIOGACKADR which
*                          returns the address of the Ack counter.
*
*    9/21/95    MCSQ       Changed Ack_ctr to Ack_flag.  What was a counter
*                          is now a flag.  If the Ack_flag is 1, an 
*                          acknowlegement packet is expected from the host.
*
*    3/ 2/97    MCSQ       Changed for the CPU-60.  Many changes in 
*                          ethernet data structures.
*****************************************************************************/
#ifndef   LAN_H_
#define   LAN_H_

/*   Ethernet parameters                                                  */

/*   Transmit/Receive ring status bit definitions                         */
#define  OWN  0x80         /* Set means chip owns buffer                  */
#define  ERR  0x40         /* Set means some error                        */
#define  STP  0x2          /* Start of packet flag                        */
#define  ENP  0x1          /* End of packet flag                          */

#ifndef  CPU60     /* for the CPU-40 use the following                    */


/*   Receive message descriptors                                          */
struct LAN_RMD {
   unsigned short ladr;    /* Buffer address in shared RAM - low 16 bits  */
   unsigned char  stat;    /* Receiver status byte                        */
   unsigned char  hadr;    /* High order address, should be zero          */
   short          bcnt;    /* negative buffer byte count                  */
   unsigned short mcnt;    /* message byte count                          */
  };
/*  Transmit message descriptors                                          */
struct LAN_TMD {
   unsigned short ladr;    /* Buffer address in shared RAM - low 16 bits  */
   unsigned char  stat;    /* Transmit status byte                        */
   unsigned char  hadr;    /* High order address, should be zero          */
   short          bcnt;    /* negative count of bytes in buffer           */
   unsigned short err;     /* Transmit error flags                        */
  };
/*   Initialization block                                                 */
struct LAN_INIT {
  unsigned short  mode;    /* Mode word                                   */
  unsigned char   padr[6]; /* Our physical address                        */
  unsigned char   ladr[8]; /* Logical address filter                      */
  unsigned short  rdra;    /* Base address of receive ring structure      */
  int             rlen:3;  /* Number of receive structures                */
  int             res1:13; /* Reserved.  Should be zero                   */
  unsigned short  tdra;    /* Base address of transmit ring structure     */
  int             tlen:3;  /* Number of transmit structures               */
  int             res2:13; /* Reserved.  Should be zero                   */
 };

#else              /* for the CPU-60 use the following                    */

/*   Receive message descriptors                                          */
struct LAN_RMD {
   unsigned char  stat;    /* Receiver status byte                        */
   unsigned char  hadr;    /* High order address - 8 bits                 */
   unsigned short ladr;    /* Buffer address in shared RAM - low 16 bits  */
   unsigned short mcnt;    /* message byte count                          */
   short          bcnt;    /* negative buffer byte count                  */
  };
/*  Transmit message descriptors                                          */
struct LAN_TMD {
   unsigned char  stat;    /* Transmit status byte                        */
   unsigned char  hadr;    /* High order address - 8 bits                 */
   unsigned short ladr;    /* Buffer address in shared RAM - low 16 bits  */
   unsigned short err;     /* Transmit error flags                        */
   short          bcnt;    /* negative count of bytes in buffer           */
  };
/*   Initialization block  CAUTION: MUST be word swapped before use!!!    */
struct LAN_INIT {
  unsigned short  mode;    /* Mode word                                   */
  unsigned char   padr[6]; /* Our physical address                        */
  unsigned char   ladr[8]; /* Logical address filter                      */
  unsigned short  lrdra;   /* Base address of receive ring structure      */
  int             rlen:3;  /* Number of receive structures                */
  int             res1:5;  /* Reserved.  Should be zero                   */
  int             hrdra:8; /*                                             */
  unsigned short  ltdra;   /* Base address of transmit ring structure     */
  int             tlen:3;  /* Number of transmit structures               */
  int             res2:5;  /* Reserved.  Should be zero                   */
  int             htdra:8; /*                                             */
 };
#endif

/*  Initialization mode word bit definitions                              */
#define PROM 0x8000        /* Promiscuous mode. Accept all packets        */
#define INTL 0x80          /* Internal loopback                           */
#define LOOP 0x8           /* Loopback                                    */

/*  LANCE Control Register bit definitions                                */
#define INEA 0x40          /* Interrupt enable                            */
#define RXON 0x20          /* Enable receiver                             */
#define TXON 0x10          /* Transmitter enable                          */
#define TDMD 0x8           /* Transmit immediate                          */
#define STOP 0x4           /* Stop LANCE controller                       */
#define STRT 0x2           /* Start LANCE controller                      */
#define INIT 0x1           /* Initialize LANCE controller                 */

/*  LANCE Status Register bit definitions                                 */
#define ERROR  0x8000      /* Any error                                   */
#define BABL   0x4000      /* Babble error                                */
#define CERR   0x2000      /* Collision error                             */
#define MISS   0x1000      /* Missed packet error                         */
#define MERR   0x800       /* Memory error                                */
#define RINT   0x400       /* Receiver interrupt                          */
#define TINT   0x200       /* Transmit interrupt                          */
#define IDON   0x100       /* Initialization done                         */
#define INTR   0x80        /* Interrupt flag                              */

#ifndef  CPU60     /* for the CPU-40 use the following                    */

/*  Control and Status structure                                          */
struct CRTL {
  unsigned short rdp;      /* CSR data register                           */
  unsigned short rap;      /* Register address. 0 = Command and status    */
 };                        /* 1 = initialization block low 16 bits addr   */
                           /* 2 = initialization block high bits addr     */
                           /* 3 = hardware control bits                   */


#else              /* for the CPU-60 use the following                    */

/*  Control and Status structure                                          */
struct CRTL {
  unsigned short rap;      /* Register address. 0 = Command and status    */
                           /* 1 = initialization block low 16 bits addr   */
                           /* 2 = initialization block high 16 bits addr  */
                           /* 3 = hardware control bits                   */
  unsigned short rdp;      /* CSR data register                           */
  unsigned short bdp;      /* BCR data register                           */
  unsigned short rst;      /* Read access resets LAN controller           */
 };

#endif

/*    Ethernet message format.  This format is used for messages passed   */
/*  to the ethernet driver by a user task.                                */
struct LAN_CALL {
  char              flag;  /* Set to 0 by call.  Set to 0x80 by service   */
  char              eno;   /* Ethernet file index                         */
  unsigned short    func;  /* Requested function                          */
  unsigned short    len;   /* Length (bytes) of data buffer if any        */
  void             *data;  /* pointer to data                             */
  int              status; /* return status                               */
};

/*   Structure for the ioctl call EIOCHKREC.                              */
struct chk_receive {
  int    status;         /* Returned status                               */
  unsigned char *Source; /* Pointer to ethernet address                   */
  int    Request_Number; /*  If zero, test for any packet, else check for */
  unsigned char   Order; /*  matching Request_Number and Order            */
};

/*   Ethernet Function codes                                              */
#define  E_INIT     1      /* Initialize the ethernet driver              */
#define  E_OPEN     2      /* Open ethernet file                          */
#define  E_CLOSE    3      /* Close ethernet file                         */
#define  E_READ     4      /* Read ethernet packet                        */
#define  E_WRITE    5      /* Send ethernet packet                        */
#define  E_SYS_CLOSE  6    /* Close ethernet - system call                */

/*   Ethernet ioctl codes                                                 */
#define  EIOSTIMEOUT  32   /* Set receive timeout                         */
#define  EIOGTIMEOUT  33   /* Get receive timeout                         */
#define  EIOSPROTO    34   /* Set protocol                                */
#define  EIOGPROTO    35   /* Get protocol                                */
#define  EIOPHYSADR   36   /* Get out physical address                    */
#define  EIOFLUSH     37   /* Flush receive buffers and clear Ack wait    */
#define  EIOCHKREC    38   /* Returns TRUE if packet available            */
#define  EIOGACKCTR   39   /* Returns count of expected acks              */
#define  EIOGACKADR   40   /* Returns address of ack counter              */

/*   Open Protocol list                                                   */
struct OPEN_PROTO {
  char             Task;        /* Requesting task number                 */
  unsigned char    Protocol;    /* Receive protocol byte                  */
  short            Ack_flag;    /* ACKs expected flag                     */
  struct LAN_CALL *Msg;         /* Message pointer from calling task      */
  unsigned uint32_t    Timeout;     /* Timeout period (ticks, i.e. 10 ticks/s */
  unsigned uint32_t    Tmo;         /* Ticks counter when timeout occurs.
                                   Zero means infinite wait!!             */
};

struct Lance {
   unsigned short status;       /* LANCE controller status register       */
   int            miss;         /* Missed packet counter                  */
   int            babl;         /* Transmit babble error counter          */
   int            merr;         /* Memory error counter                   */
   int            cerr;         /* Collision error counter                */
} ;

#define  E_FILES  20          /* number of ethernet file slots            */

#ifndef  CPU60     /* for the CPU-40 use the following                    */

/*    Pointer to our Ethernet Physical address                            */
#define our_ether_address   (unsigned char **)0xffc10002

/*    Pointer to the boot host Ethernet Physical address                  */
#define host_ether_address  (unsigned char **)0xffc10006

#else              /* for the CPU-60 use the following                    */

/*    Pointer to our Ethernet Physical address                            */
#define our_ether_address   (unsigned char **)0xffc08002

/*    Pointer to the boot host Ethernet Physical address                  */
#define host_ether_address  (unsigned char **)0xffc08006

#endif

#endif    /*     LAN_H_                                                   */
