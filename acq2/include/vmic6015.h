/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1994
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
*    Environment: RMS Control System
*
*    File:         /usr/users/mcsq/Dvme3/vmic6015.h
*
*    Description:  Include file for a VME Microsystems International Corp.
*                  Model 6015, Quad-Serial Input/Output Interface module.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/21/94    MCSQ         
*
*    6/12/94    MCSQ      Added definitions for Group 3 Teslameters.
*
*    9/25/94    MCSQ      Added definitions for Alpha Power Supplies
*****************************************************************************/
#ifndef  VMIC6015_H_
#define  VMIC6015_H_

/*
*    Hardware definitions for the VMIC 6015 Quad-Serial I/O VME module
*/
#define  CH0  0x00   /* Channel 0 offset.  P3 connector*/
#define  CH1  0x20   /* Channel 1 offset.  P4 connector*/
#define  CH2  0x40   /* Channel 2 offset.  P5 connector*/
#define  CH3  0x60   /* Channel 3 offset.  P6 connector*/
#define  BIM  0x80   /* 68153 BIM interrupt offset */

/*
*   Register structure for one channel of a MK68564 Serial input/output
*   chip used in the 6015 module.
*/
struct vmic_ch {
                   char dum0;
 volatile unsigned char cmdreg;  /* Command register                         */
                   char dum1;
          unsigned char modectl; /* Mode control register                    */
                   char dum2;
          unsigned char intctl;  /*Interrupt control register                */
                   char dum3;
          unsigned char sync1;   /* Sync word register 1                     */
                   char dum4;
          unsigned char sync2;   /* Sync word register 2                     */
                   char dum5;
          unsigned char rcvctl;  /* Receiver control register                */
                   char dum6;
          unsigned char xmtctl;  /* Transmitter control register             */
                   char dum7;
 volatile unsigned char stat0;   /* Status register 0                        */
                   char dum8;
 volatile unsigned char stat1;   /* Status register 1                        */
                   char dum9;
 volatile unsigned char datarg;  /* Data register                            */
                   char dum10;
          unsigned char tcreg;   /* Time constant register                   */
                   char dum11;
          unsigned char brgctl;  /* Baud rate generator control register     */
                   char dum12;
          unsigned char vectrg;  /* Vector register                          */
                   char dumx[6];
};

/*
*  Register structure for the MC68153 Bus Interrupter Module.  Only
*  control registers 2 and 3 are used.
*/
struct vmic_bim {
                   char dum0;
          unsigned char cr0;
                   char dum1;
          unsigned char cr1;
                   char dum2;
          unsigned char cr2;     /* Only control registers 2 and 3 are used */
                   char dum3;
          unsigned char cr3;
                   char dum4;
          unsigned char vr0;
                   char dum5;
          unsigned char vr1;
                   char dum6;
          unsigned char vr2;
                   char dum7;
          unsigned char vr3;
};
/*
*   Baud rate parameters for the TCREG (Time constant register).
*   Following setup is assumed:
*     a) Clock mode is X16.  See register MODECTL.
*     b) D1 of the BRGCTL register is zero.  i.e. divide by 4 mode.
*/
#define  VMIC_9600  8
#define  VMIC_4800  16
#define  VMIC_2400  32
#define  VMIC_1200  64
/*
*   Bits per character, used for receive and transmit setup
*/
#define  VMIC_7BITS 0x80
#define  VMIC_8BITS 0xc0
/*
*   Definitions for receiver setup
*/
#define  VMIC_RXENA    1
#define  VMIC_PAR_ENA  1
#define  VMIC_PAR_ODD  0
#define  VMIC PAR_EVEN 2
#define  VMIC_STOP1    0x04
#define  VMIC_STOP2    0x0c
/*
*  Definitions of transmit setup
*/
#define  VMIC_TXENA  1
#define  VMIC_TXRTS  2
#define  VMIC_TXDTR  4
/*
*  Staus register 0 definitions
*/
#define  VMIC_S0_RXRDY    1
#define  VMIC_S0_INTRPEND 2
#define  VMIC_S0_TXEMPTY  4
#define  VMIC_S0_DCD      8
#define  VMIC_S0_CTS      0x10
#define  VMIC_S0_BREAK    0x80
/*
*  Staus register 1 definitions
*/
#define  VMIC_S1_TXDONE     1
#define  VMIC_S1_PARITY_ERR 0x10
#define  VMIC_S1_OVER_ERR   0x20
#define  VMIC_S1_FRAME_ERR  0x40
/*
*  Command definitions
*/
#define  VMIC_RESET_EXT    0x10
#define  VMIC_CH_RESET     0x18
#define  VMIC_RESET_TXPEND 0x28
#define  VMIC_ERR_RESET    0x30

/*
*   Data structures for serial I/O channels
*/
#define  NUM_CHAN     8
#define  IN_BUF_SIZE  1024
#define  OUT_BUF_SIZE 128

struct indata {
      unsigned char err;
      unsigned char data;
} ;

struct recvbuf {
      struct indata *input;
      struct indata *output;
      struct indata buf[IN_BUF_SIZE];
} ;

struct xmitbuf {
      unsigned char *input;
      unsigned char *output;
      unsigned char buf[OUT_BUF_SIZE];
} ;

struct channel_data {
      struct vmic_ch *vmic;  /* Pointer to channel hardware registers     */
                char xflg;   /* XON/XOFF protocol flag.                   */
       unsigned char xcnt;   /* char count above which to send XOFF       */
       unsigned char nxchar;
                char rxoff;  /* Receive XOFF flag                         */
                char txoff;  /* Have sent XOFF flag                       */
                char xidle;  /* Transmit interrupt flag                   */
       unsigned char err;    /* Channel error flag                        */
       unsigned char mask;   /* receive character mask                    */
       unsigned char modectl; /* */
       unsigned char intctl;
       unsigned char rcvctl;
       unsigned char xmtctl;
       unsigned char tcreg;
       unsigned char brgctl;
      struct recvbuf ibuf;   /* Receive buffer                            */
      struct xmitbuf obuf;   /* Transmit buffer                           */
};
#define  XON  0x11
#define  XOFF 0x13

#endif     /* end VMIC6015_H_    */
