#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <time.h>

/* #include  "system.h"  */
/* #include  "vmeprom.h" */
/* #include  "lnfvme.h"  */
#include  "vme_sys.h"
#include  "orph.h"
#include  "devices.h"
#include  "ln_alarm.h"

#define  EVT1  LNFXX_ERR  /* event flag set by the filling system. */

/*    Function Prototypes        */
void host_message(int ,char *);
void vmic_init(void);
void ch2_xmit(void);
void ch3_xmit(void);
void ch2_recv(void);
void ch3_recv(void);
void ch2_special(void);
void ch3_special(void);
void ch2_ext(void);
void ch3_ext(void);
void clr_recv_buf(struct recvbuf *);
void clr_xmit_buf(struct xmitbuf *);
int  get_recv_count(struct recvbuf *);
int  get_xmit_count(struct xmitbuf *);
int  put_xmit_char(int , unsigned char );
int  get_xmit_char(struct channel_data *, unsigned char *);
int  put_recv_char(struct channel_data *, unsigned char , unsigned char *);
int  get_recv_char(int , unsigned char *,unsigned char *);
void get_time(void);

/*   Global data */
/* static struct devices *devtbl = DEVTBL; */
static struct devices *devtbl = DEVTBL;
struct channel_data channel[4];
char   ascii_time[22];   /* array for time and date                          */
char   error_msg[105];   /* message buffer for host messages                 */
int    hardware_ok;      /* 0 means one or more hardware modules missing     */
int    hardware_status;  /* bit encoded status word.  There are bits for     */
                         /* each VME module type and one for disabled by     */
                         /* operator.                                        */

/******************************************************
 * Initialize VMIC 6015 Quad-Serial I/O Module
 */
void vmic_init(void)
{
  struct vmic_ch *vmic;
  int  i;

  /*  Initialize transmit buffer empty vectors */
  *((void (**)(void))((VMIC_VECS+12)*4)) = ch2_xmit;
  *((void (**)(void))((VMIC_VECS+8)*4)) = ch3_xmit;

  /*  Initialize the receive character interrupt vectors */
  *((void (**)(void))((VMIC_VECS+14)*4)) = ch2_recv;
  *((void (**)(void))((VMIC_VECS+10)*4)) = ch3_recv;

  /*  Initialize the special receive interrupt vectors */
  *((void (**)(void))((VMIC_VECS+15)*4)) = ch2_special;
  *((void (**)(void))((VMIC_VECS+11)*4)) = ch3_special;

  /*  Initialize the external/status change interrupt vectors */
  *((void (**)(void))((VMIC_VECS+13)*4)) = ch2_ext;
  *((void (**)(void))((VMIC_VECS+9)*4)) = ch3_ext;

  vmic = (struct vmic_ch *)VMIC6015A;
  vmic += 2;
  for (i=2; i<4; i++) {
    vmic->cmdreg = VMIC_CH_RESET;  /* Reset channel  */
    /*  Setup for Asynchronous mode, X16 clock, no parity and 1 stop bit */
    vmic->modectl = 0x40 | VMIC_STOP1;
    /*  Set for 19200 baud and enable baud rate generator. */
    /* vmic->tcreg = channel[i].tcreg = VMIC_9600; */
    vmic->tcreg = channel[i].tcreg = 4;
    vmic->brgctl = channel[i].brgctl = 0xd;
    vmic->xmtctl = VMIC_8BITS;
    vmic->rcvctl = VMIC_8BITS;
    if (!(i % 2)) vmic->vectrg = VMIC_VECS + i * 4;
    vmic->intctl = channel[i].intctl = 0x16;
    vmic->rcvctl |= VMIC_RXENA;
    channel[i].rcvctl = vmic->rcvctl;
    vmic->xmtctl |= VMIC_TXENA;
    channel[i].xmtctl = vmic->xmtctl;
    clr_recv_buf(&channel[i].ibuf);
    clr_xmit_buf(&channel[i].obuf);
    channel[i].vmic = vmic;
    channel[i].mask = 0x7f;
    channel[i].xcnt = sizeof(channel[i].ibuf.buf)/sizeof(struct indata) - 80;
    channel[i].xflg = 0;
    channel[i].xidle = 0;
    vmic++;
  }
}
/**********************************************************/
void ch2_xmit(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[2];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char data;

  if (get_xmit_char(chan,&data)) vmic->datarg = data;
  else {
    vmic->cmdreg = VMIC_RESET_TXPEND;
    chan->xidle = 0;
  }
}
/***********************************************************/
void ch3_xmit(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[3];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char data;

  if (get_xmit_char(chan,&data)) vmic->datarg = data;
  else {
    vmic->cmdreg = VMIC_RESET_TXPEND;
    chan->xidle = 0;
  }
}
/*************************************************************/
void ch2_recv(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[2];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status = 0,rdata;

  rdata = vmic->datarg & chan->mask;
  put_recv_char(chan,rdata,&status);
  if (status != 0) chan->err |= status;
}
/************************************************************/
void ch3_recv(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[3];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status = 0,rdata;

  rdata = vmic->datarg & chan->mask;
  put_recv_char(chan,rdata,&status);
  if (status != 0) chan->err |= status;
}
/*************************************************************/
void ch2_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[2];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*************************************************************/
void ch3_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[3];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*************************************************************/
void ch2_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[2].vmic;
  unsigned char dat;

  channel[2].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/**************************************************************/
void ch3_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[3].vmic;
  unsigned char dat;

  channel[3].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*************************************************************/
void clr_recv_buf(struct recvbuf *bufptr)
{
  int i,level;

  level = set_intr_level_(0x700);
  bufptr->input = bufptr->output = &bufptr->buf[0];

  for (i=0; i < IN_BUF_SIZE; i++) {
    bufptr->buf[i].err = 0;
    bufptr->buf[i].data = 0;
  }
  set_intr_level_(level);
}
/*************************************************************/
void clr_xmit_buf(struct xmitbuf *bufptr)
{
  int level;

  level = set_intr_level_(0x700);
  bufptr->input = bufptr->output = bufptr->buf;
  set_intr_level_(level);
}
/*************************************************************
 *  Get number of characters in the receive buffer.
 *  Call:  bufptr  -  pointer to receive buffer
 *  Return:   0  means buffer is empty
 *          > 0  buffer character count
 *           -1  means the buffer is full
 */
int get_recv_count(struct recvbuf *bufptr)
{
  int ic,oc;

  ic = bufptr->input - &bufptr->buf[0];
  oc = bufptr->output - &bufptr->buf[0];
  if (ic >= oc) ic = ic - oc;
  else ic = IN_BUF_SIZE -oc +ic;
  if (ic >= IN_BUF_SIZE - 1) ic = -1;
  return (ic);
}
/**************************************************************
 *  Get number of characters in the transmit buffer.
 *  Call:  bufptr  -  pointer to transmit buffer
 *  Return:   0  means buffer is empty
 *          > 0  buffer character count
 *           -1  means the buffer is full
 */
int get_xmit_count(struct xmitbuf *bufptr)
{
  int ic,oc;

  ic = bufptr->input - bufptr->buf;
  oc = bufptr->output - bufptr->buf;
  if (ic >= oc) ic = ic - oc;
  else ic = OUT_BUF_SIZE -oc +ic;
  if (ic >= OUT_BUF_SIZE - 1) ic = -1;
  return (ic);
}
/***************************************************************
 *  Put one character into the transmit buffer.  If the transmitter is idle,
 *  get one character from the buffer and start the transmitter.
 *  Call:   chan  -  output channel number
 *          data  -  character data
 *  Return:  0  means buffer is full.  Nonzero means OK.
 */
int put_xmit_char(int chan, unsigned char data)
{
  struct xmitbuf *ptr = &channel[chan].obuf;
  int count,level;

  level = set_intr_level_(0x700);
  count = get_xmit_count(ptr);
  if (count < 0) {
    set_intr_level_(level);
    return (0);
  }
  *ptr->input++ = data;
  if ((ptr->input - ptr->buf) >= OUT_BUF_SIZE) ptr->input = ptr->buf;
  if (count == 0 && channel[chan].xidle == 0) {
    get_xmit_char(&channel[chan],&data);
    channel[chan].xidle = 0xff;
    channel[chan].vmic->datarg = data;
  }
  set_intr_level_(level);
  return (1);
}
/*************************************************************
 *  Get one character from the transmit buffer.
 *  Call:   ptr  -  Pointer to transmit buffer structure
 *  Return:  0 means buffer is empty.  Nonzero means OK.
 *           data -  character data
 */
int get_xmit_char(struct channel_data *chan, unsigned char *data)
{
  int count;
  struct xmitbuf *ptr = &chan->obuf;

  count = get_xmit_count(ptr);
  if (!count) return 0;
  *data = *ptr->output++;
  if ((ptr->output - ptr->buf) >= OUT_BUF_SIZE) ptr->output = ptr->buf;
  return (1);
}
/************************************************************
 *  Put one character in the receive buffer.  Routine is called only by receive
 *  interrupt routines.
 *  Call:  ptr   -  pointer to the receive buffer structure
 *         data  -  the received character
 *         status - status bits from UART status register 1
 *  Return:  0  means buffer is full.  Nonzero means OK.
 */
int put_recv_char(struct channel_data *chan, 
		  unsigned char data, unsigned char *status)
{
  int count;
  struct recvbuf *ptr = &chan->ibuf;

  count = get_recv_count(ptr);   /* get buffer character count     */
  if (count < 0) {
    /*   Receiver buffer is full.  Put the Buffer overrun bit in the status
     *   and return buffer full status. */
    *status |= 0x8;
    (ptr->input)->err = *status;
    return (0);
  }
  if (chan->xflg && count >= chan->xcnt) {
    if (!chan->xidle) {
      chan->xidle = 0xff;
      (chan->vmic)->datarg = XOFF;
      chan->txoff = XOFF;
    }
    else  chan->nxchar = XOFF;
  }
  /*   Put the character and status in the buffer.  Increment the buffer
   *   pointer and return OK status. */
  (ptr->input)->data = data;
  (ptr->input)->err = *status;
  ptr->input++;
  if ((ptr->input - ptr->buf) >= IN_BUF_SIZE) ptr->input = ptr->buf;
  return (1);
}
/*********************************************************
 *  Get one character and the error status byte from a receive buffer.
 *  Call:   chan  -  Serial channel number(0 thru 7)
 *  return:   0  means buffer is empty.  Nonzero means valid char
 *          data   -  character
 *          status - error status
 */
int get_recv_char(int chan, unsigned char *data, unsigned char *status)
{
  struct recvbuf *ptr = &channel[chan].ibuf;
  int count;

/*  If the buffer is empty, just return buffer empty flag. */
  count = get_recv_count(ptr);
  if (!count) return 0;

/*  Return the character and status.  Increment buffer pointer and
 *   return OK status.
 */
  *data = (ptr->output)->data;
  *status = (ptr->output)->err;
  ptr->output++;
  if ((ptr->output - ptr->buf) >= IN_BUF_SIZE) ptr->output = ptr->buf;
  if (channel[chan].txoff && count < channel[chan].xcnt) ;
  return (1);
}
/*******************************************************
 *  Send a message to the Host.  If enabled, also output the message
 *  to the local terminal attached to the VME processor.
 *  These messages are sent to the host which booted the VME system.
 */
void host_message(int type,char *msg)
{
  int  status,level;
  time_t  tod;
  static struct VMEmsg *host_msg;
  static int  eno;
  static struct Ether_Packet out_pkt;

#ifdef  HOST_MSG
  if (!eno) {
    eno = open("ln1",3);
    if (eno <= 0) {
      printf("Can't open device 'ln'\n");
      exit(1001);
    }
    ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in
                                                the packet header */
    memcpy((char *)out_pkt.Destination,*host_ether_address,6);
    out_pkt.Order = 0;
    out_pkt.Protocol[0] = PROTO_PREFIX;
    out_pkt.Protocol[1] = PROTO_FEMSG;
    out_pkt.Ack = NOACK;
    host_msg = (struct VMEmsg *)out_pkt.Data;
    strcpy(host_msg->sender,"LN_FILL ");
  }
  host_msg->type = type;
  strcpy(host_msg->text,msg);
  status = write(eno,(char *)&out_pkt,sizeof(struct VMEmsg) + ORPH_HDR_LEN);
  if (status < 0) {
    printf("Write failure on device 'ln'\n");
    exit(1003);
  }
  level = set_intr_level_(0);
  delay_(2);
  set_intr_level_(level);
#endif

#ifdef  LOCAL_MSG
  time(&tod);
  strftime(ascii_time,20,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
  printf("%s%s\n",ascii_time,msg);
#endif
}

/**********************************************************/
int read_modem(sec)
int sec;
{
  static unsigned char modbuf[1024];
  unsigned char  *s, status;
  int  dialtime = 0, ret = 0;
  time_t now, last;

  s = modbuf;
  time(&last);

  while (dialtime < sec) {
    time(&now);
    if (last != now) dialtime += (now - last);
    last = now;

    delay_(3);
    while (get_recv_char(3,s,&status)) {
      if ( dialtime>12 && dialtime<17 &&
	   *(s-3)=='B' && *(s-2)=='U' && *(s-1)=='S' && *s=='Y')
	ret = 1;
      s++;
    }
  }
  return (ret);
}

/***** main main main main main main main main main main main *****/
void main(void)
{
  static char msg0[] = "\r\r";
  static char msg1[] = "ATZ\r";
  static char msg2[] = "ATDT2314468,,,,,,,,234#\r";
  int i, evt;
  struct vmic_ch *vmic;

  super_mode_();
  /*  Initialize the rs232 port. */
  hardware_ok = 1;
  hardware_status = 0;
  vmic_init();
  vmic = (struct vmic_ch *)VMIC6015A;
  vmic += 3;

  if (EVT1 != 0) clr_evt_(EVT1);
  while(1) {
/*
 *   Wait for Event
 *
 *   Event LNFXX_ERR is set by lnfxx.  It is set on any detector error EXCEPT
 *   short_fill_time.  It is also set for any manifold error, any hardware
 *   error and fill system disabled by user.  This code should clear Event
 *   LNFXX_ERR.  The fill system checks for any error every 15 minutes.
 *   Event LNFXX_ERR will repeat every 15 minutes until the error condition is
 *   cleared.
 *
 */
    evt = wait_evts_(EVT1,0);

    /* we have an error event; use modem to dial beeper */
    vmic->xmtctl |= 4;  /* set DTR to enable modem */
    channel[3].xmtctl = vmic->xmtctl;

    for (i=0; msg0[i] != '\0'; i++)
      put_xmit_char(3,msg0[i]);
    read_modem(2);

    for (i=0; msg1[i] != '\0'; i++)
      put_xmit_char(3,msg1[i]);
    read_modem(2);

    for (i=0; msg2[i] != '\0'; i++)
      put_xmit_char(3,msg2[i]);

    /* clear error event only if paging was successful */
    if (read_modem(18) == 1) clr_evt_(EVT1);

    vmic->xmtctl &= (255-4);  /* reset DTR to disable modem */
    channel[3].xmtctl = vmic->xmtctl;
  }
}

