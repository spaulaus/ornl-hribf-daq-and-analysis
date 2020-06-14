/****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    Environment:  Force CPU-40 with Eagle 01 and VMEPROM
*
*    File:         /usr/users/mcsq/Dvme3/lance.c
*
*    Description:  Ethernet driver task.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/ 3/92    MCSQ        Original
*
*    1/ 2/93    MCSQ        Add a test of the status byte for each received
*                           packet.  It appears that errors recorded here
*                           may not be in the controller status word when
*                           the interrupt occurs.  On any error, we send
*                           a NAK to the host.  Hopefully the host will
*                           retransmit the bad packet.
*
*    4/ 2/93    MCSQ        Modified for use of DMA controller to move
*                           packet data from DRAM to LAN RAM.  First of
*                           all, I could not get 16 transfers to work.
*                           Secondly, it seems that the CPU is blocked
*                           from memory access while the DMA is running.
*                           Therefore, presently, use of the DMA transfers
*                           with a data acquisition system is NOT recommended!
*
*    4/ 5/93    MCSQ        By combining the byte swap with the data copy
*                           to LAN_RAM, the data throughput was increased
*                           by approx. 7%.  This byte swap and copy is in
*                           swap_copy.s.  This routine is used ONLY for
*                           packets with protocol PROTO_DATA.
*
*    2/18/95    MCSQ        Added ioctl function EIOGACKADR which returns
*                           the address of the Ack counter.
*
*    3/ 7/97    MCSQ        Added code for the Force CPU60.
******************************************************************************/

#include  <stddef.h>
#include  <string.h>
#include  <errno.h>
#include  "vme_sys.h"
#include  "vmeprom.h"
#include  "lan.h"
#include  "orph.h"

#define  TRUE  1
#define  FALSE 0
/*
#define  DMA
*/

#define RLEN          5         /* Receive ring length - power of 2        */
#define TLEN          3         /* Transmit ring length - power of 2       */
#define NUM_RBUF      (1<<RLEN) /* Number of receiver buffers              */
#define NUM_TBUF      (1<<TLEN) /* Number of transmit buffers              */
#define RBUF_SIZE     1536      /* Receive buffer size (bytes)             */
#define TBUF_SIZE     1536      /* Transmit buffer size (bytes)            */
#define LAN_IBLK      0         /* Initialization block local address      */
#define LAN_REC_RING  0x100     /* Receive ring local address              */

/*     Pointers for buffer management                                      */
       struct LAN_RMD *rec_ptr;        /* Current receiver buffer          */
       struct LAN_RMD *rec_ptr_str;    /* First receive buffer             */
       struct LAN_RMD *rec_ptr_end;    /* Last + 1 receiver buffer         */
       struct LAN_RMD *rec_int_ptr;    /* Pointer to next receive buffer   */
       struct LAN_TMD *trans_ptr;      /* Current transmit buffer          */
static struct LAN_TMD *trans_ptr_str;  /* First transmit buffer            */
static struct LAN_TMD *trans_ptr_end;  /* Last + 1 transmit buffer         */

/*   Open channel database                                                 */
       struct OPEN_PROTO open_list[E_FILES];

/*   Acknowledge packet                                                    */
       struct LAN_CALL ack_call;
       char            ack_pkt[ORPH_HDR_LEN + MIN_ORPH_DATA];

/*   Ethernet LANCE controller status                                      */
struct Lance lance;

/*   Pointer to our physical ethernet address                              */
static unsigned char *PHYS_ADR;

/*
*   Error counter for received packets.  Updated in lan_rec.
*/
int rc_fram = 0;
int rc_oflo = 0;
int rc_crc = 0;
int rc_buff = 0;

/*
*   DMA interrupt status.  Nonzero is last DMA transfer terminated with
*   an error.
*/
int dma_err;

/*   Function  Prototypes                                                  */
extern void lan_int_(void);
extern void swap_copy_(void *,void *,void *);
static void dma_norm_(void);
static void dma_error_(void);
static void lan_rec(void);
static void lan_xmit(struct LAN_CALL *);
static void lan_init(int ,unsigned char *);
static void lan_open(int ,struct LAN_CALL *);
static void lan_close(struct LAN_CALL *);
static void lan_sys_close(struct LAN_CALL *);
static void lan_ioctl(struct LAN_CALL *);
static void lan_chk_rec(struct LAN_CALL *);
static void lan_flush(struct LAN_CALL *);

#define  LOC128   128     /* Local event flag                              */

/****************************************************************************
*
*    Ethernet Driver Process
*
****************************************************************************/
static void lan_driver(void)
{
  struct LAN_CALL   *call;
  struct OPEN_PROTO *open_ptr;
  struct OPEN_PROTO *open_ptr_end;
  struct Lance      *lan = &lance;
  int                lan_mode = 0,evt,i,task;
  short             *sptr = (short *)LAN_RAM;
  static unsigned char log_adr[8] = {0,0,0,0,0,0,0,0};

#ifndef  CPU60       /* use following for CPU-40                          */

/*   Disable Ethernet controller interrupts until we get setup             */

  *((unsigned char *)LAN_CTL) = 0x0;              /* Disable local 6       */

#else                /* use following for CPU-60                          */

  struct CRTL     *csr = (struct CRTL *)LANCE;
  
/*   Disable Ethernet controller interrupts until we get setup             */

  *((unsigned char *)LAN_CTL) = 0x0;              /* Disable local 7       */
  csr->rap = 0;         /* Select csr register 0                           */
  csr->rdp = STOP;      /* Stop LANCE controller                           */

#endif

/*   
*    Set all RAM shared by CPU and LANCE controller to ones
*/
  for(i=0; i < 32768;i++) *sptr++ = -1;

  PHYS_ADR = *(our_ether_address);

/*
*    Initialize the LANCE controller and driver database
*/
  lan_init(lan_mode,log_adr);
  ack_call.len = ORPH_HDR_LEN + MIN_ORPH_DATA;
  ack_call.data = ack_pkt;
  task_priority_(-1,1,70);

  evt = LAN_MSG;
  while(1)
   {
/*
*   Suspend this task until one of both events occur.  One event is always
*   LAN_INT which is set on receive interrupt by the LANCE Ethernet controller.
*   The other event is either LAN_MSG, set by a message pointer being sent, or
*   a one millisecond clock.  If no channel has an active receive timeout,
*   the message pointer event is selected.
*/
     if (wait_evts_(LAN_INT,evt) == LAN_INT)
      {
        clr_evt_(LAN_INT);              /* Clear the interrupt flag        */
        if (lan->status & RINT)         /* Was it a receive interrupt?     */
          {
            lan->status &= ~RINT;       /* Yes.  Check for a read request  */
            lan_rec();                  /* for the packet and if so        */
          }                             /* pass to task and set it's flag. */
        if (lan->status & TINT) lan->status &= ~TINT;
        if (lan->status & ERROR)        /* On error, just count it.        */
          {
            if (lan->status & BABL) lan->babl++;
            if (lan->status & CERR) lan->cerr++;
            if (lan->status & MISS) lan->miss++;
            if (lan->status & MERR) lan->merr++;
            lan->status &= ~(ERROR | BABL | CERR | MISS | MERR);
          }
      }
     else 
      {
/*
*    Here we have either message pointer event or a timer event.  First
*    check for a message pointer.  (A message pointer is a request from
*    another task for driver service.)
*/
       if (receive_ptr_(LAN_MSG_SLOT,&task,(char **)&call))
         {
/*
*    We have a driver service request.  Read is special since a packet
*    may already be available.  If a packet is available pass it to the
*    caller.  In this case, the timer should not be enabled even if
*    receive timeout mode is enabled!
*/
           if (call->func == E_READ)
             {
               open_ptr = &open_list[call->eno - 1];
               open_ptr->Msg = call;
               lan_rec();
               if(open_ptr->Msg != NULL)
                 {
                   if (open_ptr->Tmo = open_ptr->Timeout) 
                     {
                       evt = LOC128;            /* Enable timer only if    */
                       dly_set_evt_(evt,1);     /* no packet was available */
                     }
                 }
             }
           else
             {
               call->status = 0;
               switch (call->func)
                {
                  case E_OPEN:
                     lan_open(task,call);
                     break;
                  case E_CLOSE:
                     lan_close(call);
                     break;
                  case E_SYS_CLOSE:
                     lan_sys_close(call);
                     break;
                  case E_WRITE:
                     lan_xmit(call);
                     break;
                  default:
                     lan_ioctl(call);
                     break;
                }
               call->flag = 0x80;
             }
         }
        else
         {
/*
*   Since it was not a message pointer event, it must be the timer.  There
*   must be at least one channel which has a receiver timeout enabled.  Scan
*   the open channel list and update enabled timers.  If a timeout has
*   occured, respond to the caller with timeout status.
*/
           open_ptr = &open_list[0];
           open_ptr_end = &open_list[E_FILES];
           evt = LAN_MSG;
           while (open_ptr < open_ptr_end)
            {
              call = open_ptr->Msg;
              if (call != NULL && open_ptr->Timeout)
               {
/*
*   This channel has a read request with timeout enabled.
*/
                 if (open_ptr->Tmo - 1 == 0)
                   {
                     open_ptr->Msg = NULL;      /* Timeout!. Reset request */
                     call->status = 0;          /* Set timeout status      */
                     call->flag = 0x80;         /* Awake task              */
                   }
                 else
                   {
                     open_ptr->Tmo--;           /* No timeout yet.  Update */
                     evt = LOC128;              /* counter and enable      */
                     dly_set_evt_(evt,1);       /* timer again             */
                   }
               }
             open_ptr++;
            }
         }
      }
   }
/*  No good reason we should ever get here!.  But just in case.           */

  task_exit_();
}

#ifndef  CPU60       /* use following for CPU-40                          */

/****************************************************************************
*   Routine to initialize the Ethernet controller
*
*   1)  Stop the LANCE controller and initialize with the mode and logical
*       HASH address filter supplied by caller.
*   2)  Setup the receive and transmit ring structures.  Parameters for
*       the buffers are in defines above.
*   3)  Setup buffer pointers for buffer management.
*   4)  Initialize interrupt vector and interrupt handler.
****************************************************************************/
static void lan_init(int lan_mode,unsigned char *log_adr)
{
  register struct LAN_INIT *iblk = (struct LAN_INIT *)LAN_RAM;
  register struct LAN_RMD  *rmd = (struct LAN_RMD *)(LAN_RAM + LAN_REC_RING);
  register struct LAN_TMD  *tmd;
  register struct CRTL     *csr = (struct CRTL *)LANCE;
  struct OPEN_PROTO        *open_ptr;
  unsigned char            *s1, *s2, *s3;
  char                     *dma;
  int                      i,badr;
  
  csr->rap = 0;         /* Select csr register 0                           */
  csr->rdp = STOP;      /* Stop LANCE controller                           */
  csr->rap = 3;         /* Select register 3                               */
  csr->rdp = 4;         /* Select big Endian mode                          */
  csr->rap = 1;         /* Select initialization address register - low    */
  csr->rdp = 0;         /* set low 16 bits of initialization block address */
  csr->rap = 2;         /* Select Initialization address register - high   */
  csr->rdp = 0;         /* set high 8 bits                                 */

/*   Build the initialization block for the LANCE controller               */
  iblk->mode = lan_mode;        /* Set mode control word                   */
  s1 = PHYS_ADR;                /* Put in our physical address.  Note the  */
  s2 = iblk->padr;              /* byte swap required for both the physical */
  s3 = s1 + 8;                  /* address and the HASH address.            */ 
  while(s1 < s3)
    {
      *(s2 + 1) = *s1;
      *s2 = *(s1 + 1);
      s1 += 2;
      s2 += 2;
    }
  s1 = log_adr;                 /* HASH address                            */
  s2 = iblk->ladr;
  s3 = s1 + 8;
  while(s1 < s3)
    {
      *(s2 + 1) = *s1;
      *s2 = *(s1 + 1);
      s1 += 2;
      s2 += 2;
    }

/*  Define the ring buffer structure for both receive and transmit         */

  iblk->rdra = LAN_REC_RING;
  iblk->tdra = LAN_REC_RING + (NUM_RBUF * 8);
  iblk->rlen = RLEN;
  iblk->tlen = TLEN;
  iblk->res1 = 0;
  iblk->res2 = 0;
  
/*   Build the ring buffer descriptors for receive                         */

  rec_ptr = rmd;                /* Initialize buffer pointers for receive  */
  rec_ptr_str = rmd;
  rec_int_ptr = rmd;            /* Receive pointer for interrupt routine   */
  badr = LAN_REC_RING + (NUM_RBUF * 8) + (NUM_TBUF * 8);
  for (i=NUM_RBUF; i > 0; i--)
    {
      rmd->ladr = (unsigned short)badr;
      rmd->stat = OWN;
      rmd->hadr = (unsigned char)(badr >> 16);
      rmd->bcnt = -RBUF_SIZE;
      rmd->mcnt = 0;
      badr += RBUF_SIZE;
      rmd++;
    }
  rec_ptr_end = rmd;

/*   Build the ring buffer descriptors for transmit                        */

  tmd = (struct LAN_TMD *)rmd;
  trans_ptr = tmd;
  trans_ptr_str = tmd;
  for (i=NUM_TBUF; i > 0; i--)
    {
      tmd->ladr = (unsigned short)badr;
      tmd->stat = 0;
      tmd->hadr = (unsigned char)(badr >> 16);
      tmd->bcnt = 0;
      tmd->err  = 0;
      badr += TBUF_SIZE;
      tmd++;
    }
  trans_ptr_end = tmd;

/*  Initialize the channel database                                        */

  open_ptr = &open_list[0];
  for(i=0; i < E_FILES; i++) 
    {
      open_ptr->Protocol = 0;
      open_ptr->Timeout = 0;
      open_ptr->Msg = NULL;
      open_ptr->Ack_flag = 0;
      open_ptr++;
    }

/*  Setup the interrupt handler and enable interrupts.                     */
/*  The interrupt is Local 6 and is connected to CPU level 7 (highest      */
/*  priority ).                                                            */

  *((void (**)())(LAN_VEC*4)) = lan_int_; /* set service routine address   */
  *((unsigned char *)LAN_CTL) = 0xf;      /* Enable local 6 at level 7     */

/*  Initialize the LANCE controller                                        */

  csr->rap = 0;
  csr->rdp = INIT;
  while((csr->rdp & IDON) == 0);        /* Wait till initialization done   */
  csr->rdp = INEA + RXON + TXON + STRT + IDON;  /* Start LANCE controller  */

/*   Initialize the LANCE controller status structure                      */
  lance.status = csr->rdp;      /* LANCE chip status                       */
  lance.miss = 0;               /* Missed packet counter                   */
  lance.cerr = 0;               /* Collision error counter                 */
  lance.merr = 0;               /* Memory error counter                    */
  lance.babl = 0;               /* Transmit babble error counter           */

#ifdef DMA

  dma = (char *)DMA_BASE;
  *(dma + DMA_DST_ATR) = DMA_SEC8;
  *(dma + DMA_SRC_ATR) = DMA_RAM32;
  *(dma + ISTAT_NORM) = 0;
  *(dma + ISTAT_ERROR) = 0;
  *((void (**)())(DMA_NORM_V*4)) = dma_norm_;
  *((void (**)())(DMA_ERR_V*4)) = dma_error_;
  dma_err = 0;
  *(dma + DMA_GEN) = DMA_ENABLE;
  *(dma + ICTL_NORM) = 0xf;
  *(dma + ICTL_ERROR) = 0xf;
#endif
}
#else                /* use following for CPU-60                          */

/****************************************************************************
*   Routine to initialize the Ethernet controller
*
*   1)  Stop the LANCE controller and initialize with the mode and logical
*       HASH address filter supplied by caller.
*   2)  Setup the receive and transmit ring structures.  Parameters for
*       the buffers are in defines above.
*   3)  Setup buffer pointers for buffer management.
*   4)  Initialize interrupt vector and interrupt handler.
****************************************************************************/
static void lan_init(int lan_mode,unsigned char *log_adr)
{
  register struct LAN_INIT *iblk = (struct LAN_INIT *)LAN_RAM;
  register struct LAN_RMD  *rmd = (struct LAN_RMD *)(LAN_RAM + LAN_REC_RING);
  register struct LAN_TMD  *tmd;
  register struct CRTL     *csr = (struct CRTL *)LANCE;
  struct OPEN_PROTO        *open_ptr;
  unsigned char            *s1, *s2, *s3;
  char                     *dma;
  int                      i,badr;
  
  csr->rap = 0;         /* Select csr register 0                           */
  csr->rdp = STOP;      /* Stop LANCE controller                           */
  csr->rap = 3;         /* Select register 3                               */
  csr->rdp = 4;         /* Select big Endian mode                          */
  csr->rap = 1;         /* Select initialization address register - low    */
  csr->rdp = LAN_RAM;   /* set low 16 bits of initialization block address */
  csr->rap = 2;         /* Select Initialization address register - high   */
  csr->rdp = (LAN_RAM >> 16); /* set high 16 bits                          */

/*   Build the initialization block for the LANCE controller               */
  iblk->mode = lan_mode;        /* Set mode control word                   */
  s1 = PHYS_ADR;                /* Put in our physical address.  Note the  */
  s2 = iblk->padr;              /* byte swap required for both the physical */
  s3 = s1 + 6;                  /* address and the HASH address.            */ 
  while(s1 < s3)
    {
      *(s2 + 1) = *s1;
      *s2 = *(s1 + 1);
      s1 += 2;
      s2 += 2;
    }
  s1 = log_adr;                 /* HASH address                            */
  s2 = iblk->ladr;
  s3 = s1 + 8;
  while(s1 < s3)
    {
      *(s2 + 1) = *s1;
      *s2 = *(s1 + 1);
      s1 += 2;
      s2 += 2;
    }

/*  Define the ring buffer structure for both receive and transmit         */

  iblk->lrdra = LAN_RAM + LAN_REC_RING;
  iblk->hrdra = (unsigned char)((LAN_RAM + LAN_REC_RING) >> 16);
  iblk->ltdra = LAN_RAM + LAN_REC_RING + (NUM_RBUF * 8);
  iblk->htdra = (unsigned char)((LAN_RAM + LAN_REC_RING + (NUM_RBUF * 8)) >> 16);
  iblk->rlen = RLEN;
  iblk->tlen = TLEN;
  iblk->res1 = 0;
  iblk->res2 = 0;
  word_swap_(iblk,sizeof(struct LAN_INIT)/2);
  
/*   Build the ring buffer descriptors for receive                         */

  rec_ptr = rmd;                /* Initialize buffer pointers for receive  */
  rec_ptr_str = rmd;
  rec_int_ptr = rmd;            /* Receive pointer for interrupt routine   */
  badr = LAN_RAM + LAN_REC_RING + (NUM_RBUF * 8) + (NUM_TBUF * 8);
  for (i=NUM_RBUF; i > 0; i--)
    {
      rmd->ladr = (unsigned short)badr;
      rmd->stat = OWN;
      rmd->hadr = (unsigned char)(badr >> 16);
      rmd->bcnt = -RBUF_SIZE;
      rmd->mcnt = 0;
      badr += RBUF_SIZE;
      rmd++;
    }
  rec_ptr_end = rmd;

/*   Build the ring buffer descriptors for transmit                        */

  tmd = (struct LAN_TMD *)rmd;
  trans_ptr = tmd;
  trans_ptr_str = tmd;
  for (i=NUM_TBUF; i > 0; i--)
    {
      tmd->ladr = (unsigned short)badr;
      tmd->stat = 0;
      tmd->hadr = (unsigned char)(badr >> 16);
      tmd->bcnt = 0;
      tmd->err  = 0;
      badr += TBUF_SIZE;
      tmd++;
    }
  trans_ptr_end = tmd;

/*  Initialize the channel database                                        */

  open_ptr = &open_list[0];
  for(i=0; i < E_FILES; i++) 
    {
      open_ptr->Protocol = 0;
      open_ptr->Timeout = 0;
      open_ptr->Msg = NULL;
      open_ptr->Ack_flag = 0;
      open_ptr++;
    }

/*  Setup the interrupt handler and enable interrupts.                     */
/*  The interrupt is Local 7 and is connected to CPU level 7 (highest      */
/*  priority ).                                                            */

  csr->rap = 2;
  csr->bdp = 0;
  *((void (**)())(LAN_VEC*4)) = lan_int_; /* set service routine address   */
  *((unsigned char *)LAN_CTL) = 0x2f;     /* Enable local 7 at level 7     */

/*  Initialize the LANCE controller                                        */

  csr->rap = 0;
  csr->rdp = INIT;
  while(1)
    {
      i = csr->rdp;
      if ((i & IDON) != 0) break;
    }
/*  while((csr->rdp & IDON) == 0);    */  /* Wait till initialization done   */
  csr->rdp = INEA + RXON + TXON + STRT + IDON;  /* Start LANCE controller  */

/*   Initialize the LANCE controller status structure                      */
  lance.status = csr->rdp;      /* LANCE chip status                       */
  lance.miss = 0;               /* Missed packet counter                   */
  lance.cerr = 0;               /* Collision error counter                 */
  lance.merr = 0;               /* Memory error counter                    */
  lance.babl = 0;               /* Transmit babble error counter           */

#ifdef DMA

  dma = (char *)DMA_BASE;
  *(dma + DMA_DST_ATR) = DMA_SEC8;
  *(dma + DMA_SRC_ATR) = DMA_RAM32;
  *(dma + ISTAT_NORM) = 0;
  *(dma + ISTAT_ERROR) = 0;
  *((void (**)())(DMA_NORM_V*4)) = dma_norm_;
  *((void (**)())(DMA_ERR_V*4)) = dma_error_;
  dma_err = 0;
  *(dma + DMA_GEN) = DMA_ENABLE;
  *(dma + ICTL_NORM) = 0xf;
  *(dma + ICTL_ERROR) = 0xf;
#endif
}
#endif
/****************************************************************************
*   Dispatch received packets to requesters.  This routine is called 
*    1)  on a receive interrupt
*    2)  a read service request is received.
*  The list of packets is scanned for a match between the protocol word
*  and open channel for this protocol.  If no channel is open for the
*  protocol, the packet is discarded.  If a channel is open for this 
*  protocol, check for an outstanding read request and pass the packet
*  to the requester.
****************************************************************************/
static void lan_rec(void)
{
  struct Ether_Packet     *pkt;
  struct Ether_Packet     *apkt;
  struct LAN_RMD          *ring = rec_ptr,*move,tmp;
  struct LAN_CALL         *call;
  struct OPEN_PROTO       *open_ptr, *open_ptr_end;
  unsigned short          *w1,*w2,*w3;
  int                     cnt,delete,found,error;

  while (ring != rec_int_ptr)
    {
/*
*   If we own this ring descriptor,  first check the protocol word.
*/
      if ((ring->stat & OWN) == 0)
        {
          found = 0;
          delete = 0;
          pkt = (struct Ether_Packet *)(LAN_RAM + ring->ladr);
/*
*   If the first protocol byte is not ours, discard this packet and
*   free the ring descriptor for further use.
*/
          if (pkt->Protocol[0] == PROTO_PREFIX)
            {
              open_ptr = &open_list[0];
              open_ptr_end = &open_list[E_FILES];
/*
*   First protocol byte is Ok.  Now scan the open channel list for a match
*   with the second protocol byte.
*/
              while (open_ptr < open_ptr_end)
               {
                 if(open_ptr->Protocol == pkt->Protocol[1])
                  {
                    found++;
/*
*   We have a match on the second protocol byte.  See if this is an
*   acknowledge packet for one of out transmissions.  If so set
*   the acknowledgment flag to zero and delete the packet.
*/
                    if (pkt->Ack == ACKPKT)
                      {
                        open_ptr->Ack_flag = 0;
                        delete++;
                        break;
                      }
                    call = open_ptr->Msg;
                    if(call != NULL && open_ptr->Ack_flag == 0)
                     {
/*
*   We have a read request for this protocol.  If the host has requested
*   and acknowledgment, send it.  Then transfer the received packet to
*   the requesting routine buffer.
*/
                       error = ring->stat & 0x7c;  /* get receive errors */
                       if (pkt->Ack == ACK)
                         {
                           apkt = (struct Ether_Packet *)ack_pkt;
                           memcpy(apkt->Source,pkt->Destination,6);
                           memcpy(apkt->Destination,pkt->Source,6);
                           memcpy(apkt->Protocol,pkt->Protocol,8);
                           if(error != 0) apkt->Ack = NAKPKT;
                           else  apkt->Ack = ACKPKT;
                           ack_call.eno = (open_ptr - &open_list[0]) + 1;
                           lan_xmit(&ack_call);
                         }
/*
*   Check for received packet errors.  Update counters and
*   mark the packet for deletion.  NOTE: Packets with receive errors
*   are NOT given to the user.  Instead they are just deleted.  We
*   have sent the host a NAK - see above, so we hope the host will
*   retransmit the bad packet.
*/
                       if (error != 0)
                         {
                           if ((error & 0x20) != 0) rc_fram++;
                           if ((error & 0x10) != 0) rc_oflo++;
                           if ((error & 0x08) != 0) rc_crc++;
                           if ((error & 0x04) != 0) rc_buff++;
                           delete++;
                           break;
                         }
/*
*   Move packet to requester's buffer and set his done flag.
*/
                       cnt = ring->mcnt - 4;
                       if (call->len < cnt) cnt = call->len;
                       w1 = (unsigned short *)call->data;
                       w2 = (unsigned short *)pkt;
                       w3 = w2 + cnt/2;
                       while (w2 < w3) *w1++ = *w2++;
                       if (cnt % 2) *(char *)w1 = *(char *)w2;
                       open_ptr->Msg = NULL;
                       call->status = cnt;
                       call->flag = 0x80;
                       delete++;
                       break;
                     }
                  }
                open_ptr++;
               }    /*  end while  */
            }
/*
*   All tests of this packet have been made.  If found is zero, it means
*   that 1) the protocol is not ours or 2) no channel is open for this
*   protocol. In either case discard this packet.  If delete is nonzero,
*   the packet is either an acknowledgment or it has been send to
*   the requester.  So, just free the ring descriptor.
*/
          if (found == 0 || delete != 0)
            {
              ring->stat = OWN;
              if (ring == rec_ptr) rec_ptr++;
              if (rec_ptr >= rec_ptr_end) rec_ptr = rec_ptr_str;
            }
        }
/*
*   Check the next ring descriptor.
*/
      ring++;
      if (ring >= rec_ptr_end) ring = rec_ptr_str;
    }   /*  end while  */
/*
*   If packets remain, we must compact the Lance chips ring structure
*   such that the maximum number of free buffers are available.  This
*   is done by reordering the descriptors.
*/
   while (ring != rec_ptr)
     {
/*
*   ring points to the descriptor for the next received packet.  From this
*   point search backward for a free descriptor.
*/
       --ring;
       if (ring < rec_ptr_str) ring = rec_ptr_end - 1;
       if ((ring->stat & OWN) != 0)
         {
/*
*   ring now points to a free descriptor.  Now search backward for a used
*   descriptor and then swap the free and used descriptors.
*/
           move = ring;
           do
             {
               --move;
               if (move < rec_ptr_str) move = rec_ptr_end -1;
             }
           while ((move->stat & OWN) != 0);
           tmp = *ring;
           *ring = *move;
           *move = tmp;
           if (move == rec_ptr) break;
         }
      }
    rec_ptr = ring;
}
/****************************************************************************
*
*   Transmit a packet.
*
****************************************************************************/
static void lan_xmit(struct LAN_CALL *call)
{
  struct CRTL             *lanc;
  struct LAN_TMD          *ring = trans_ptr;
  struct Ether_Packet     *pkt;
  struct OPEN_PROTO       *open_ptr;
  register unsigned short *w1,*w2,*w3;
  char                    *cptr,*dma;
  int                     cnt,i;

/*
*   If packet buffer is too large, just return an error code.
*/
  if (call->len > MAX_ORPH_DATA + ORPH_HDR_LEN)
    {
      call->status = -EFBIG;
    }
  else
    {
/*
*   If necessary wait till a transmit ring descriptor is available.
*/
      while ((ring->stat & OWN) != 0) task_swap_();
      pkt = (struct Ether_Packet *)call->data;
/*
*   If an acknowledgment is expected,  set the Ack_flag to one so the
*   receiver will trap it.
*/
      if (pkt->Ack == ACK) 
        {
          open_ptr = &open_list[call->eno -1];
          open_ptr->Ack_flag = 1;
        }
/*
*   Move the user buffer to the shared RAM which serves the LANCE
*   controller.  If necessary, pad out buffer to minimum size.
*/
#ifndef  DMA
      w3 = (unsigned short *)((char *)call->data + call->len);
      w1 = (unsigned short *)(LAN_RAM + ring->ladr);
      cptr = (char *)w1 + call->len;
      w2 = (unsigned short *)pkt;
      if (pkt->Protocol[1] != PROTO_DATA)
        {
          while (w2 < w3) *w1++ = *w2++;
        }
      else
        {
          swap_copy_(w1,w2,w3);
        }
      cnt = call->len;
      if (cnt < ORPH_HDR_LEN + MIN_ORPH_DATA)
        {
          for(i = MIN_ORPH_DATA + ORPH_HDR_LEN - cnt; i > 0; i--)
                                                                *cptr++ = '\0';
          ring->bcnt = -(ORPH_HDR_LEN + MIN_ORPH_DATA);
        }
      else
        {
          ring->bcnt = -cnt;
        }
#else
      cnt = call->len;
      if (cnt < ORPH_HDR_LEN + MIN_ORPH_DATA)
        {
          w3 = (unsigned short *)((char *)call->data + call->len);
          w1 = (unsigned short *)(LAN_RAM + ring->ladr);
          cptr = (char *)w1 + call->len;
          w2 = (unsigned short *)pkt;
          while (w2 < w3) *w1++ = *w2++;
          for(i = MIN_ORPH_DATA + ORPH_HDR_LEN - cnt; i > 0; i--)
                                                                *cptr++ = '\0';
          ring->bcnt = -(ORPH_HDR_LEN + MIN_ORPH_DATA);
        }
      else
        {
          if (pkt->Protocol[1] == PROTO_DATA)
            {
              swap_bytes_((char *)pkt->Data,cnt - ORPH_HDR_LEN);
            }
          dma = (char *)DMA_BASE;
          *(long **)(dma + DMA_SRC_ADDR) = (long *)call->data;
          *(long **)(dma + DMA_DST_ADDR) = (long *)(LAN_RAM + ring->ladr);
          *(long *)(dma + DMA_CNT) = call->len;
          *(dma + DMA_RUN_CTL) = DMA_START;
          wait_evts_(DMA_DONE,0);
          ring->bcnt = -cnt;
        }
#endif
      ring->err = 0;
      ring->stat = OWN | STP | ENP;
      ring++;
      trans_ptr = (ring >= trans_ptr_end) ? trans_ptr_str : ring;
      call->status = cnt;
/*
*   Force immediate action by the LANCE controller.
*/
      lanc = (struct CRTL *)LANCE;
      lanc->rap = 0;
      lanc->rdp = INEA | TDMD;
    }
}
/****************************************************************************
*
*   Open a channel to the Ethernet driver.  Routine return the channel
*   number which the caller then uses in all others calls for service.
*
****************************************************************************/
static void lan_open(int task,struct LAN_CALL *call)
{
  int               eno = 1;
  struct OPEN_PROTO *open_ptr;

  open_ptr = &open_list[0];
  for(; eno < E_FILES; eno++)
   {
     if(!open_ptr->Task)
       {
         open_ptr->Task = task | 0x80;
         call->status = eno;
         open_ptr->Msg = NULL;
         open_ptr->Protocol = 0;
         open_ptr->Ack_flag = 0;
         open_ptr->Timeout = 0;
         return;
       }
     open_ptr++;
    }
/*
*   Channel table full.  Return error code
*/
   call->status = -ENFILE;
}
/****************************************************************************
*
*   Close and release a channel.
*
****************************************************************************/
static void lan_close(struct LAN_CALL *call)
{
  struct OPEN_PROTO *open_ptr;
  int               eno;

  eno = call->eno - 1;
  open_ptr = &open_list[eno];
  open_ptr->Task = 0;
  open_ptr->Protocol = 0;
  open_ptr->Ack_flag = 0;
  open_ptr->Msg = NULL;
  call->status = 0;
}
/****************************************************************************
*
*   System Close and release a channel.
*
****************************************************************************/
static void lan_sys_close(struct LAN_CALL *call)
{
  struct OPEN_PROTO *open_ptr,*open_ptr_end;
  char              task;

  task = call->eno + 0x80;
  open_ptr = &open_list[0];
  open_ptr_end = &open_list[E_FILES];
  while (open_ptr <= open_ptr_end)
    {
      if (open_ptr->Task == task)
        {
          open_ptr->Task = 0;
          open_ptr->Protocol = 0;
          open_ptr->Ack_flag = 0;
          open_ptr->Msg = NULL;
        }
      open_ptr++;
    }
  call->status = 0;
}
/****************************************************************************
*
*   Routine to handle I/O control calls
*
****************************************************************************/
static void lan_ioctl(struct LAN_CALL *call)
{
   int  eno;

   eno = call->eno - 1;
   call->status = 0;
   switch  (call->func)
    {
     case  EIOSTIMEOUT:
       open_list[eno].Timeout = *((long *)call->data);
       break;
     case  EIOGTIMEOUT:
       *((long *)call->data) = open_list[eno].Timeout;
       break;
     case  EIOSPROTO:
       open_list[eno].Protocol = *((char *)call->data);
       break;
     case  EIOGPROTO:
       *((char *)call->data) = open_list[eno].Protocol;
       break;
     case  EIOPHYSADR:
       memcpy(call->data,PHYS_ADR,6);
       break;
     case  EIOCHKREC:
       lan_chk_rec(call);
       break;
     case  EIOFLUSH:
       lan_flush(call);
       break;
     case  EIOGACKCTR:
       *((int *)call->data) = open_list[eno].Ack_flag;
       break;
     case  EIOGACKADR:
       *((char **)call->data) = (char *)(&open_list[eno].Ack_flag);
       break;
     default:
/*
*   Unrecognized request
*/
       call->status = -EINVAL;
    }
}
/****************************************************************************
*
*   Check for receive packets on this channel.  If any packets are available
*   return a TRUE status.  Otherwise, return FALSE.
*
****************************************************************************/
static void lan_chk_rec(struct LAN_CALL *call)
{
  struct Ether_Packet     *pkt;
  struct LAN_RMD          *ring = rec_ptr;
  struct OPEN_PROTO       *open_ptr;
  struct chk_receive      *chk;

  chk = (struct chk_receive *)call->data;
  open_ptr = &open_list[call->eno - 1];
  while (ring != rec_int_ptr)
    {
/*
*   If we own this ring descriptor,  first check the protocol word.
*/
      if ((ring->stat & OWN) == 0)
        {
          pkt = (struct Ether_Packet *)(LAN_RAM + ring->ladr);
          if (pkt->Protocol[0] == PROTO_PREFIX)
            {
              int found = 0;

              if(open_ptr->Protocol == pkt->Protocol[1])
                {

                  if (chk->Request_Number == 0) found = 1;
                  else
                    {
                      if (chk->Request_Number == pkt->Request_Number &&
                           chk->Order == pkt->Order &&
                           !memcmp(chk->Source,pkt->Source,6)) found = 1;
                    }
                }
              if (found)
                {
                  chk->status = TRUE;
                  return;
                }
            }
         }
/*
*   Check the next ring descriptor.
*/
      ring++;
      if (ring >= rec_ptr_end) ring = rec_ptr_str;
    }
  chk->status = FALSE;
  return;
}
/****************************************************************************
*
*    Discard any packets for this channel.
*
****************************************************************************/
static void lan_flush(struct LAN_CALL *call)
{
  struct Ether_Packet     *pkt;
  struct LAN_RMD          *ring = rec_ptr;
  struct OPEN_PROTO       *open_ptr;

  open_ptr = &open_list[call->eno - 1];
  while (ring != rec_int_ptr)
    {
/*
*   If we own this ring descriptor,  first check the protocol word.
*/
      if ((ring->stat & OWN) == 0)
        {
          pkt = (struct Ether_Packet *)(LAN_RAM + ring->ladr);
          if (pkt->Protocol[0] == PROTO_PREFIX)
            {
              if(open_ptr->Protocol == pkt->Protocol[1]) ring->stat = OWN;
            }
         }
/*
*   Check the next ring descriptor.
*/
      ring++;
      if (ring >= rec_ptr_end) ring = rec_ptr_str;
    }
  open_ptr->Ack_flag = 0;
}
