/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-1996
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/VMEskel.c
*
*    Description:  This is a template for a custom data acquisition process.
*                  Included are all the host control (i.e. initialize, start
*                  stop and status functions) and  data buffer routines which
*                  communicate with the process data_proc.  To customize
*                  this code, the user needs to add custom setup in the
*                  acq_init routine and in the process_event routine.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/29/93    MCSQ       Original  
*
*    4/14/95    MCSQ       Upgrade to current acquisition system.
*
*   10/25/96    MCSQ       Another upgrade
*****************************************************************************/
#include "vme_sys.h"
#include "vmeprom.h"
#include "system.h"
#include "ksc.h"
#include "lrs.h"
#include "lrs1821.h"
#include "lrs1190.h"
#include "ces.h"
#include "orph.h"
#include "lan_lib.h"
#include "lan.h"
#include "Acq_Params.h"
#include "orphmsg.h"
#include "acq_ctl.h"
#include "devices.h"
#include "trigger.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/****************************************************************************
*
*
******************************************************************************/

/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

/*    structure for calls to ksc_camio                                      */
struct camac {
     unsigned char  c;
     unsigned char  n;
     unsigned char  a;
     unsigned char  f;
	      int   d;
};

/*    structure for calls to fastio                                          */
struct fastd {
     unsigned char  f;
     unsigned char  a;
     unsigned short  d;
};

/*
*       Local variables
*/
int                         ACQ_initialized = 0;
static struct KSC_VME *ksc_ptr = (struct KSC_VME *)KSC1;
static char                 error_msg[81];
static unsigned short       Initial_cma = 0x20;
static unsigned short       Current_cma;
static unsigned short       id_list[50];
static unsigned short       list_cma;

/*
*     Data buffers and pointers
*/
unsigned short *Event;
struct data_buf *AcqBuf,*NewBuf;
struct data_buf Buf1,Buf2;

/*
*    Data in the memory segment  "Acq_Params"
*/
struct acq_share *share = (struct acq_share *)ACQ_SHARED_RAM;

/*
*    Device table
*/
struct devices *devtbl = DEVTBL;

/*************************    Function Prototypes    ************************/

int  acq_stop(void);
int  acq_start(void);
int  acq_init(void);
void buffer_setup(void);

int  set_CAMAC_inhibit(void);
int  remove_CAMAC_inhibit(void);

void host_message(int ,char *);
int  ksc_camio(struct camac *);

/*
*   The following routines are executed when the acquisition event
*   interrupt occurs.
*/

void process_event(void);
void dump_event(void);
void wait_msg_buf(void);
int  send_event(void);
void event_process(void);
void spurious_vme_(void);

/****************************************************************************
*
*   Command Processor
*
*  The host controls the acqusition via messages.  Control messages include:
*
*  1) Initialize - Initialize executes the setup routines which build the
*                  run-time tables for the readout routines.  You must
*                  initialize before starting acquisition.
*
*  2) Start      - Start data acquisition.  You can start acquisition only
*                  if initialization has been done and no errors were 
*                  detected in the initialization process.
*
*  3) Stop       - Stop data acquisition.  Initialization cannot not be
*                  done unless acquisition is stopped.
*
*  Errors:  Many error codes can be returned to the host.  For example,
*           a Start command when the acquisition is running returns an
*           error.  The file  acq_ctl.h contains a list of valid error
*           codes.
****************************************************************************/
main(void)
{
   struct Ether_Packet *out_hdr,*in_hdr;
   int   len;
   char  *inbuf,*outbuf;

   lan_open(PROTO_FECNTRL,&outbuf,&out_hdr);

   task_priority_(-1,1,69);

   share->acqrun = 0;
   share->FB_enabled = 0;
   share->FB_error = 0;
   share->KSC3982_enabled = 0;
   while(1)
    {
      lan_read(0,&inbuf);               /* get a command packet from host  */

      *outbuf = *(outbuf+1) = 0;
      len = 2;
      switch (*inbuf)                   /* dispatch on function code       */
       {
         case  INIT_ACQ:          /* Build run-time tables from loaded PAC */
          in_hdr = (struct Ether_Packet *)(inbuf - ORPH_HDR_LEN);
          memcpy(share->Host_Ether_Adr,in_hdr->Source,6);
          if (share->acqrun == 0) *outbuf = acq_init();
          else  *outbuf = ACQ_INIT_RUN;
          break;
         case  START_ACQ:         /* Start data acquisition                */
          *outbuf = acq_start();
          if (*outbuf == ACQ_OK) share->acqrun = 1;
          break;
         case  STOP_ACQ:          /* Stop data acquisition                 */
          *outbuf = acq_stop();
          if (*outbuf == ACQ_OK) share->acqrun = 0;
          break;
         case  STATUS_ACQ:        /* Return acquisition status             */
          *outbuf = ACQ_OK;
          if (ACQ_initialized == 0) *(outbuf+1) = ACQ_UNINIT;
          else if (share->acqrun == 1) *(outbuf+1) = ACQ_RUN;
          else  *(outbuf+1) = ACQ_STOP;
          break;
         case  PAC_FILE:
          *outbuf = ACQ_OK;
          if (ACQ_initialized == 0) *(outbuf+1) = ACQ_UNINIT;
          strcpy(outbuf+2,"Custom Acquisition code: ");
          strcat(outbuf+2,__FILE__);
          len = strlen(outbuf+2);
          len += 2;
          break;
         case  HOST:
          *outbuf = ACQ_OK;
          memcpy(outbuf+1,share->Host_Ether_Adr,6);
          len = 8;
          break;
         default:
          *outbuf = ACQ_UNKNOWN_COMMAND;
          break;
       }
     lan_reply(len,ACK);                  /* Send command response   */
    }
}
/****************************************************************************
*
*   STOP Data Acquisition.
*
*   1) If acquisition is already stopped, just return error code.
*
*   2) Disable the event interrupt.
*
*   3) Set INHIBIT in all CAMAC crate controllers.  This should stop counting
*      in all CAMAC scalers.
*
*   4) Send remainder of the data, if any, to the data format/transmission
*      process.
*
*   5) Send a special buffer(zero length) to the data format/transmission
*      process.  This causes a special Ethernet packet to be sent to the
*      host indicating that acquisition is really stopped.
*
*   6) Send message to the host logger process showing the number of
*      next event.
****************************************************************************/
int acq_stop(void)
{
   char  csr,*cptr;
   int   size;
   struct trig *trigger = (struct trig *)TRIGGER;

   cptr = *((char **)(DEF_VEC*4));
   super_mode_();
   csr = trigger->iera;
   *((void (**)())(DEF_VEC*4)) = spurious_vme_;
   trigger->iera = csr & ~EVT_INTR_ENA;
   *((char **)(DEF_VEC*4)) = cptr;
   user_mode_();
   if (csr & EVT_INTR_ENA)
     {
/*
*   Set INHIBIT in all CAMAC crates to prevent scalers from counting
*/
       set_CAMAC_inhibit();
/*
*   If acquisition was enabled,  send remainder of data plus any
*   special stop stuff.
*/
       if (NewBuf == NULL)
         {
           size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
           if (size != 0)
             {
               AcqBuf->Bufhdr.totalevents = share->event_number;
               share->event_number += AcqBuf->Bufhdr.events;
               AcqBuf->Bufhdr.end_buf = Event;
               AcqBuf->Bufhdr.busy = -1;
               while(!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf))
                                                      wait_evts_(-DATA_MSG,0);
               wait_phys_(0x07,(char *)&(AcqBuf->Bufhdr.busy),NULL);
             }
         }
       else  {while(NewBuf != NULL) delay_(1);}
       *((char *)PIT2_CTL) = 0;                 /* Disable PI/T #2 intr     */
/*
*   Send special stop acquisition buffer - zero length buffer.  This causes
*   special Ethernet packet to be sent to host.
*/
       AcqBuf->Bufhdr.totalevents = share->event_number;
       AcqBuf->Bufhdr.events = 0;
       AcqBuf->Bufhdr.busy = -1;
       AcqBuf->Bufhdr.end_buf = AcqBuf->Bufhdr.str_buf;
       while(!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf)) wait_evts_(-DATA_MSG,0);
       wait_phys_(0x07,(char *)&(AcqBuf->Bufhdr.busy),NULL);
       Event = AcqBuf->Bufhdr.str_buf;

/*
*   Send message to acquisition logger process on the host indicating the
*   event number at stop.
*/
       sprintf(error_msg,"Stop VME acquisition.  Event Number = %u",
                                                           share->event_number);
       host_message(INFORM,error_msg);
/*
*   Patch the 68040 event interrupt vector and PI/T #2 timer interrupt vector.
*/
       super_mode_();
       trigger->iera = 0;
       trigger->imra = 0;
       trigger->gpip = ORNL_STOPPED;
       user_mode_();
       *((void (**)())(ORNL_EVT_VEC*4)) = *((void (**)())(DEF_VEC*4));
       *((void (**)())(ORNL_TIM_VEC*4)) = *((void (**)())(DEF_VEC*4));
       return(0);
     }
   return(ACQ_STP_HALT);
}
/****************************************************************************
*
*   START Data Acquisition.
*
*  1) If the acquisition process has never been initialized or the last
*     attempted initialization resulted in an error, return error code
*     to host.
*
*  2) If acquisition is already running, return error code.
*
*  3) Remove INHIBIT in all CAMAC crates so scalers will count and ADC/TDCs
*     will convert.
*
*  4) Initialize the event interrupt module and it's 68040 interrupt
*     vector.
*
*  5) Send message to the host logger process showing the number of
*     next event.
****************************************************************************/
int acq_start(void)
{
   struct trig *trigger = (struct trig *)TRIGGER;
   int  status;
   int  err;

/*
*    We can start only is a PAC has been initialized.
*/
   if (ACQ_initialized != -1) return(ACQ_STR_NOINIT);

   super_mode_();
   status = trigger->iera;
   user_mode_();
   if (status) return(ACQ_STR_RUN);

   if (err = remove_CAMAC_inhibit()) return(err);

   *((void (**)())(ORNL_EVT_VEC*4)) = process_event;
   NewBuf = NULL;
   super_mode_();
   trigger->iera = EVT_INTR_ENA | TIMER_INTR_ENA;
   trigger->imra = EVT_INTR_ENA;
   trigger->gpip = 0;
   trigger->event_clear = 0;
   user_mode_();
   sprintf(error_msg,"Start VME acquisition. Event Number = %u",
                                                          share->event_number);
   host_message(INFORM,error_msg);
   return(ACQ_OK);
}
/****************************************************************************
*
*   Initialization of the data acquisition system.  This function builds
*   the run-time tables required for data acquisition.  Any errors send
*   messages to the host and also prevent start of data acquisition.
****************************************************************************/
int  acq_init(void)
{
   register struct KSC_VME *ksc = ksc_ptr;
   struct camac lcnaf;
   int  level;

   ACQ_initialized = 0;
   if (!devtbl->ksc2917a) return(ACQ_NO_KSC2917);

   Current_cma = Initial_cma;	/* Set start of lists in KSC2917 */

/*==========================================================================
=
=    Replace example code below with your custom code
=
==========================================================================*/
/*
*   Set mode register in the LeCroy 4300
*/
   lcnaf.f = 9;
   lcnaf.a = 0;
   lcnaf.n = 14;
   lcnaf.c = 6;
   ksc_camio(&lcnaf);
   lcnaf.f = 0;
   ksc_camio(&lcnaf);
   lcnaf.d &= 0x8800;   /* save overflow suppression bit         */
   lcnaf.d |= 0x3055;   /* set CAMAC sequential and compression  */
   lcnaf.f = 16;
   ksc_camio(&lcnaf);

   ksc->cma = Current_cma;
   list_cma = Current_cma;
   level = set_intr_level_(0x500);
/*
*   Read data from a LeCroy 4300 FERA ADC in sparse readout format
*/
   ksc->cmr = BLOCK(6,Q_Stop,WS16,A_DIS);
   ksc->cmr = NAF(14,0,2);
   ksc->cmr = -18;
   ksc->cmr = 0;
   ksc->cmr = HALT;
/*
*  Clear the ADC
*/
   ksc->cmr = CAM(6,WS16,A_DIS);
   ksc->cmr = NAF(14,0,9);
   ksc->cmr = HALT;
   set_intr_level_(level);

/*==========================================================================
==========================================================================*/
/*
*   Set CAMAC Inhibit in all crates.
*/
   set_CAMAC_inhibit();

/*
*   Setup event buffers and initialize the Event interrupt I/O
*   module.
*/
   if (devtbl->trigger) buffer_setup();
   else  return(ACQ_NO_TRIGGER);

   return(0);
}
/****************************************************************************
*
*   Initialize the event data buffers, the timer, and the interrupt
*   I/O moudle.  When finished, enable the command to start data
*   acquisition.
****************************************************************************/
void buffer_setup(void)
{
   struct trig *trigger = (struct trig *)TRIGGER;
   int  task;
   char *cptr;

/*
*   Initialize data buffer headers
*/
   Buf1.Bufhdr.str_buf = Buf1.Data;
   Buf1.Bufhdr.end_buf = Buf1.Data;
   Buf1.Bufhdr.events = 0;
   Buf1.Bufhdr.busy = 0;
   Buf1.Bufhdr.ack = 0;
   Buf2.Bufhdr.str_buf = Buf2.Data;
   Buf2.Bufhdr.end_buf = Buf2.Data;
   Buf2.Bufhdr.events = 0;
   Buf2.Bufhdr.busy = 0;
   Buf2.Bufhdr.ack = 0;

/*
*   Initialize headers for Ethernet packets
*/
   memcpy(Buf1.Head.Source,*(our_ether_address),6);
   memcpy(Buf1.Head.Destination,share->Host_Ether_Adr,6);
   Buf1.Head.Protocol[0] = PROTO_PREFIX;
   Buf1.Head.Protocol[1] = PROTO_DATA;
   Buf1.Head.Ack = NOACK;
   memcpy(Buf2.Head.Source,*(our_ether_address),6);
   memcpy(Buf2.Head.Destination,share->Host_Ether_Adr,6);
   Buf2.Head.Protocol[0] = PROTO_PREFIX;
   Buf2.Head.Protocol[1] = PROTO_DATA;
   Buf2.Head.Ack = NOACK;
/*
*   Initialize timers - FGA-002
*/
   *((unsigned char *)TIM_CTL) = 0;        /* Stop timer                   */
   *((unsigned char *)TIM_PRELOAD) = 255;  /* Set preload register         */
   *((unsigned char *)TIM_ICTL) = 0;       /* Disable interrupts           */
/*
*   Initialize message pointer for data packets
*/
   receive_ptr_(DATA_MSG_SLOT,&task,&cptr);
   clr_evt_(DATA_MSG);
/*
*   Initialize buffer pointers
*/
   Event = Buf1.Data;
   AcqBuf = &Buf1;

   super_mode_();
   trigger->tacr = 6;           /* Set timer A for divide by 100 prescale */
   trigger->tadr = 20;          /* 500 us timer interval                  */
   trigger->tbcr = 6;           /* Set timer B for divide by 100 prescale */
   trigger->tbdr = 200;         /* 5 ms timer interval                    */
   trigger->imra = 0;
   trigger->imrb = 0;
   trigger->iera = 0;
   trigger->ierb = 0;
   trigger->vr = (ORNL_EVT_VEC/16)*16;
   trigger->aer = 0x80;
   trigger->gpip = 0;
   trigger->ddr = ORNL_CLEAR | ORNL_BUSY | ORNL_STOPPED;
   trigger->gpip = ORNL_STOPPED;
   user_mode_();

/*
*   Enable the command to start data acquisition.
*/
   ACQ_initialized = -1;

   sprintf(error_msg,"VME acquisition system initialized");
   host_message(INFORM,error_msg);
   return;
}
/****************************************************************************
*
*   Set the CAMAC Inhibit in each crate.  Function is called when data
*   acquisition stops.  INHIBIT prevents scalers from counting while
*   data acquisition is stopped.
****************************************************************************/
int set_CAMAC_inhibit(void)
{
      struct camac cnaf;
              int  crate,status;

  crate = ACQ_MAX_CRATE;
  while(crate >= 0)
   {
     cnaf.c = crate--;
     cnaf.n = 30;
     cnaf.a = 0;
     cnaf.f = 1;                            /* Read crate controller status */
     status = ksc_camio(&cnaf);
     if (status & KSC_CSR_TMO) continue;    /* Nonexistent crate           */
     if (cnaf.d & 0x2000) continue;         /* Crate Off_line              */
     cnaf.f = 17;
     cnaf.d = 4;                            /* set inhibit                 */
     status = ksc_camio(&cnaf);
   }
  return(0);
}
/****************************************************************************
*
*   Remove CAMAC inhibit in each crate.  Function is called when data
*   acquisition starts.  When INHIBIT is clear, scalers count and
*   ADCs can do conversions.
****************************************************************************/
int remove_CAMAC_inhibit(void)
{
      struct camac cnaf;
              int  crate,status;
              int  err = 0;

  crate = ACQ_MAX_CRATE;
  while(crate >= 0)
   {
     cnaf.c = crate--;
     cnaf.n = 30;
     cnaf.a = 0;
     cnaf.f = 1;                            /* Read crate controller status */
     status = ksc_camio(&cnaf);
     if (status & KSC_CSR_TMO) continue;    /* Nonexistent crate           */
     if (cnaf.d & 0x2000) continue;         /* Crate Off_line              */
     cnaf.f = 17;
     cnaf.d = 0;                            /* reset inhibit               */
     status = ksc_camio(&cnaf);
     cnaf.f = 1;
     status = ksc_camio(&cnaf);
     if (cnaf.d &0x44)
       {
         err = ACQ_CAM_INHIBIT;
       }
   }
  return(err);
}
/****************************************************************************
*
*  Set a message to the Host.  If enabled, also output the message
*  to the local terminal attached to the VME processor.
****************************************************************************/
void host_message(int type,char *msg)
{
  int  status;
  static struct VMEmsg *host_msg;
  static int  eno;
  static struct Ether_Packet out_pkt;

  if(!eno)
    {
      eno = open("ln1",3);
      if (eno <= 0)
        {
          printf("Can't open device 'ln'\n");
          exit(1001);
        }
      ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in
                                                the packet header           */
      out_pkt.Order = 0;
      out_pkt.Protocol[0] = PROTO_PREFIX;
      out_pkt.Protocol[1] = PROTO_FEMSG;
      out_pkt.Ack = NOACK;
      host_msg = (struct VMEmsg *)out_pkt.Data;
      strcpy(host_msg->sender,"VMEacq  ");
    }
   memcpy((char *)out_pkt.Destination,share->Host_Ether_Adr,6);
   host_msg->type = type;
   strcpy(host_msg->text,msg);
   status = write(eno,(char *)&out_pkt,sizeof(struct VMEmsg) + ORPH_HDR_LEN);
   if (status < 0)
     {
       printf("Write failure on device 'ln'\n");
       exit(1003);
     }
   delay_(2);

#ifdef  LOCAL_MSG
   printf("%s\n",msg);
#endif
}
/****************************************************************************
*
*   Routine to do one CAMAC CNAF.  The CAMAC operation is described in the
*   structure camac.  A software timeout is used to prevent hanging when
*   the addressed crate is switched off-line.  The acquisition interrupt
*   is disabled while the KSC 2917 is in use.
*
*  Call:   pointer to struct camac
*
*  Return: KSC 2917 status register.
*
****************************************************************************/
int ksc_camio(struct camac *cambuf)
{
   int                      level;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   register int             wd0,wd1;
   register unsigned short  status;
   register int tmo = 15;       /* Timeout is approx. 1.5*tmo microseconds */

/*
*   Build the command words for KSC 2917 interface from the data in struct
*   camac.
*/
   wd0 = CAM(cambuf->c,WS16,A_DIS);
   wd1 = NAF(cambuf->n,cambuf->a,cambuf->f);

   switch  (cambuf->f & 0x18)
     {
/*
*    CAMAC non-data transfer functions - F(8) thru F(15) & F(24) thru F(31)
*/
       case  0x8:
       case  0x18:
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
         break;
/*
*    CAMAC read functions - F(0) thru F(7)
*/
       case  0x0:
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
         do
          {
            if(ksc->csr & KSC_CSR_RDY)
             {
              cambuf->d = ksc->dlr;
              break;
             }
            tmo--;
          } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
         break;
/*
*   CAMAC write functions - F(16) thru F(23)
*/
       case  0x10:
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
         do
          {
            if(ksc->csr & KSC_CSR_RDY)
             {
              ksc->dlr = cambuf->d;
              break;
             }
            tmo--;
          } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
         break;
     }
/*
*   If the CAMAC operation was executed, we return the KSC 2917 status
*   register.  On a timeout (i.e. tmo == 0), clear the run flag in the
*   KSC 2917 and return timeout status.  NOTE: The caller cannot
*   distinguish between a crate off-line and a nonexistent crate!!
*/
   do
     {
       status = ksc->csr;
     } while (tmo-- && !(status & KSC_CSR_DONE));
   if (tmo < 0)
     {
       status = KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ;
       ksc->csr = KSC_CSR_RST;
     }
   else if ((status & KSC_CSR_TMO) != 0)
                                    status = status | KSC_CSR_NOX |KSC_CSR_NOQ;
   set_intr_level_(level);  /* restore interrupt level */
   return (status);
}
/****************************************************************************
*
*   The following routines execute when an event interrupt occurs.  There are
*   several important considerations for code here:
*     1)  The task ID will be that of the INTERRUPTED TASK.  Avoid any
*         VMEPROM function which uses the task ID for any purpose.
*         You will find an exception to this rule in the routine 
*         "send_event".  A message pointer is sent to another task
*         which processes data buffers for transmission to the host.
*         This case works because the receiving task does not use
*         the source task ID.  However,  waiting for event flags (
*         logical or physical) CANNOT be used.
*
*     2)  Execution will be in supervisor mode and the stack will be the
*         supervisor stack of the INTERRUPTED TASK.  These stacks are by
*         default much smaller than user mode stacks.  So excessive stack
*         storage should be avoided (i.e. use mostly static variables).
*
*     3)  The clock interrupt should be at the same, or a lower, interrupt
*         level as the event interrupt.  This prevents task rescheduling
*         by the clock interrupt while in these routines.  However, it
*         also means we CANNOT call any routines which require execution
*         of another task for completion (i.e. any Ethernet service call).
****************************************************************************
*
*   This function is the Event interrupt routine.
*
****************************************************************************/
void  process_event(void)
{
#pragma ghs interrupt

   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   register unsigned short *id_ptr = id_list;
   register unsigned short *evt = Event;
   register struct data_buf *acqbuf = AcqBuf;
   register unsigned short stat,tmp;
   register short count = 0;

/*==========================================================================
=
=    Replace the example event processing code below with your custom
=    event processing
=
==========================================================================*/
   ksc->cma = list_cma;
   ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
   while(!(ksc->csr & KSC_CSR_DONE))
     {
       register unsigned short param_id;

       while (!((stat = ksc->csr) & KSC_CSR_RDY));
       tmp = ksc->dlr;
       if (stat & KSC_CSR_NOQ) break;
       *evt++ = tmp;
      }
   while(!(ksc->csr & KSC_CSR_DONE));
   ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;

/*==========================================================================
==========================================================================*/

   ((struct trig *)TRIGGER)->event_clear = 0;
   if (count = evt - Event)
     {
       int size;

       if (count % 2) *evt++ = 0xffff;     /* make sure there are an even 
                                              number of 16 bit words       */
       *(unsigned int *)evt = 0xffffffff;  /* put in the End-Of-Event flag */
       evt += 2;
/*
*   Increment event counter and then check for space for another event.
*/
       acqbuf->Bufhdr.events += 1;
       size = (evt - acqbuf->Bufhdr.str_buf) * sizeof(unsigned short);
       if (size >= MAX_PKT_DATA -
                       ((struct acq_share *)ACQ_SHARED_RAM)->avg_param_size)
         {
           send_event();       /* when buffer is full, send to host */
           evt = Event;
         }
       else
         {
/*
*    There is more room in the inn
*/
           acqbuf->Bufhdr.last_event = evt;
         }
       Event = evt;
     }

   return;

}
/****************************************************************************
****************************************************************************/
void spurious_vme_(void)
{
#pragma ghs interrupt
}
/****************************************************************************
*
*  This becomes the interrupt routine when all acquisition data buffers
*  are in use.  The first time here is due to an Event interrupt.  Thereafter
*  the PI/T #2 timer interrupt brings us here.  Test the assigned buffer
*  and return until it is free.  When it becomes free, reenable the Event
*  interrupt and disable the timer interrupt.  Set interrupt handler
*  back to normal Event interrupt service routine.
****************************************************************************/
void  dump_event(void)
{
#pragma ghs interrupt

   struct trig *trigger = (struct trig *)TRIGGER;

   if (NewBuf->Bufhdr.busy != 0) return;
   NewBuf->Bufhdr.events = 0;
   Event = NewBuf->Bufhdr.str_buf;
   AcqBuf = NewBuf;
   NewBuf = NULL;
   trigger->imra = EVT_INTR_ENA;
}
/****************************************************************************
*
*   This becomes the interrupt routine when we are not able to send a
*   message pointer to the data format/transmission process( we cannot
*   send a message pointer until the previous one is acknowledged as
*   received).  The first Event interrupt disables additional Event
*   interrupts.  After that the PI/T timer #2 brings us here.  When
*   we can send the message pointer, Event interrupts are reenabled
*   and PI/T timer #2 is disabled.  Event interrupt service is restored
*   to the normal service routine.
****************************************************************************/
void  wait_msg_buf(void)
{
#pragma ghs interrupt

   struct trig *trigger = (struct trig *)TRIGGER;

   if (Buf1.Bufhdr.ack != 0 || Buf2.Bufhdr.ack != 0) return;
   AcqBuf->Bufhdr.ack = -1;
   AcqBuf->Bufhdr.busy = -1;
   if (!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf))
     {
       printf("send pointer failure in wait_msg_buf\n");
       return;
     }
/*
*   Switch buffers for the acquisition process.
*/
   if (AcqBuf == &Buf1)
     {
/*
*   We were using Buf1, so switch to Buf2.
*/
       if (Buf2.Bufhdr.busy < 0)
         {
           NewBuf = &Buf2;
           *((void (**)())(ORNL_TIM_VEC*4)) = dump_event;
           trigger->imra = TIMER_INTR_ENA;
           return;
         }
       NewBuf = NULL;
       Buf2.Bufhdr.events = 0;
       AcqBuf = &Buf2;
       Event = Buf2.Data;
     }
   else
     {
/*
*   We were using Buf2, so switch to Buf1.
*/
       if (Buf1.Bufhdr.busy < 0)
         {
           NewBuf = &Buf1;
           *((void (**)())(ORNL_TIM_VEC*4)) = dump_event;
           trigger->imra = TIMER_INTR_ENA;
           return;
         }
       NewBuf = NULL;
       Buf1.Bufhdr.events = 0;
       AcqBuf = &Buf1;
       Event = Buf1.Data;
     }
   trigger->imra = EVT_INTR_ENA;
}
/****************************************************************************
*
*   Send a message pointer to the task which formats Ethernet packets and
*   sends them to the host.  Switch to the next buffer and return.
*
*   If we cannot send a message pointer or the next buffer is not available
*   switch to an alternate interrupt service routine.  The event interrupt
*   is disabled by the alternate handlers and on interrupt by PI/T inter #2
*   check for buffer available.  When a buffer is available switch back
*   to the normal Event interrupt service routine.
****************************************************************************/
int send_event(void)
{
/*
*   Send data buffer to Ethernet output task.
*/
   AcqBuf->Bufhdr.totalevents =
                          ((struct acq_share *)ACQ_SHARED_RAM)->event_number;
   ((struct acq_share *)ACQ_SHARED_RAM)->event_number += AcqBuf->Bufhdr.events;
   AcqBuf->Bufhdr.end_buf = Event;
   if (Buf1.Bufhdr.ack != 0 || Buf2.Bufhdr.ack != 0)
     {
       *((void (**)())(ORNL_TIM_VEC*4)) = wait_msg_buf;
       ((struct trig *)TRIGGER)->imra = TIMER_INTR_ENA;
       return (1);
     }
   AcqBuf->Bufhdr.ack = -1;
   AcqBuf->Bufhdr.busy = -1;
   if (!send_ptr_(DATA_MSG_SLOT,(char *)AcqBuf))
     {
       printf("Send pointer failed in send_event\n");
       ((struct trig *)TRIGGER)->imra = 0;
       return (1);
     }

/*
*   Switch buffers for the acquisition process.
*/
   if (AcqBuf == &Buf1)
     {
/*
*   We were using Buf1, so switch to Buf2.
*/
       if (Buf2.Bufhdr.busy < 0)
         {
           NewBuf = &Buf2;
           *((void (**)())(ORNL_TIM_VEC*4)) = dump_event;
           ((struct trig *)TRIGGER)->imra = TIMER_INTR_ENA;
           return (1);
         }
       Buf2.Bufhdr.events = 0;
       AcqBuf = &Buf2;
       Event = Buf2.Data;
     }
   else
     {
/*
*   We were using Buf2, so switch to Buf1.
*/
       if (Buf1.Bufhdr.busy < 0)
         {
           NewBuf = &Buf1;
           *((void (**)())(ORNL_TIM_VEC*4)) = dump_event;
           ((struct trig *)TRIGGER)->imra = TIMER_INTR_ENA;
           return (1);
         }
       Buf1.Bufhdr.events = 0;
       AcqBuf = &Buf1;
       Event = Buf1.Data;
     }
   return (0);
}
