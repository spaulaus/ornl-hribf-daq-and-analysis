/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1994-1998
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
*    Environment:  RMS Control System
*
*    File:         /usr/users/mcsq/Dvme3/rmsxx.c
*
*    Description:  Driver for all RMS control hardware.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/11/94    MCSQ         
*
*    6/12/94    MCSQ       Added routine for the Group 3 Teslameters.
*
*    9/25/94    MCSQ       Add routine for Alpha Power supplies.
*
*    9/29/94    MCSQ       Routines for ACROMAG, ITG1300 and DATEL628.
*
*   10/13/94    MCSQ       Revised for new structure for alpha ethernet
*                          packets.  Size of the reply packet now depends
*                          on device type.
*
*   11/22/94    MCSQ       Add routine for DATEL 613 16-bit ADC.  Changed
*                          DATEL 628 to DATEL 626 16-bit DAC.  Added local
*                          storage for last output values to the DAC.  These
*                          values are returned in response to a DAC read
*                          request.
*
*    3/25/95    MCSQ       Add routines for the finger stepper motors.
*                          Uses Green Spring IP-Stepper/IP-Digital 24
*                          industry packs mounted in a VIPC610 VME
*                          module.
*
*    3/27/95    MCSQ       Remove ITG1300 routines.
*
*    4/23/95    MCSQ       Add routines for a Granville-Phillips 303
*                          Vacuum Process Controller.
*
*    5/17/95    MCSQ       Change datel_dac_io write call so that only
*                          one of the six DACs is written per call.  Also,
*                          function acro now only changes 1 output bit
*                          per call.
*
*   10/12/95    MCSQ       Add no response detection in function tesla.
*                          Return error if teslameter does not respond to
*                          a read command.
*                          In function tesla, always send address for every
*                          command, add CR after each command string and
*                          allow two retries on serial error or no response.
*
*   11/ 2/95    MCSQ       Add DATEL 628 12-bit DAC.  Function datel_dac_io()
*                          handles both the DATEL 626 and DATEL 628.  The
*                          first 6 DAC channels are 16-bit and the last 8
*                          channels are 12-bit.  Made revisions to the 
*                          functions alpha(), tesla(), and granny().
*
*   11/ 9/95    MCSQ       Allow three retrys in function alpha if we get
*                          a serial error or no response.
*
*   12/ 7/95    MCSQ       Detect an undefined function request and
*                          return error code to host.
*
*    5/31/96    MCSQ       Report the position of the step motor as
*                          a signed integer. The 24 bit register is
*                          taken as a signed 24 bit integer and sign extended
*                          to a 32 bit integer.
*
*   10/ 2/96    MCSQ       Removed the test for zero steps in gs_move function.
*                          This allows the position counter to be set without
*                          any motor movement.
*
*   10/ 4/96    MCSQ       Fix error in put_xmit_char routine  when the
*                          the transmit buffer is full.  Was not restoring
*                          the proper interrupt level before return when
*                          call failed because the buffer was full.
*
*    3/11/98    MCSQ       Added digital low pass filter to ADC readout.
*                          Cutoff frequency is approx. 0.1 Hz.
*****************************************************************************/
#include "devices.h"
#include "vmic6015.h"
#include "acromag.h"
#include "vme_sys.h"
#include "system.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan.h"
#include "lan_lib.h"
#include "datel.h"
#include "rmsvme.h"
#include "stepper.h"

#include <stdio.h>
#include <stdlib.h>

#define  CR     0x0d
#define  LF     0x0a
#define  SPACE  0x020

/*    Function Prototypes        */
void mainloop(void);
int granny(int,struct granny_ctl *);
void gs_init(void);
char gs_move(struct stepper_ctl *);
char gs_get_status(struct stepper_status *);
void acro_init(void);
int acro(struct acro_ctl *);
void datel_adc_init(void);
void adc_intr(void);
int datel_dac_io(struct datel_dac_ctl *);
int datel_adc_io(struct datel_adc_ctl *);
int alpha(int ,struct alpha_ctl *);
int tesla(int ,struct tesla_ctl *);
int varian(int ,struct varian_ctl *);
void vmic_init(void);
void ch0_xmit(void);
void ch1_xmit(void);
void ch2_xmit(void);
void ch3_xmit(void);
void ch4_xmit(void);
void ch5_xmit(void);
void ch6_xmit(void);
void ch7_xmit(void);
void ch0_recv(void);
void ch1_recv(void);
void ch2_recv(void);
void ch3_recv(void);
void ch4_recv(void);
void ch5_recv(void);
void ch6_recv(void);
void ch7_recv(void);
void ch0_special(void);
void ch1_special(void);
void ch2_special(void);
void ch3_special(void);
void ch4_special(void);
void ch5_special(void);
void ch6_special(void);
void ch7_special(void);
void ch0_ext(void);
void ch1_ext(void);
void ch2_ext(void);
void ch3_ext(void);
void ch4_ext(void);
void ch5_ext(void);
void ch6_ext(void);
void ch7_ext(void);
void clr_recv_buf(struct recvbuf *);
void clr_xmit_buf(struct xmitbuf *);
int get_recv_count(struct recvbuf *);
int get_xmit_count(struct xmitbuf *);
int put_xmit_char(int , unsigned char );
int get_xmit_char(struct channel_data *, unsigned char *);
int put_recv_char(struct channel_data *, unsigned char , unsigned char *);
int get_recv_char(int , unsigned char *,unsigned char *);

/*      Global data              */
static struct devices *devtbl = DEVTBL;
struct channel_data channel[NUM_CHAN];
struct acromag *avme = (struct acromag *)ACROMAG;
struct datel_dac_regs *datel_dac626 = (struct datel_dac_regs *)DATEL626A;
struct datel_dac_regs *datel_dac628 = (struct datel_dac_regs *)DATEL628A;
struct datel_adc_regs *datel_adc = (struct datel_adc_regs *)DATEL613A;
struct stepper_ip *gs = (struct stepper_ip *)STEPPER;

unsigned short adc[16];
long xadc[16];

int tesla_nores,tesla_serial,tesla_buf,tesla_count,alpha_nores,alpha_serial;
int alpha_buf,alpha_count;
int ch_size;

/*****************************************************************************
*
*  Dummy main routine.  All it does is change to supervisor mode and
*  call the real main - mainloop.
*****************************************************************************/
void main()
{
  super_mode_();
  mainloop();
}
/*****************************************************************************
*****************************************************************************/
void mainloop(void)
{
  int    size;
  char   *inbuf,*outbuf;
  struct Ether_Packet *out_hdr;
  union Cmd *cmd,*rply;

  lan_open(PROTO_RMSSIO,&outbuf,&out_hdr);
  task_priority_(-1,1,69);
  ch_size = sizeof(struct channel_data);
  vmic_init();
  acro_init();
  gs_init();
  if (devtbl->datel613a)
    {
      datel_adc->cal = 0;
      delay_(50);
      datel_adc_init();
    }
  while (1)
   {
     size = lan_read(0,&inbuf);
     if (!size) 
       { 
         fprintf(stderr,"\nEthernet read error\n"); 
         exit(0);
       }
     cmd = (union Cmd *)inbuf;
     rply = (union Cmd *)outbuf;
     *rply = *cmd;
     if (cmd->tesla.func == TESLA)
       {
         rply->tesla.rpystat = tesla(0,(struct tesla_ctl *)rply);
         size = sizeof(struct tesla_ctl);
       }
     else if (cmd->alpha.func == ALPHA)
       {
         rply->alpha.rpystat = alpha(1,(struct alpha_ctl *)rply);
         size = sizeof(struct alpha_ctl);
       }
     else if (cmd->adc.func == DATEL_DAC)
       {
         rply->dac.rpystat = datel_dac_io((struct datel_dac_ctl *)rply);
         size = sizeof(struct datel_dac_ctl);
       }
     else if (cmd->adc.func == DATEL_ADC)
       {
         rply->adc.rpystat = datel_adc_io((struct datel_adc_ctl *)rply);
         size = sizeof(struct datel_adc_ctl);
       }
     else if (cmd->acro.func == ACRO)
       {
         rply->acro.rpystat = acro((struct acro_ctl *)rply);
         size = sizeof(struct acro_ctl);
       }
     else if (cmd->gs_ctl.func == GS_CTRL)
       {
         rply->gs_ctl.rpystat = gs_move((struct stepper_ctl *)rply);
         size = sizeof(struct stepper_ctl);
       }
     else if (cmd->gs_status.func == GS_STATUS)
       {
         rply->gs_status.rpystat = gs_get_status((struct stepper_status *)rply);
         size = sizeof(struct stepper_status);
       }
     else if (cmd->granny.func == GRANNY)
       {
         rply->granny.rpystat = granny(3,(struct granny_ctl *)rply);
         size = sizeof(struct granny_ctl);
       }
     else if (cmd->varian.func == VARIAN1)
       {
         rply->varian.rpystat = varian(4,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN2)
       {
         rply->varian.rpystat = varian(5,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN3)
       {
         rply->varian.rpystat = varian(6,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN4)
       {
         rply->varian.rpystat = varian(7,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else
       {
         size = sizeof(struct rmsio_stat);
         rply->varian.rpystat = VME_ERR_FUNC;
       }
     lan_reply(size,ACK);
   }
}
/****************************************************************************
*   Granville-Phillips 303 Vacuum Process Controller
****************************************************************************/
int granny(int ch,struct granny_ctl *cmd)
{
  int count,dly,err,i,j;
  unsigned char data,status;

  if (!devtbl->vmic6015a) return (IG_ERR_NONEXIST);
/*
*   Clear input buffer, clear channel error byte and send the command string.
*/
  channel[ch].err = 0;
  clr_recv_buf(&channel[ch].ibuf);
  i = 0;
  while(cmd->cmd[i] != '\0')
    {
      put_xmit_char(ch,cmd->cmd[i]);
      i++;
    }
/*
*   Wait for a reply from granny.
*/
  dly = 10;
  j = 0;
  count = 0;
  while(j < 3)
    {
      delay_(dly);
      i = get_recv_count(&channel[ch].ibuf);
      if (i != count)
        {
          count = i;
          j = 0;
        }
      dly = 1;
      j++;
    }
  if (count == 0) return (IG_ERR_NORESPOND);
  j = 0;
  i = 0;
  err = 0;
  if (channel[ch].err) err = IG_ERR_SERIALIO;
  while(1)
   {
     if (!get_recv_char(ch,&data,&status)) break;
     err |= status;
     cmd->reply[j] = data;
     j++;
     if (j >= sizeof(cmd->reply))
       {
         err = IG_ERR_BUFFULL;    /* Reply buffer full */
         break;
       }
   }
  cmd->reply[j] = 0;
  return (err);
}
/****************************************************************************
*  Green Spring IP-Stepper/IP-Digital 24 Initialization
************************** **************************************************/
void gs_init(void)
{
   unsigned char *uptr = (unsigned char *)gs->stepio;

   if (!devtbl->stepper) return;

/*
*  Initialize both motor channels
*/
   gs->m1_ctrl_stat = SSCMD | M_STOP;
   gs->m1_ctrl_stat = OPMODE;
   gs->m1_ctrl_stat = OUTMODE;
   gs->m2_ctrl_stat = SSCMD | M_STOP;
   gs->m2_ctrl_stat = OPMODE;
   gs->m2_ctrl_stat = OUTMODE;
/*
*  Set all 24 bits of IP-Digital 24 to 1s (i.e. all bits can be inputs).
*/
   *uptr++ = 0xff;
   *uptr++ = 0xff;
   uptr++;
   *uptr = 0xff;
/*
*  Initialize Extended mode registers R12 and R13
*/
   gs->m1_ctrl_stat = RSELECT | R12;
   gs->m1_data23 = 0;
   gs->m1_data15 = 0;
   gs->m1_data7 = 0x40;
   gs->m1_ctrl_stat = RSELECT | R13;
   gs->m1_data23 = 0x13;
   gs->m1_data15 = 0x20;
   gs->m1_data7 = 0;
   gs->m2_ctrl_stat = RSELECT | R12;
   gs->m2_data23 = 0;
   gs->m2_data15 = 0;
   gs->m2_data7 = 0x40;
   gs->m2_ctrl_stat = RSELECT | R13;
   gs->m2_data23 = 0x13;
   gs->m2_data15 = 0x20;
   gs->m2_data7 = 0;
}
/****************************************************************************
*  IP-Stepper Motor command function
****************************************************************************/
char gs_move(struct stepper_ctl *rply)
{
   static int whichmotor = 0xff;
   unsigned char *uptr,tmp;

   if (!devtbl->stepper) return (STEP_ERR_NONEXIST);

/*
*   Stop motor channel 1.  Bits 8-15 of the IP-Digital 24 select which
*   if any of the 7 motors.  Writing all 1s to the output disables all
*   motors.
*/
   gs->m1_ctrl_stat = SSCMD | M_STOP;
   if (rply->motor == 0)
     {
/*
*   Power off all motors.
*/
       gs->stepio[0] = 0xff;
       whichmotor = 0xff;
       return(0);            /* All done, just return */
     }
   else
     {
       if (rply->motor != whichmotor)
         {
/*
*  New motor selection.  If all motors were powered off, select the new
*  motor and wait one second.  Otherwise, turn all off for one second and
*  then select the new one.
*/
           if (whichmotor != 0xff)
             {
               gs->stepio[0] = 0xff;
               delay_(100);
             }
           gs->stepio[0] = rply->motor;
           whichmotor = rply->motor;
           delay_(100);
         }
     }
/*
*   Selection the direction of this move.
*/
   tmp = OPMODE;
   if (rply->dir != 0) tmp = tmp | M_CCW;
   gs->m1_ctrl_stat = tmp;
   gs->m1_ctrl_stat = OUTMODE;
/*
*   Load the rate multiplication register.
*/
   gs->m1_ctrl_stat = RSELECT | R7;
   uptr = (unsigned char *)&rply->multi;
   uptr += 1;
   gs->m1_data23 = 0;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Load the low rate FL register.
*/
   gs->m1_ctrl_stat = RSELECT | R1;
   uptr = (unsigned char *)&rply->lspeed;
   uptr += 1;
   gs->m1_data23 = 0;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Load the high rate FH1 register.
*/
   gs->m1_ctrl_stat = RSELECT | R2;
   uptr = (unsigned char *)&rply->hspeed;
   uptr += 1;
   gs->m1_data23 = 0;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Load the Acceleration rate register.  Since we always use the automatic
*   ramp-down point mode, this is also the Deceleration rate.
*/
   gs->m1_ctrl_stat = RSELECT | R4;
   uptr = (unsigned char *)&rply->accel;
   uptr += 1;
   gs->m1_data23 = 0;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Set the current position register.
*/
   gs->m1_ctrl_stat = RSELECT | R10;
   uptr = (unsigned char *)&rply->position;
   uptr += 2;
   gs->m1_data23 = *uptr--;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Load the down counter (i.e. number of steps to move).
*/
   gs->m1_ctrl_stat = RSELECT | R0;
   uptr = (unsigned char *)&rply->steps;
   uptr += 2;
   gs->m1_data23 = *uptr--;
   gs->m1_data15 = *uptr--;
   gs->m1_data7 = *uptr;
/*
*   Start motor.
*/
   gs->m1_ctrl_stat = SSCMD | M_START;
   return (0);
}
/****************************************************************************
*   IP-Stepper status return function
****************************************************************************/
char gs_get_status(struct stepper_status *rply)
{
   unsigned char *uptr;

   if (!devtbl->stepper) return (STEP_ERR_NONEXIST);

   rply->position = 0;
   rply->in_bits = 0;
   rply->out_bits = 0;
/*
*   Return the now current position.
*/
   gs->m1_ctrl_stat = RSELECT | R10;
   uptr = (unsigned char *)&rply->position;
   uptr += 3;
   if ((gs->m1_data23 & 0x80) == 0) *uptr-- = 0;
   else  *uptr-- = 0xff;
   *uptr-- = gs->m1_data23;
   *uptr-- = gs->m1_data15;
   *uptr = gs->m1_data7;
/*
*   Return primary status byte
*/
   rply->stat1 = gs->m1_ctrl_stat;
/*
*   Read and return the extended status bytes - 16-bits.
*/
   gs->m1_ctrl_stat = RSELECT | R17;
   rply->stat2 = gs->m1_data15;
   rply->stat3 = gs->m1_data7;
/*
*   Get and return the IP-Digital 24 input bytes
*/
   uptr = (unsigned char *)&rply->in_bits;
   *uptr++ = ~gs->stepio[5];
   *uptr++ = gs->stepio[4];
   *uptr = ~gs->stepio[7];
/*
*   Also return the IP-Digital 24 output bytes.
*/
   uptr = (unsigned char *)&rply->out_bits;
   *uptr++ = gs->stepio[1];
   *uptr++ = gs->stepio[0];
   *uptr = gs->stepio[3];
   return (0);
}
/****************************************************************************
*   ACROMAG initialization
************************** **************************************************/
void acro_init(void)
{
   if (!devtbl->acromag) return;
   avme->csr = AVME_GRN_ON | AVME_RED_OFF;
   avme->intr_vec0 = 0;
   avme->intr_vec1 = 0;
   avme->intr_vec2 = 0;
   avme->intr_vec3 = 0;
   avme->intr_vec4 = 0;
   avme->intr_vec5 = 0;
   avme->intr_vec6 = 0;
   avme->intr_vec7 = 0;
}
/****************************************************************************
*   ACROMAG Digital I/O
****************************************************************************/
int acro(struct acro_ctl *rply)
{
  int i;
  unsigned char mask = 1;
  volatile unsigned char *port;

  if (!devtbl->acromag) return (AVME_ERR_NONEXIST);
  if (rply->bitnum < 0 || rply->bitnum > 63) return(-5);
  if (rply->rw)
    {
/*       Write a bit in the I/O register   */
      port = &(avme->port0) + rply->bitnum/8;
      mask = mask << (rply->bitnum % 8);
      if (rply->data[0] == 0)
        {
          mask = ~mask;
          *port = *port & mask;
        }
      else
        {
          *port = *port | mask;
        }
    }
  else
    {
/*       Read I/O registers   */
      port = &(avme->port0);
      for(i=0; i < 8; i++) rply->data[i] = *port++;
    }
  return (0);
}
/****************************************************************************
*   Initialize Datel 613  ADCs
****************************************************************************/
void datel_adc_init(void)
{
   int i,j;

   if (devtbl->datel613a)
     {
       datel_adc->csr = LED_ON_613 | CNV_ENB_613 | START_REG_613;
       for(i=0; i < 16; i++)
         {
           datel_adc->addr = i;
           for(j=1000; j > 0; j--);     /* approx 80 usec delay */
           datel_adc->start = 0;
           while(!(datel_adc->csr & EOC_613));
           adc[i] = datel_adc->data;
           xadc[i] = (long)adc[i] << 15;
         }
       *((void (**)(void))((DATEL_ADC_VEC)*4)) = adc_intr;
       datel_adc->vector = DATEL_ADC_VEC;
       datel_adc->addr = 0;
       datel_adc->csr = LED_ON_613 | CNV_ENB_613 | START_REG_613 | INT_ENB_613;
     }
}
/****************************************************************************
*   Datel 613 interrupt routine
*
*   Pacer clock is set to 61.03 Hz.  We sample each ADC channel at 1/8 th
*   that rate or 7.63 Hz.  The digital low pass filter for the Datel
*   ADCs has a cutoff frequency of approx. 0.08 Hz.
*
****************************************************************************/
void adc_intr(void)
{
#pragma ghs interrupt

  unsigned short *sptr;
  static i = 0;
  long diff;
  int  adcdat,j;

  datel_adc->start = 0;
  while(!(datel_adc->csr & EOC_613));
  adcdat = datel_adc->data;
  diff = ((long)adcdat << 15) - xadc[i];
  xadc[i] += diff >> 4;
  adc[i] = xadc[i] >> 15;
  i++;
  datel_adc->addr = i;
  for(j=1000; j > 0; j--);     /* approx 80 usec delay */
  datel_adc->start = 0;
  while(!(datel_adc->csr & EOC_613));
  adcdat = datel_adc->data;
  diff = ((long)adcdat << 15) - xadc[i];
  xadc[i] += diff >> 4;
  adc[i] = xadc[i] >> 15;
  i++;
  if (i >= 16) i = 0;
  datel_adc->addr = i;
  return;
}
/****************************************************************************
*   Return ADC readings to host
****************************************************************************/
int datel_adc_io(struct datel_adc_ctl *rply)
{
  int k;

  if (!devtbl->datel613a) return (DATEL_ERR_NONEXIST);
  for (k=0; k < 16; k++) rply->data[k] = adc[k];
  byte_swap_((char *)rply->data,sizeof(rply->data));
  return (0);
}
/****************************************************************************
*   Read/Write Datel 626 and Datel 628
****************************************************************************/
int datel_dac_io(struct datel_dac_ctl *rply)
{
  int i,j;
  unsigned short *dac;
  static unsigned short val[sizeof(rply->data)];

  if (rply->dacnum <= 5)
    {
      if (!devtbl->datel626a) return (DATEL_ERR_NONEXIST);
      dac = &datel_dac626->dac0;
      dac += rply->dacnum;
    }
  else
    {
      if (!devtbl->datel628a) return (DATEL_ERR_NONEXIST);
      dac = &datel_dac628->dac0;
      dac += (rply->dacnum - 6);
    }
  if (rply->rw)
    {
      i = sizeof(rply->data);
      byte_swap_((char *)rply->data,i);
      *dac = rply->data[0];
      val[rply->dacnum] = rply->data[0];
    }
  else
    {
      i = sizeof(rply->data)/sizeof(short);
      for(j=0; j < i; j++) rply->data[j] = val[j];
      i = i * sizeof(short);
      byte_swap_((char *)rply->data,i);
    }
  return (0);
}
/****************************************************************************
*   Alpha Scientific Power Supplies
****************************************************************************/
int alpha(int ch,struct alpha_ctl *cmd)
{
  int count,err,i,j,retry = 3;
  char  addrstr[8];
  unsigned char data,status;

  if (!devtbl->vmic6015a) return (ALPHA_ERR_NONEXIST);
  alpha_count++;
  while(retry--)
   {
/*
 *  If the controller number is non-zero, convert the controller number to
 *  ASCII and prefix the message with the 2 digit controller number.
 */
     if (cmd->controller)
       {
         sprintf(addrstr,"%2.2i",cmd->controller);
         put_xmit_char(ch,addrstr[0]);
         put_xmit_char(ch,addrstr[1]);
       }
/*
 *  Send the ASCII command string.
 */
     i = 0;
     while(cmd->data[i] != '\0')
       {
         put_xmit_char(ch,cmd->data[i]);
         i++;
       }
     put_xmit_char(ch,CR);
/*
 *   Clear input buffer, clear channel error byte.
 */
     channel[ch].err = 0;
     clr_recv_buf(&channel[ch].ibuf);
     if (strncmp(cmd->data,"ON",2) == 0) delay_(100);
/*
 *   Wait for a reply from the power supply
 */
     delay_(15);
     j = 0;
     count = 0;
     while(j < 3)
       {
         i = get_recv_count(&channel[ch].ibuf);
         if (i != count)
           {
             count = i;
             if (count >= sizeof(cmd->data)) return(ALPHA_ERR_BUFFULL);
             j = 0;
           }
         delay_(1);
         j++;
       }
     j = 0;
     i = 0;
     err = 0;
     if (channel[ch].err)
       {
         err = ALPHA_ERR_SERIALIO;
         delay_(10);
alpha_serial++;
break;
         continue;
       }
     else if (count == 0)
       {
         err = ALPHA_ERR_NORESPOND;
         delay_(10);
alpha_nores++;
break;
         continue;
       }
     while(1)
       {
         if (!get_recv_char(ch,&data,&status)) break;
         err |= status;
         cmd->data[j] = data;
         j++;      
         if (j >= sizeof(cmd->data))
           {
             err = ALPHA_ERR_BUFFULL;      /* Reply buffer full */
             j--;
             alpha_buf++;
             break;
           }
        }
     cmd->data[j] = 0;
     break;
   }
  return (err);
}
/****************************************************************************
*   Group 3 DTM132 Teslameters
****************************************************************************/
int tesla(int ch,struct tesla_ctl *cmd)
{
  int count,dly,err,i,j,retry = 3;
  char  addrstr[8],first;
  unsigned char data,status;

  if (!devtbl->vmic6015a) return (TESLA_ERR_NONEXIST);

  tesla_count++;
  while (retry-- > 0)
    {
/*
*    Address the controller.
*/
     sprintf(addrstr,"a%i",cmd->controller);
     i = 0;
     while(addrstr[i] != '\0')
       {
         put_xmit_char(ch,addrstr[i]);
         i++;
       }
     put_xmit_char(ch,CR);     /* Need CR at end of An command */
     delay_(4);                /* Wait for echo                */
/*
*   Clear input buffer, clear channel error byte and send the command string.
*/
     channel[ch].err = 0;
     clr_recv_buf(&channel[ch].ibuf);
     i = 0;
     first = cmd->cmd[0];
     while(cmd->cmd[i] != '\0')
       {
         put_xmit_char(ch,cmd->cmd[i]);
         i++;
       }
/*
*   Wait for a reply from the teslameter.
*/
     dly = 3;
     j = 0;
     count = 0;
     while(j < 3)
       {
         delay_(dly);
         i = get_recv_count(&channel[ch].ibuf);
         if (i != count)
           {
             count = i;
             j = 0;
           }
         dly = 1;
         j++;
       }
     if (count == 0) return (TESLA_ERR_BROKEN);  /* Serial loop open */
     err = 0;
     if (channel[ch].err)
       {
         err = TESLA_ERR_SERIALIO;
         tesla_serial++;
         delay_(10);
         continue;
       }
     if (count >= (sizeof(cmd->reply)))
       {
         err = TESLA_ERR_BUFFULL;
         tesla_buf++;
         delay_(10);
         continue;
       }
     j = 0;
     i = 0;
     while(1)
       {
         if (!get_recv_char(ch,&data,&status)) break;
         err |= status;
         if (i)
           {
             if (data == LF) data = SPACE;
             else if (data == CR) data = SPACE;
             cmd->reply[j] = data;
             j++;
           }
         if (data == SPACE) i = 1;
       }
     cmd->reply[j] = 0;
     if (j == 0)
       {
         if (first == 'F') err = TESLA_ERR_NORESPOND;
         else if (first == 'f') err = TESLA_ERR_NORESPOND;
         else if (first == 'P') err = TESLA_ERR_NORESPOND;
         else if (first == 'p') err = TESLA_ERR_NORESPOND;
         else if (first == 'W') err = TESLA_ERR_NORESPOND;
         else if (first == 'w') err = TESLA_ERR_NORESPOND;
         else if (first == 'I') err = TESLA_ERR_NORESPOND;
         else if (first == 'i') err = TESLA_ERR_NORESPOND;
         if (err != 0)
           {
             tesla_nores++;
             delay_(10);
             continue;
           }
       }
     return (err);
    }
  return (err);
}
/****************************************************************************
*  Varian Multigauge Controllers
****************************************************************************/
int varian(int ch,struct varian_ctl *cmd)
{
  int err,i,j;
  unsigned char data,status;

  if (!devtbl->vmic6015b) return (IG_ERR_NONEXIST);
  channel[ch].err = 0;
  clr_recv_buf(&channel[ch].ibuf);
  i = 0;
  while(i < cmd->cmdbytes)
    {
      put_xmit_char(ch,cmd->cmd[i]);
      i++;
    }
  status = cmd->cmd[0] >> 1;
  if (status == 0x18 || status == 0x20 || cmd->cmd[0] == 0x33 ||
                                                cmd->cmd[0] == 6) delay_(100);
  else if (status == 8 || status == 9) delay_(3);
  else  delay_(cmd->rpybytes + cmd->cmdbytes);
  i = j = 0;
  err = 0;
  while(i < 3)
   {
     do
      {
        if (!get_recv_char(ch,&data,&status)) break;
        cmd->reply[j] = data;
        if (j == 0 && data == 0xff) return (IG_ERR_INVALID);
        if (status) err = IG_ERR_SERIALIO;
        j++;
      }
     while (j < cmd->rpybytes);
     if (j >= cmd->rpybytes) return (err);
     i++;
     delay_(2);
   }
  return (IG_ERR_NORESPOND);
}
/****************************************************************************
*   Initialize VMIC 6015 Quad-Serial I/O Modules
*
****************************************************************************/
void vmic_init(void)
{
  struct vmic_ch *vmic;
  struct vmic_bim *bim;
  int  i;

/*
*  Initialize transmit buffer empty vectors
*/
   if (devtbl->vmic6015a)
     {
       *((void (**)())((VMIC_VECS+4)*4)) = ch0_xmit;
       *((void (**)())((VMIC_VECS+0)*4)) = ch1_xmit;
       *((void (**)())((VMIC_VECS+12)*4)) = ch2_xmit;
       *((void (**)())((VMIC_VECS+8)*4)) = ch3_xmit;
     }
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+20)*4)) = ch4_xmit;
       *((void (**)())((VMIC_VECS+16)*4)) = ch5_xmit;
       *((void (**)())((VMIC_VECS+28)*4)) = ch6_xmit;
       *((void (**)())((VMIC_VECS+24)*4)) = ch7_xmit;
     }
/*
*  Initialize the receive character interrupt vectors
*/
   if (devtbl->vmic6015a)
     {
       *((void (**)())((VMIC_VECS+6)*4)) = ch0_recv;
       *((void (**)())((VMIC_VECS+2)*4)) = ch1_recv;
       *((void (**)())((VMIC_VECS+14)*4)) = ch2_recv;
       *((void (**)())((VMIC_VECS+10)*4)) = ch3_recv;
     }
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+22)*4)) = ch4_recv;
       *((void (**)())((VMIC_VECS+18)*4)) = ch5_recv;
       *((void (**)())((VMIC_VECS+30)*4)) = ch6_recv;
       *((void (**)())((VMIC_VECS+26)*4)) = ch7_recv;
     }
/*
*  Initialize the special receive interrupt vectors
*/
   if (devtbl->vmic6015a)
     {
       *((void (**)())((VMIC_VECS+7)*4)) = ch0_special;
       *((void (**)())((VMIC_VECS+3)*4)) = ch1_special;
       *((void (**)())((VMIC_VECS+15)*4)) = ch2_special;
       *((void (**)())((VMIC_VECS+11)*4)) = ch3_special;
     }
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+23)*4)) = ch4_special;
       *((void (**)())((VMIC_VECS+19)*4)) = ch5_special;
       *((void (**)())((VMIC_VECS+31)*4)) = ch6_special;
       *((void (**)())((VMIC_VECS+27)*4)) = ch7_special;
     }
/*
*  Initialize the external/status change interrupt vectors
*/
   if (devtbl->vmic6015a)
     {
       *((void (**)())((VMIC_VECS+5)*4)) = ch0_ext;
       *((void (**)())((VMIC_VECS+1)*4)) = ch1_ext;
       *((void (**)())((VMIC_VECS+13)*4)) = ch2_ext;
       *((void (**)())((VMIC_VECS+9)*4)) = ch3_ext;
     }
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+21)*4)) = ch4_ext;
       *((void (**)())((VMIC_VECS+17)*4)) = ch5_ext;
       *((void (**)())((VMIC_VECS+29)*4)) = ch6_ext;
       *((void (**)())((VMIC_VECS+25)*4)) = ch7_ext;
     }
  if (devtbl->vmic6015a)
    {
      vmic = (struct vmic_ch *)VMIC6015A;
      bim = (struct vmic_bim *)(VMIC6015A + BIM);
      bim->cr0 = bim->cr1 = bim->cr2 = bim->cr3 = 0;
      bim->vr0 = bim->vr1 = bim->vr2 = bim->vr3 = 0;
      for (i=0; i < 4; i++)
        {
          vmic->cmdreg = VMIC_CH_RESET;  /* Reset channel  */
/*
*   Setup for Asynchronous mode, X16 clock, no parity and 1 stop bit
*/
          vmic->modectl = 0x40 | VMIC_STOP1;
/*
*   Set for 9600 baud and enable baud rate generator.
*/
          vmic->tcreg = channel[i].tcreg = VMIC_9600;
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
          channel[i].mask = 0xff;
          channel[i].xcnt = 
                        sizeof(channel[i].ibuf.buf)/sizeof(struct indata) - 80;
          channel[i].xflg = 0;
          channel[i].xidle = 0;
          vmic++;
        }
      bim->cr2 = bim->cr3 = 0x35;
    }
  if (devtbl->vmic6015b)
    {
      vmic = (struct vmic_ch *)VMIC6015B;
      bim = (struct vmic_bim *)(VMIC6015B + BIM);
      bim->cr0 = bim->cr1 = bim->cr2 = bim->cr3 = 0;
      bim->vr0 = bim->vr1 = bim->vr2 = bim->vr3 = 0;
      for (i=4; i < 8; i++)
        {
          vmic->cmdreg = VMIC_CH_RESET;  /* Reset channel  */
/*
*   Setup for Asynchronous mode, X16 clock, no parity and 1 stop bit
*/
          vmic->modectl = 0x40 | VMIC_STOP1;
/*
*   Set for 4800 baud and enable baud rate generator.
*/
          vmic->tcreg = channel[i].tcreg = VMIC_4800;
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
          channel[i].mask = 0xff;
          channel[i].xcnt = 
                        sizeof(channel[i].ibuf.buf)/sizeof(struct indata) - 80;
          channel[i].xflg = 0;
          channel[i].xidle = 0;
          vmic++;
        }
      bim->cr2 = bim->cr3 = 0x35;
    }
}
/*****************************************************************************
*****************************************************************************/
void ch0_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[0];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch1_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[1];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch2_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[2];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch3_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[3];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch4_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[4];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch5_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[5];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch6_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[6];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch7_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[7];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch0_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[0];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch1_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[1];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch2_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[2];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata,tdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
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
/*****************************************************************************
*****************************************************************************/
void ch4_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[4];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch5_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[5];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch6_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[6];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch7_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[7];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch0_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[0];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch1_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[1];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
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
/*****************************************************************************
*****************************************************************************/
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
/*****************************************************************************
*****************************************************************************/
void ch4_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[4];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch5_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[5];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch6_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[6];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch7_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[7];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch0_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[0].vmic;
  unsigned char dat;

  channel[0].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch1_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[1].vmic;
  unsigned char dat;

  channel[1].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch2_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[2].vmic;
  unsigned char dat;

  channel[2].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch3_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[3].vmic;
  unsigned char dat;

  channel[3].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch4_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[4].vmic;
  unsigned char dat;

  channel[4].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch5_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[5].vmic;
  unsigned char dat;

  channel[5].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch6_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[6].vmic;
  unsigned char dat;

  channel[6].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch7_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[7].vmic;
  unsigned char dat;

  channel[7].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*  Clear the receive buffer.
*****************************************************************************/
void clr_recv_buf(struct recvbuf *bufptr)
{
   int i,level;

   level = set_intr_level_(0x700);
   bufptr->input = bufptr->output = &bufptr->buf[0];
/**************
   for (i=0; i < IN_BUF_SIZE; i++)
     {
       bufptr->buf[i].err = 0;
       bufptr->buf[i].data = 0;
     }
**************/
   set_intr_level_(level);
}
/*****************************************************************************
*   Clear the transmit buffer.
*****************************************************************************/
void clr_xmit_buf(struct xmitbuf *bufptr)
{
   int level;

   level = set_intr_level_(0x700);
   bufptr->input = bufptr->output = bufptr->buf;
   set_intr_level_(level);
}
/*****************************************************************************
*  Get number of characters in the receive buffer.
*  Call:  bufptr  -  pointer to receive buffer
*
*  Return:
*          0  means buffer is empty
*        > 0  buffer character count
*         -1  means the buffer is full
*****************************************************************************/
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
/*****************************************************************************
*  Get number of characters in the transmit buffer.
*  Call:  bufptr  -  pointer to transmit buffer
*
*  Return:
*          0  means buffer is empty
*        > 0  buffer character count
*         -1  means the buffer is full
*****************************************************************************/
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
/*****************************************************************************
*  Put one character into the transmit buffer.  If the transmitter is idle,
*  get one character from the buffer and start the transmitter.
*  Call:   chan  -  output channel number
*          data  -  character data
*
*  Return:
*          0  means buffer is full.  Nonzero means OK.
*
*****************************************************************************/
int put_xmit_char(int chan, unsigned char data)
{
   struct xmitbuf *ptr = &channel[chan].obuf;
   int count,level;

   level = set_intr_level_(0x700);
   count = get_xmit_count(ptr);
   if (count < 0)
     {
       set_intr_level_(level);
       return (0);
     }
   *ptr->input++ = data;
   if ((ptr->input - ptr->buf) >= OUT_BUF_SIZE) ptr->input = ptr->buf;
   if (count == 0 && channel[chan].xidle == 0)
     {
       get_xmit_char(&channel[chan],&data);
       channel[chan].xidle = 0xff;
       channel[chan].vmic->datarg = data;
     }
   set_intr_level_(level);
   return (1);
}
/*****************************************************************************
*  Get one character from the transmit buffer.
*  Call:   ptr  -  Pointer to transmit buffer structure
*
*  Return:  0 means buffer is empty.  Nonzero means OK.
*          data -  character data
*****************************************************************************/
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
/*****************************************************************************
*  Put one character in the receive buffer.  Routine is called only by receive
*  interrupt routines.
*  Call:  ptr   -  pointer to the receive buffer structure
*         data  -  the received character
*         status - status bits from UART status register 1
*
*  Return:  0  means buffer is full.  Nonzero means OK.
*****************************************************************************/
int put_recv_char(struct channel_data *chan, unsigned char data,
                                                        unsigned char *status)
{
   int count;
   struct recvbuf *ptr = &chan->ibuf;

   count = get_recv_count(ptr);   /* get buffer character count     */
   if (count < 0)
     {
/*
*   Receiver buffer is full.  Put the Buffer overrun bit in the status
*   and return buffer full status.
*/
       *status |= 0x8;
       (ptr->input)->err = *status;
       return (0);
     }
    if (chan->xflg && count >= chan->xcnt)
      {
        if (!chan->xidle)
          {
            chan->xidle = 0xff;
            (chan->vmic)->datarg = XOFF;
            chan->txoff = XOFF;
          }
        else  chan->nxchar = XOFF;
      }
/*
*   Put the character and status in the buffer.  Increment the buffer
*   pointer and return OK status.
*/
   (ptr->input)->data = data;
   (ptr->input)->err = *status;
   ptr->input++;
   if ((ptr->input - ptr->buf) >= IN_BUF_SIZE) ptr->input = ptr->buf;
   return (1);
}
/*****************************************************************************
*  Get one character and the error status byte from a receive buffer.
*  Call:   chan  -  Serial channel number(0 thru 7)
*
*  return:   0  means buffer is empty.  Nonzero means valid char
*          data   -  character
*          status - error status
*
*****************************************************************************/
int get_recv_char(int chan, unsigned char *data,unsigned char *status)
{
   struct recvbuf *ptr = &channel[chan].ibuf;
   int count;

/*
*   If the buffer is empty, just return buffer empty flag.
*/
   count = get_recv_count(ptr);
   if (!count) return 0;
/*
*   Return the character and status.  Increment buffer pointer and
*   return OK status.
*/
   *data = (ptr->output)->data;
   *status = (ptr->output)->err;
   ptr->output++;
   if ((ptr->output - ptr->buf) >= IN_BUF_SIZE) ptr->output = ptr->buf;
   if (channel[chan].txoff && count < channel[chan].xcnt) ;
   return (1);
}
