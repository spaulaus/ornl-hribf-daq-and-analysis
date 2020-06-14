/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1995-1998
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
*    File:         /usr/users/mcsq/Dvme3/drsxx.c
*
*    Description:  Driver for all DRS VME control hardware.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/22/95    MCSQ         
*
*    2/18/97    MCSQ       Add VMIC3128 64 channel 14-bit ADC
*
*   11/24/97    MCSQ       Add code for stepper motors.  Copied from
*                          rmsxx.c
*
*    5/28/98    MCSQ       Moved Danfysik power supplies from channel 5 to
*                          channel seven. (channel 5 has a bad SN75175).
*                          Also corrected test for module present in danfysik
*                          routine.  It was testing for vmic_6015a were it
*                          should be vmic_6015b.
*****************************************************************************/
#include "devices.h"
#include "vmic6015.h"
#include "vmic3128.h"
#include "vme_sys.h"
#include "system.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan.h"
#include "lan_lib.h"
#include "datel.h"
#include "drsvme.h"
#include "stepper.h"

#include <stdio.h>
#include <stdlib.h>


/*    Function Prototypes        */
void mainloop(void);
void gs_init(void);
char gs_move(struct stepper_ctl *);
char gs_get_status(struct stepper_status *);
void vmic_3128_init(void);
void datel_adc_init(void);
void adc_intr(void);
int read_adcs(struct read_adc *);
int danfysik(int ,struct danfysik_ctl *);
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
struct datel_adc_regs *datel_adc1 = (struct datel_adc_regs *)DATEL613A;
struct datel_adc_regs *datel_adc2 = (struct datel_adc_regs *)DATEL613B;
struct stepper_ip *gs = (struct stepper_ip *)STEPPER;

int  adc[80];

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
void mainloop()
{
  int    size;
  char   *inbuf,*outbuf;
  struct Ether_Packet *out_hdr;
  union Cmd *cmd,*rply;

  lan_open(PROTO_RMSSIO,&outbuf,&out_hdr);
  task_priority_(-1,1,69);
  ch_size = sizeof(struct channel_data);
  vmic_init();
  if (devtbl->datel613a) datel_adc1->cal = 0;
  if (devtbl->datel613b) datel_adc2->cal = 0;
  if (devtbl->vmic3128a) vmic_3128_init();
  datel_adc_init();
  gs_init();
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
     if (cmd->danfysik.func == DANFYSIK)
       {
         rply->danfysik.rpystat = danfysik(7,(struct danfysik_ctl *)rply);
         size = sizeof(struct danfysik_ctl);
       }
     else if (cmd->adc.func == READ_ADCS)
       {
         rply->adc.rpystat = read_adcs((struct read_adc *)rply);
         size = sizeof(struct read_adc);
       }
     else if (cmd->varian.func == VARIAN1)
       {
         rply->varian.rpystat = varian(0,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN2)
       {
         rply->varian.rpystat = varian(1,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN3)
       {
         rply->varian.rpystat = varian(2,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
       }
     else if (cmd->varian.func == VARIAN4)
       {
         rply->varian.rpystat = varian(3,(struct varian_ctl *)rply);
         size = sizeof(struct varian_ctl);
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
     else
       {
         size = sizeof(struct drsio_stat);
         rply->varian.rpystat = VME_ERR_FUNC;
       }
     lan_reply(size,ACK);
   }
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
       gs->stepio[3] = 0xff;
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
               gs->stepio[3] = 0xff;
               delay_(100);
             }
           gs->stepio[3] = rply->motor;
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
   *uptr++ = ~gs->stepio[4];
   *uptr = gs->stepio[7];
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
*
*  Initialize the VMIC 3128 64 channel, 14-bit ADC.
*
****************************************************************************/
void vmic_3128_init(void)
{
   struct vmic3128_adc *vmic = (struct vmic3128_adc *)VMIC3128A;
   int  i;
/*
*   gain codes are:  0 - X1
*                    1 - X10
*                    2 - X100
*                    3 - invalid code
*/
   static char gains[64] =
         {1,1,1,2,2,2,2,2,2,2,2,2,2,2,1,1,
          1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

/*
*   Reset ADC and wait till first scan is complete
*/
   vmic->csr = V3128_SOFT_RESET;
   while (!(vmic->csr && V3128_NEW_DATA));
/*
*   Set gains and wait till first scan is complete
*/
   for(i=0; i < 64; i++)
     {
       vmic->csr = V3128_SET_GAIN + i;
       vmic->buff[0] = gains[i];
     }
   vmic->csr = 0;
   while (!(vmic->csr && V3128_NEW_DATA));

   vmic->csr = V3128_LED_OFF;
}
/****************************************************************************
*   Initialize Datel 613  ADCs
****************************************************************************/
void datel_adc_init(void)
{
   if (devtbl->datel613a)
     {
       *((void (**)(void))((DATEL_ADC_VEC)*4)) = adc_intr;
       datel_adc1->vector = DATEL_ADC_VEC;
       datel_adc1->addr = 0;
       datel_adc1->csr = LED_ON_613 | CNV_ENB_613 | START_REG_613 | INT_ENB_613;
       if (devtbl->datel613b)
         {
           datel_adc2->addr = 0;
           datel_adc2->csr = LED_ON_613 | CNV_ENB_613 | START_REG_613;
         }
     }
   else if (devtbl->datel613b)
     {
       *((void (**)(void))((DATEL_ADC_VEC)*4)) = adc_intr;
       datel_adc2->vector = DATEL_ADC_VEC;
       datel_adc2->addr = 0;
       datel_adc2->csr = LED_ON_613 | CNV_ENB_613 | START_REG_613 | INT_ENB_613;
     }
}
/****************************************************************************
*   Datel 613 interrupt routine
*
*   Pacer clock is set to 61.03 Hz.  We sample each ADC channel at 1/8 th
*   that rate or 7.63 Hz.  The digital low pass filter for the Datel
*   ADCs has a cutoff frequency of approx. 0.08 Hz.
*
*   The filter for the VMIC ADC has a cutoff frequency of approx 1.3 Hz.
****************************************************************************/
void adc_intr(void)
{
#pragma ghs interrupt

  struct vmic3128_adc *vmic = (struct vmic3128_adc *)VMIC3128A;
  unsigned short *sptr;
  static long xadc[80];
  static i = 0;
  long diff;
  int  adcdat,k;

  if (devtbl->datel613a) datel_adc1->start = 0;
  if (devtbl->datel613b) datel_adc2->start = 0;
  if (devtbl->datel613a)
    {
      while(!(datel_adc1->csr & EOC_613));
      adcdat = datel_adc1->data;
      diff = ((long)adcdat << 15) - xadc[i];
      xadc[i] += diff >> 4;
      adc[i] = xadc[i] >> 15;
    }
  if (devtbl->datel613b)
    {
      while(!(datel_adc2->csr & EOC_613));
      adcdat = datel_adc2->data;
      diff = ((long)adcdat << 15) - xadc[i+8];
      xadc[i+8] += diff >> 4;
      adc[i+8] = xadc[i+8] >> 15;
    }
  if (devtbl->vmic3128a)
    {
      sptr = &vmic->buff[0];
      for (k=16; k < 80; k++)
        {
          adcdat = *sptr++;
          diff = ((long)adcdat << 14) - xadc[k];
          xadc[k] += diff >> 3;
          adc[k] = xadc[k] >> 14;
        }
    }
  i++;
  if (i >= 8) i = 0;
  if (devtbl->datel613a) datel_adc1->addr = i;
  if (devtbl->datel613b) datel_adc2->addr = i;
  return;
}
/****************************************************************************
*   Return ADC readings to host
****************************************************************************/
int read_adcs(struct read_adc *rply)
{
  int k;

  if (!devtbl->datel613a && !devtbl->datel613b && !devtbl->vmic3128a)
                                                    return (DATEL_ERR_NONEXIST);
  for (k=0; k < 80; k++) rply->data[k] = adc[k];
  byte_swap_((char *)rply->data,sizeof(rply->data));
  return (0);
}
/****************************************************************************
*   Danfysik Scientific Power Supplies
****************************************************************************/
int danfysik(int ch,struct danfysik_ctl *cmd)
{
  int count,dly,err,i,j;
  char  addrstr[8];
  unsigned char data,status;

  if (!devtbl->vmic6015b) return (DANFYSIK_ERR_NONEXIST);
/*
 *  If the controller number is non-zero, convert the controller number to
 *  ASCII and prefix the message with the 2 digit controller number.
 */
  if (cmd->controller)
    {
      put_xmit_char(ch,'A');
      put_xmit_char(ch,'D');
      put_xmit_char(ch,'R');
      put_xmit_char(ch,' ');
      sprintf(addrstr,"%2.2i",cmd->controller);
      put_xmit_char(ch,addrstr[0]);
      put_xmit_char(ch,addrstr[1]);
      put_xmit_char(ch,13);
      delay_(2);
    }
/*
 *  Send the ASCII command string.
 */
  i = 0;
  while(cmd->data[i] != '\0')
    {
      put_xmit_char(ch,cmd->data[i]);
      i++;
  delay_(1);
    }
  put_xmit_char(ch,13);
/*
 *   Clear input buffer, clear channel error byte.
 */
  channel[ch].err = 0;
  clr_recv_buf(&channel[ch].ibuf);
/*
 *   Wait for a reply from the power supply
 */
  dly = 2;
  j = 0;
  count = 0;
  while(j < 2)
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
  j = 0;
  i = 0;
  err = 0;
  if (channel[ch].err) err = DANFYSIK_ERR_SERIALIO;
  else if (count == 0 && cmd->rpymess != 0) err = DANFYSIK_ERR_NORESPOND;
  while(1)
    {
      if (!get_recv_char(ch,&data,&status)) break;
      err |= status;
      cmd->data[j] = data;
      j++;      
      if (j >= sizeof(cmd->data))
        {
          err = DANFYSIK_ERR_BUFFULL;      /* Reply buffer full */
          j--;
          break;
        }
     }
  cmd->data[j] = 0;
  return (err);
}
/****************************************************************************
*  Varian Multigauge Controllers
****************************************************************************/
int varian(int ch,struct varian_ctl *cmd)
{
  int err,i,j;
  unsigned char data,status;

  if (!devtbl->vmic6015a) return (IG_ERR_NONEXIST);
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
*   Set for 4800 baud and enable baud rate generator.
*/
          vmic->tcreg = channel[i].tcreg = VMIC_4800;
          vmic->brgctl = channel[i].brgctl = 0xd;
          vmic->xmtctl = VMIC_8BITS;
          vmic->rcvctl = VMIC_8BITS;
          if (!(i % 2)) vmic->vectrg = VMIC_VECS + i * 4;
          vmic->intctl = channel[i].intctl = 0x17;
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
*   Set for 9600 baud and enable baud rate generator.
*/
          vmic->tcreg = channel[i].tcreg = VMIC_9600;
          vmic->brgctl = channel[i].brgctl = 0xd;
          vmic->xmtctl = VMIC_8BITS;
          vmic->rcvctl = VMIC_8BITS;
          if (!(i % 2)) vmic->vectrg = VMIC_VECS + i * 4;
          vmic->intctl = channel[i].intctl = 0x17;
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
   unsigned char status = 0,rdata,tdata;

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
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch2_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[2].vmic;

  channel[2].err |= (vmic->stat0 & 0x80);
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch3_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[3].vmic;

  channel[3].err |= (vmic->stat0 & 0x80);
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
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*  Clear the receive buffer.
*****************************************************************************/
void clr_recv_buf(struct recvbuf *bufptr)
{
   int i;

   bufptr->input = bufptr->output = &bufptr->buf[0];
   for (i=0; i < IN_BUF_SIZE; i++)
     {
       bufptr->buf[i].err = 0;
       bufptr->buf[i].data = 0;
     }
}
/*****************************************************************************
*   Clear the transmit buffer.
*****************************************************************************/
void clr_xmit_buf(struct xmitbuf *bufptr)
{
   bufptr->input = bufptr->output = bufptr->buf;
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
   int count;

   count = get_xmit_count(ptr);
   if (count < 0) return (0);
   *ptr->input++ = data;
   if ((ptr->input - ptr->buf) >= OUT_BUF_SIZE) ptr->input = ptr->buf;
   if (count == 0 && channel[chan].xidle == 0)
     {
       get_xmit_char(&channel[chan],&data);
       channel[chan].xidle = 0xff;
       channel[chan].vmic->datarg = data;
     }
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
