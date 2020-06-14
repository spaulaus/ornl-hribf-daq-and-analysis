/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2001
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
*    File:         /usr/users/mcsq/Dvme3/vmexx.c
*
*    Description:
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/08/01    MCSQ         
*
*   10/31/01    MCSQ        Set Common mode(start or stop) in caen_tdc_write
*                           routine.
*
*    7/14/06    MCSQ        Add routine for read and control of the SIS3820
*                           VME scaler module.
*****************************************************************************/
#include "devices.h"
#include "vme_sys.h"
#include "system.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan.h"
#include "lan_lib.h"
#include "caen.h"
#include "vmexx.h"
#include "sis3820.h"

#include <stdio.h>
#include <stdlib.h>

/*    Function Prototypes        */

char caen775_ctl(struct caen_ctl *);
char caen785_ctl(struct caen_ctl *);

char sis3820_ctl(struct sis_ctl  *);

/*      Global data              */

/*
*   Pointers to CAEN ADCs and TDCs
*/
struct CAEN *CAEN785_LIST[10] = {       /* V785 ADC             */
                                  (struct CAEN *)CAEN785_1,
                                  (struct CAEN *)CAEN785_2,
                                  (struct CAEN *)CAEN785_3,
                                  (struct CAEN *)CAEN785_4,
                                  (struct CAEN *)CAEN785_5,
                                  (struct CAEN *)CAEN785_6,
                                  (struct CAEN *)CAEN785_7,
                                  (struct CAEN *)CAEN785_8,
                                  (struct CAEN *)CAEN785_9,
                                  (struct CAEN *)CAEN785_10};

struct CAEN *CAEN775_LIST[10] = {       /* V775 TDC             */
                                  (struct CAEN *)CAEN775_1,
                                  (struct CAEN *)CAEN775_2,
                                  (struct CAEN *)CAEN775_3,
                                  (struct CAEN *)CAEN775_4,
                                  (struct CAEN *)CAEN775_5,
                                  (struct CAEN *)CAEN775_6,
                                  (struct CAEN *)CAEN775_7,
                                  (struct CAEN *)CAEN775_8,
                                  (struct CAEN *)CAEN775_9,
                                  (struct CAEN *)CAEN775_10};

struct SIS3820 * SIS3820_LIST[1] = {(struct SIS3820 *)SIS3820_1};

static struct devices *devtbl = DEVTBL;
                 char *dev785;
                 char *dev775;
                 char *dev3820;


/*****************************************************************************
*****************************************************************************/
main()
{
  int    size;
  char   *inbuf,*outbuf;
  struct Ether_Packet *out_hdr;
  union Cmd *cmd,*rply;

  dev785 = &devtbl->caen785_1;
  dev775 = &devtbl->caen775_1;
  dev3820 = &devtbl->sis3820_1;

  lan_open(PROTO_VMEIO,&outbuf,&out_hdr);
  task_priority_(-1,1,69);
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
     if (cmd->conv.func == CAEN775)
       {
         rply->conv.rpystat = caen775_ctl((struct caen_ctl *)rply);
         size = sizeof(struct caen_ctl);
       }
     else if (cmd->conv.func == CAEN785)
       {
         rply->conv.rpystat = caen785_ctl((struct caen_ctl *)rply);
         size = sizeof(struct caen_ctl);
       }
     else if (cmd->conv.func == SIS3820MOD)
       {
         rply->sca.rpystat = sis3820_ctl((struct sis_ctl *)rply);
         size = sizeof(struct sis_ctl);
       }
     else
       {
         size = sizeof(struct vmeio_stat);
         rply->conv.rpystat = VME_ERR_FUNC;
       }
     lan_reply(size,ACK);
   }
}
/****************************************************************************
****************************************************************************/
char caen775_ctl(struct caen_ctl *cmd)
{
   int i;
   short srange,smode;
   struct CAEN *caen;

   if (cmd->cvt_num < 0 || cmd->cvt_num > 9) return(CAEN_ERR_NONEXIST);
   if (!dev775[cmd->cvt_num]) return(CAEN_ERR_NONEXIST);
   caen =  CAEN775_LIST[cmd->cvt_num];
   byte_swap_((char *)cmd->data,sizeof(cmd->data));

   if (cmd->rw)
     {
       for(i=0; i < 32; i++) caen->thresholds[i] = cmd->data[i];
       srange = cmd->range;
       caen->range = srange;
       if (cmd->mode) caen->bit_set2 = 0x400;
       else  caen->bit_clear2 = 0x400;
     }
   else
     {
       for(i=0; i < 32; i++) cmd->data[i] = caen->thresholds[i] & 0x1ff;
       srange = caen->range;
       cmd->range = srange;
       smode = caen->bit_set2;
       if (smode & 0x400) cmd->mode = 1;
       else  cmd->mode = 0;
     }
   byte_swap_((char *)cmd->data,sizeof(cmd->data));
   return(0);
}
/****************************************************************************
****************************************************************************/
char caen785_ctl(struct caen_ctl *cmd)
{
   int i;
   struct CAEN *caen;

   if (cmd->cvt_num < 0 || cmd->cvt_num > 9) return(CAEN_ERR_NONEXIST);
   if (!dev785[cmd->cvt_num]) return(CAEN_ERR_NONEXIST);
   caen =  CAEN785_LIST[cmd->cvt_num];
   byte_swap_((char *)cmd->data,sizeof(cmd->data));

   if (cmd->rw)
     {
       for(i=0; i < 32; i++) caen->thresholds[i] = cmd->data[i];
     }
   else
     {
       for(i=0; i < 32; i++) cmd->data[i] = caen->thresholds[i] & 0x1ff;
     }

   byte_swap_((char *)cmd->data,sizeof(cmd->data));
   return(0);
}
/****************************************************************************
****************************************************************************/

char sis3820_ctl(struct sis_ctl *cmd)
{
   int i;
   unsigned long dat = 0;
   struct SIS3820 *sis;


   if (cmd->sca_num < 0 || cmd->sca_num > 0) return(CAEN_ERR_NONEXIST);
   if (!dev3820[cmd->sca_num]) return(CAEN_ERR_NONEXIST);
   sis =  SIS3820_LIST[cmd->sca_num];

   if (cmd->code == 0)        /* Read counters                               */
     {
       for (i=0; i < 32; i++) cmd->data[i] = sis->counter[i];
       byte_swap_((char *)cmd->data,sizeof(cmd->data));
       word_swap_((short *)cmd->data,sizeof(cmd->data)/2);
     }
   else if (cmd->code == 1)   /* Clear counters                              */
     {
       sis->Key_cnt_clear = 0;
     }
   else if (cmd->code == 2)    /* enable counting                            */
     {
       sis->op_mode = 1;
       sis->Key_enable = 0;
     }
   else if (cmd->code == 3)    /* disable counting                           */
     {
       sis->Key_disable = 0;
       sis->op_mode = 1;
     }
   return (0);
}
